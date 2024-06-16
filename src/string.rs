// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Utilities for working with strings.

use crate::cst::StringPart;
use crate::error::{IntoError, Result};
use crate::lexer::Escape;
use crate::source::Span;

/// Convert an escape sequence into the string it represents.
pub fn unescape_into(input: &str, span: Span, escape: Escape, output: &mut String) -> Result<()> {
    let span_str = span.resolve(input);
    let span_bytes = span_str.as_bytes();

    match escape {
        Escape::Single => {
            debug_assert_eq!(&span_str[..1], "\\");
            debug_assert_eq!(span_bytes.len(), 2);
            unescape_single(span, span_bytes[1], output)
        }
        Escape::Unicode4 => {
            debug_assert_eq!(&span_str[..2], "\\u");
            debug_assert_eq!(span_bytes.len(), 6);
            unescape_unicode(span, &span_str[2..], output)
        }
        Escape::UnicodeDelim => {
            let n = span_str.len();
            debug_assert_eq!(&span_str[..3], "\\u{");
            debug_assert_eq!(&span_str[n - 1..], "}");
            unescape_unicode(span, &span_str[3..n - 1], output)
        }
    }
}

fn unescape_single(span: Span, ch: u8, output: &mut String) -> Result<()> {
    match ch {
        // The following escape sequences are identical to json.
        b'"' => output.push('"'),
        b'\\' => output.push('\\'),
        b'/' => output.push('/'),
        b'b' => output.push('\x08'),
        b'f' => output.push('\x0c'),
        b'n' => output.push('\n'),
        b'r' => output.push('\r'),
        b't' => output.push('\t'),

        // This one is not part of json. We add it to escape { in f-strings,
        // but for consistency it can be used in any string literal.
        b'{' => output.push('{'),

        // } never needs to be escaped, but for symmetry with { we allow it.
        b'}' => output.push('}'),

        ch if ch.is_ascii_uppercase() => {
            return span
                .error("Invalid escape sequence.")
                .with_help("Escape sequences are written lowercase.")
                .err()
        }
        _ => return span.error("Invalid escape sequence.").err(),
    }
    Ok(())
}

fn unescape_unicode(span: Span, hex: &str, output: &mut String) -> Result<()> {
    if hex.is_empty() {
        return span
            .error("Unicode escape sequence is empty, expected at least one hex digit.")
            .err();
    }
    if hex.len() > 6 {
        return span
            .error("Unicode escape sequence too long, expected at most 6 hex digits.")
            .err();
    }
    let u = u32::from_str_radix(hex, 16).expect("The lexer only admits hex digit bytes.");
    match char::from_u32(u) {
        Some(ch) => {
            output.push(ch);
            Ok(())
        }
        None => span
            .error("Invalid escape sequence: not a Unicode scalar value.")
            .with_help("For code points beyond U+FFFF, use '\\u{...}' instead of a surrogate pair.")
            .err(),
    }
}

/// Return whether the string is a valid RCL identifier.
pub fn is_identifier(s: &str) -> bool {
    let bytes = s.as_bytes();

    if bytes.is_empty() {
        return false;
    }

    if !bytes[0].is_ascii_alphabetic() && bytes[0] != b'_' {
        return false;
    }

    for &b in bytes.iter().skip(1) {
        if !b.is_ascii_alphanumeric() && b != b'_' && b != b'-' {
            return false;
        }
    }

    // If it satisfied all the above, then the only reason not to be an
    // identifier is if there is a keyword with that name.
    // TODO: Add golden test to ensure that keywords are escaped in RCL output.
    // The absence of control flow may not make it obvious from the coverage
    // report to test this case.
    !crate::lexer::is_keyword(s)
}

/// Escape a string for use inside a json string literal.
pub fn escape_json(str: &str, into: &mut String) {
    use std::fmt::Write;

    into.reserve(str.len());

    for ch in str.chars() {
        match ch {
            '\n' => into.push_str(r#"\n"#),
            '\r' => into.push_str(r#"\r"#),
            '\x08' => into.push_str(r#"\b"#),
            '\x0c' => into.push_str(r#"\f"#),
            '\t' => into.push_str(r#"\t"#),
            '\"' => into.push_str(r#"\""#),
            '\\' => into.push_str(r#"\\"#),
            ch if ch.is_ascii_control() => write!(into, "\\u{:04x}", ch as u32)
                .expect("Writing into &mut String does not fail."),
            ch => into.push(ch),
        }
    }
}

pub fn count_common_leading_spaces(input: &str, parts: &[StringPart]) -> usize {
    let mut n_spaces = None;

    // The lexer already breaks strings up into lines, so a part contains at
    // most one newline. But a line may contain multiple parts, in the case of
    // holes.
    for (i, part) in parts.iter().enumerate() {
        let line = match part {
            StringPart::String(span) => span.resolve(input),
            _ => continue,
        };
        if !line.starts_with('\n') {
            continue;
        }
        let n = line
            .as_bytes()
            .iter()
            .skip(1)
            .take_while(|ch| **ch == b' ')
            .count();

        // There may be a blank line in a multiline string literal that contains
        // no spaces even though the other lines do have spaces, to avoid
        // trailing whitespace. In that case we should not set the indent to
        // zero, even though technically it is. We also consider lines that have
        // only spaces to be blank lines.
        let has_next_line = matches!(parts.get(i + 1), Some(StringPart::String(..)));
        let is_blank_line = n + 1 == line.len() && has_next_line;

        if !is_blank_line {
            n_spaces = match n_spaces {
                None => Some(n),
                Some(m) => Some(m.min(n)),
            };
        }
    }

    n_spaces.unwrap_or(0)
}

// Note, most testing is done through golden tests and fuzzing, not unit tests.
#[cfg(test)]
mod test {
    use crate::cst::StringPart;
    use crate::error::Result;
    use crate::source::DocId;

    fn parse_string_raw(input: &str) -> Vec<StringPart> {
        use crate::cst::Expr::StringLit;
        let doc = DocId(0);
        let tokens = crate::lexer::lex(doc, input).unwrap();
        let (_span, expr) = crate::parser::parse(doc, input, &tokens).unwrap();
        match expr {
            StringLit { parts, .. } => parts,
            _ => panic!("Should have parsed a string."),
        }
    }

    fn parse_string(input: &str) -> Vec<&str> {
        let mut result = Vec::new();
        for part in parse_string_raw(input) {
            let span = match part {
                StringPart::String(span) => span,
                _ => unreachable!("Tests contain no holes or escapes."),
            };
            result.push(span.resolve(input));
        }
        result
    }

    fn count_common_leading_spaces(input: &str) -> usize {
        let quoted_input = format!("\"\"\"{input}\"\"\"");
        let parts = parse_string_raw(&quoted_input);
        super::count_common_leading_spaces(&quoted_input, &parts)
    }

    fn unescape(input: &str) -> Result<String> {
        use crate::ast::Expr::StringLit;
        let doc = DocId(0);
        let tokens = crate::lexer::lex(doc, input)?;
        let (_span, expr) = crate::parser::parse(doc, input, &tokens)?;
        let ast = crate::abstraction::abstract_expr(input, &expr)?;
        match ast {
            StringLit(s) => Ok(s.to_string()),
            bad => panic!("Expected only strings, got {bad:?}."),
        }
    }

    fn escape_json(str: &str) -> String {
        let mut out = String::new();
        super::escape_json(str, &mut out);
        out
    }

    #[test]
    fn unescape_handles_json_escape_sequences() {
        assert_eq!(unescape(r#""abc""#).unwrap(), "abc");
        assert_eq!(unescape(r#""\"\\\/\b""#).unwrap(), "\"\\/\x08");
        assert_eq!(unescape(r#""\f\r\n\t""#).unwrap(), "\x0c\r\n\t");
        assert_eq!(unescape(r#""a\u0008c""#).unwrap(), "a\x08c");
        assert_eq!(unescape(r#""a\u{0008}\u{8}c""#).unwrap(), "a\x08\x08c");
    }

    #[test]
    fn unescape_does_not_crash_on_early_end() {
        // This is a regression test.
        assert!(unescape(r#""\u""#).is_err());
    }

    #[test]
    fn unescape_does_not_slice_code_points() {
        // This is a regression test.
        assert!(unescape(r#""\u000ö""#).is_err());
    }

    #[test]
    fn unescape_handles_escaped_backslash_at_end() {
        assert_eq!(unescape(r#""\\""#).unwrap(), "\\");
    }

    // Note, the main tests for unescaping are the golden tests.

    #[test]
    fn escape_json_handles_predefined_escapes() {
        assert_eq!(escape_json("\"\\/\x08\x0c\n\r\t"), r#"\"\\/\b\f\n\r\t"#,)
    }

    #[test]
    fn escape_json_handles_ascii_control() {
        assert_eq!(
            escape_json("\x00\x01\x02\x03\x7f"),
            r#"\u0000\u0001\u0002\u0003\u007f"#,
        )
    }

    // Note, the main test for json escaping is the `escapes` fuzzer.

    #[test]
    fn parsing_triple_string_works() {
        let s = r#""""
        Line 1
          Line 2
        Line 3
        """"#;
        assert_eq!(unescape(s).unwrap(), "Line 1\n  Line 2\nLine 3\n");

        let s = r#""""
            Line 1
          Line 2
        Line 3""""#;
        assert_eq!(unescape(s).unwrap(), "    Line 1\n  Line 2\nLine 3");
    }

    #[test]
    fn parse_string_does_not_slice_code_points() {
        let s = "\"\"\"\n\u{1f574}\u{fe0e}\n    \"\"\"";
        let parts = parse_string(s);
        assert_eq!(parts, ["\n\u{1f574}\u{fe0e}", "\n    "]);
    }

    #[test]
    fn count_common_leading_spaces_handles_blank_lines() {
        assert_eq!(count_common_leading_spaces("\n  X\n  Y"), 2);
        assert_eq!(count_common_leading_spaces("\n  X\n    Y"), 2);
        assert_eq!(count_common_leading_spaces("\n  X\n    Y\n "), 1);
        assert_eq!(
            // Despite the zero-length blank line, the indent is 2.
            count_common_leading_spaces("\n  X\n\n  Y"),
            2
        );
        assert_eq!(
            // Also if the blank line is longer than the others.
            count_common_leading_spaces("\n  X\n    \n  Y"),
            2
        );
        assert_eq!(
            // Even if there are only blank lines, we should report the count.
            count_common_leading_spaces("\n  \n  "),
            2
        );
    }
}
