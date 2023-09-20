// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Utilities for working with strings.

use crate::cst::{Expr, FormatHole};
use crate::error::{IntoParseError, ParseError};
use crate::source::Span;

pub type Result<T> = std::result::Result<T, ParseError>;

/// Convert a string literal into the string it represents.
///
/// Expects the input span to not contain the surrounding quotes of the string
/// literal.
pub fn unescape(input: &str, span: Span) -> Result<String> {
    let mut output = String::with_capacity(span.len());
    unescape_into(input, span, &mut output)?;
    Ok(output)
}

/// See [`unescape`].
pub fn unescape_into(input: &str, span: Span, output: &mut String) -> Result<()> {
    let mut input = span.resolve(input);
    let mut offset = 0;

    while let Some(i) = input.find('\\') {
        offset += i;
        output.push_str(&input[..i]);
        // This index does not go out of bounds, because a (first) \ cannot
        // occur in a string literal right before the end of the literal,
        // because then it would escape the end, so that would be a bug in the
        // lexer.
        debug_assert!(i + 1 < input.len(), "Cannot have \\ before string end.");
        let next = input.as_bytes()[i + 1];
        match next {
            // The following escape sequences are identical to json.
            b'"' => output.push('"'),
            b'\\' => output.push('\\'),
            b'/' => output.push('/'),
            b'b' => output.push('\x08'),
            b'f' => output.push('\x0c'),
            b'n' => output.push('\n'),
            b'r' => output.push('\r'),
            b't' => output.push('\t'),
            b'u' => {
                let (ch, consumed) = parse_unicode_escape(&input[i + 2..], span.trim_start(i + 2))?;
                output.push(ch);
                input = &input[i + consumed + 2..];
                offset += consumed + 2;
                continue;
            }

            // This one is not part of json. We add it to escape { in f-strings,
            // but for consistency it can be used in any string literal.
            b'{' => output.push('{'),

            // The other cases are errors.
            b'\n' | b'\r' => {
                let err = span
                    .trim_start(offset)
                    .take(1)
                    .error("Invalid escape sequence.")
                    .with_help(
                        "To break a long string across lines, break it into \
                        multiple strings and concatenate them with '+'.",
                    );
                return Err(err);
            }
            ch if ch.is_ascii_uppercase() => {
                let err = span
                    .trim_start(offset)
                    .take(2)
                    .error("Invalid escape sequence.")
                    .with_help("Escape sequences are written lowercase.");
                return Err(err);
            }
            _ => {
                let err = span
                    .trim_start(offset)
                    .take(2)
                    .error("Invalid escape sequence.");
                return Err(err);
            }
        }
        offset += 2;
        input = &input[i + 2..];
    }

    output.push_str(input);

    Ok(())
}

/// Parse a unicode escape sequence starting with `\u`.
///
/// The `input` should contain the remaining input after the `\u`. The `span`
/// should cover the same.
///
/// Returns the parsed code point and the number of input bytes consumed,
/// which does not include the two bytes for `\u`.
fn parse_unicode_escape(input: &str, span: Span) -> Result<(char, usize)> {
    match input.as_bytes().first() {
        Some(b'[') => {
            // We are parsing a []-delimited code point. (This is an extension of
            // what json allows, similar to Rust escape sequences.)
            let len = match input.find(']') {
                None => {
                    let err = span.error("Unclosed '\\u[' escape sequence, expected ']'.");
                    return Err(err);
                }
                Some(n) => n + 1,
            };
            if len > 8 {
                let err = span
                    .take(len)
                    .error("Escape sequence too long, expected at most 6 hex digits.");
                return Err(err);
            }
            match u32::from_str_radix(&input[1..len - 1], 16) {
                Err(..) => {
                    let err = span
                        .trim_start(1)
                        .take(len - 2)
                        .error("Expected hex digits between '[]' in '\\u' escape sequence.");
                    Err(err)
                }
                Ok(u) => match char::from_u32(u) {
                    Some(ch) => Ok((ch, len)),
                    None => {
                        let err = span
                            .trim_start(1)
                            .take(len - 2)
                            .error("Invalid escape sequence: not a Unicode scalar value.");
                        Err(err)
                    }
                },
            }
        }
        Some(ch) if ch.is_ascii_hexdigit() => {
            // We are parsing a json-style Unicode escape sequence with 4 hex
            // digits. We expect the next 4 characters to be the next 4 bytes,
            // but it might be a multi-byte code point, so we need to measure
            // properly.
            let n_bytes = input
                .char_indices()
                .skip(4)
                .map(|p| p.0)
                .next()
                .unwrap_or(input.len());
            let err = span
                .take(n_bytes)
                .error("Expected four hex digits after '\\u' escape sequence.");
            if n_bytes != 4 {
                return Err(err);
            }
            match u32::from_str_radix(&input[..4], 16) {
                Err(..) => Err(err),
                Ok(u) => match char::from_u32(u) {
                    Some(ch) => Ok((ch, 4)),
                    // TODO: For strict json compatibility, we would have to allow
                    // surrogate pairs, which means there needs to be another \u
                    // escape after this one. But let's not complicate things right
                    // now.
                    None => {
                        assert!(
                            u >= 0xd800 && u <= 0xdfff,
                            "char::from_u32 on 2-byte input only fails on surrogate code points."
                        );
                        let err = span
                            .take(4)
                            .error("Invalid escape sequence: not a Unicode scalar value.")
                            .with_help(
                                "For code points beyond U+FFFF, use \
                                '\\u[xxxxxx]' instead of a surrogate pair.",
                            );
                        Err(err)
                    }
                },
            }
        }
        _ => {
            let err = span
                .take(1)
                .error("Invalid '\\u' escape sequence.")
                .with_help(
                    "The code point must be exactly four hex digits, \
                    or enclosed in '[]'. For example '\\u000a' or '\\u[0a]'.",
                );
            Err(err)
        }
    }
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

/// Count common leading spaces.
///
/// For every line in the input, counts the number of spaces that follow it,
/// then take the minimum.
pub fn count_common_leading_spaces(input: &str, current_min: Option<usize>) -> Option<usize> {
    let mut input = input;
    let mut n_spaces = current_min;
    while let Some(i) = input.find('\n') {
        let n = input.as_bytes()[i + 1..]
            .iter()
            .take_while(|ch| **ch == b' ')
            .count();

        // There may be a blank line in a multiline string literal that contains
        // no spaces even though the other lines do have spaces, to avoid
        // trailing whitespace. In that case we should not set the indent to
        // zero, even though technically it is. We also consider lines that have
        // only spaces to be blank lines.
        let is_blank_line = input.len() > i + n + 1 && input.as_bytes()[i + n + 1] == b'\n';

        if !is_blank_line {
            n_spaces = match n_spaces {
                None => Some(n),
                Some(m) => Some(m.min(n)),
            };
        }
        input = &input[i + 1 + n..];
    }
    n_spaces
}

/// Execute callbacks for all spans and holes of the `f"`-quoted format string.
///
/// The callbacks receive the inner spans, with surrounding quotes removed.
pub fn fold_double_format_string<E, T, F, G>(
    begin: Span,
    holes: &[FormatHole],
    seed: T,
    mut on_span: F,
    mut on_hole: G,
) -> std::result::Result<T, E>
where
    F: FnMut(T, Span) -> std::result::Result<T, E>,
    G: FnMut(T, Span, &Expr) -> std::result::Result<T, E>,
{
    let mut acc = seed;
    // Cut off the `f"` and `"`.
    let begin_inner = begin.trim_start(2).trim_end(1);
    acc = on_span(acc, begin_inner)?;

    for hole in holes.iter() {
        let suffix_inner = hole.suffix.trim_start(1).trim_end(1);
        acc = on_hole(acc, hole.span, &hole.body)?;
        acc = on_span(acc, suffix_inner)?;
    }

    Ok(acc)
}

/// Call `on_line` for every line in the input string, with leading indent stripped.
fn fold_triple_string_lines_impl<E, T, F>(
    input: &str,
    span_inner: Span,
    n_indent: usize,
    seed: T,
    mut on_line: F,
) -> std::result::Result<T, E>
where
    F: FnMut(T, Span) -> std::result::Result<T, E>,
{
    debug_assert!(
        span_inner.len() > 0 && input.as_bytes()[span_inner.start()] == b'\n',
        "The parser must ensure that triple-quoted strings start with a newline.",
    );

    let mut span = span_inner.trim_start(1);
    let mut acc = seed;

    while span.len() > 0 {
        let line_end = span
            .resolve(input)
            .find('\n')
            .map(|i| i + 1)
            .unwrap_or(span.len());
        let line_span = span.take(line_end);
        let line = line_span.resolve(input).as_bytes();
        let has_newline = !line.is_empty() && line[line.len() - 1] == b'\n';
        let content_len = if has_newline {
            line.len() - 1
        } else {
            line.len()
        };
        let n_trim = n_indent.min(content_len);
        acc = on_line(acc, line_span.trim_start(n_trim))?;
        span = span.trim_start(line_end);
    }

    Ok(acc)
}

/// Execute callbacks for all lines of the string.
///
/// The span should _not_ include the opening and closing `"""`.
/// The returned lines include a trailing newline, if applicable.
pub fn fold_triple_string_lines<E, T, F>(
    input: &str,
    span_inner: Span,
    seed: T,
    on_line: F,
) -> std::result::Result<T, E>
where
    F: FnMut(T, Span) -> std::result::Result<T, E>,
{
    let n_indent = count_common_leading_spaces(span_inner.resolve(input), None);
    let n_indent = n_indent.unwrap_or(0);
    fold_triple_string_lines_impl(input, span_inner, n_indent, seed, on_line)
}

/// Execute callbacks for all lines and holes of the format string.
///
/// The begin span should include `f"""`, the hole spans should include the
/// `}` and `{`, and the final suffix should include the `"""`.
///
/// The returned lines include a trailing newline, if applicable.
pub fn fold_triple_format_string_lines<E, T, F, G>(
    input: &str,
    begin: Span,
    holes: &[FormatHole],
    seed: T,
    mut on_line: F,
    mut on_hole: G,
) -> std::result::Result<T, E>
where
    F: FnMut(T, Span) -> std::result::Result<T, E>,
    G: FnMut(T, Span, &Expr) -> std::result::Result<T, E>,
{
    let mut acc = seed;
    let span = begin.trim_start(4).trim_end(1);
    let mut n_indent = count_common_leading_spaces(span.resolve(input), None);

    for (i, hole) in holes.iter().enumerate() {
        let is_last = i + 1 == holes.len();
        let end_len = if is_last { 3 } else { 1 };
        let suffix_inner = hole.suffix.trim_start(1).trim_end(end_len);
        n_indent = count_common_leading_spaces(suffix_inner.resolve(input), n_indent);
    }

    let n_indent = n_indent.unwrap_or(0);

    acc = fold_triple_string_lines_impl(input, span, n_indent, acc, &mut on_line)?;

    for (i, hole) in holes.iter().enumerate() {
        acc = on_hole(acc, hole.span, &hole.body)?;

        let is_last = i + 1 == holes.len();
        let end_len = if is_last { 3 } else { 1 };
        let mut span = hole.suffix.trim_start(1).trim_end(end_len);

        // After a hole we have to run the line callback without stripping
        // any indent, because we are not at the start of a line.
        if let Some(line_end) = span.resolve(input).find('\n') {
            acc = on_line(acc, span.take(line_end + 1))?;
            span = span.trim_start(line_end);
            // Then after the first newline, we can continue calling it with the
            // indent stripped.
            acc = fold_triple_string_lines_impl(input, span, n_indent, acc, &mut on_line)?;
        } else {
            acc = on_line(acc, span)?;
        }
    }

    Ok(acc)
}

// Note, most testing is done through golden tests and fuzzing, not unit tests.
#[cfg(test)]
mod test {
    use crate::source::{DocId, Span};

    fn unescape(inner: &str) -> super::Result<String> {
        let span = Span::new(DocId(0), 0, inner.len());
        super::unescape(inner, span)
    }

    fn escape_json(str: &str) -> String {
        let mut out = String::new();
        super::escape_json(str, &mut out);
        out
    }

    #[test]
    fn unescape_handles_json_escape_sequences() {
        assert_eq!(unescape(r"abc").unwrap(), "abc");
        assert_eq!(unescape(r#"\"\\\/\b"#).unwrap(), "\"\\/\x08");
        assert_eq!(unescape(r#"\f\r\n\t"#).unwrap(), "\x0c\r\n\t");
        assert_eq!(unescape(r#"a\u0008c"#).unwrap(), "a\x08c");
        assert_eq!(unescape(r#"a\u[0008]\u[8]c"#).unwrap(), "a\x08\x08c");
    }

    #[test]
    fn unescape_does_not_crash_on_early_end() {
        // This is a regression test.
        assert!(unescape(r"\u").is_err());
    }

    #[test]
    fn unescape_does_not_slice_code_points() {
        // This is a regression test.
        assert!(unescape(r"\u000ö").is_err());
    }

    #[test]
    fn unescape_handles_escaped_backslash_at_end() {
        assert_eq!(unescape(r"\\").unwrap(), "\\");
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
    fn fold_triple_string_lines_works() {
        let s = r#"
        Line 1
          Line 2
        Line 3
        "#;
        let span = Span::new(DocId(0), 0, s.len());
        let lines = super::fold_triple_string_lines(s, span, Vec::new(), |mut acc, line| {
            acc.push(line.resolve(s));
            Ok::<_, ()>(acc)
        })
        .unwrap();
        assert_eq!(lines, ["Line 1\n", "  Line 2\n", "Line 3\n", ""]);

        let s = r#"
            Line 1
          Line 2
        Line 3"#;
        let span = Span::new(DocId(0), 0, s.len());
        let lines = super::fold_triple_string_lines(s, span, Vec::new(), |mut acc, line| {
            acc.push(line.resolve(s));
            Ok::<_, ()>(acc)
        })
        .unwrap();
        assert_eq!(lines, ["    Line 1\n", "  Line 2\n", "Line 3"]);
    }

    #[test]
    fn fold_triple_string_lines_does_not_slice_code_points() {
        let s = "\n\u{1f574}\u{fe0e}\n    ";
        let span = Span::new(DocId(0), 0, s.len());
        let lines = super::fold_triple_string_lines(s, span, Vec::new(), |mut acc, line| {
            acc.push(line.resolve(s));
            Ok::<_, ()>(acc)
        })
        .unwrap();
        assert_eq!(lines, ["\u{1f574}\u{fe0e}\n", "    "]);
    }

    #[test]
    fn count_common_leading_spaces_handles_blank_lines() {
        assert_eq!(
            super::count_common_leading_spaces("\n  X\n  Y", None),
            Some(2)
        );
        assert_eq!(
            super::count_common_leading_spaces("\n  X\n    Y", None),
            Some(2)
        );
        assert_eq!(
            super::count_common_leading_spaces("\n  X\n    Y\n ", None),
            Some(1)
        );
        assert_eq!(
            // Despite the zero-length blank line, the indent is 2.
            super::count_common_leading_spaces("\n  X\n\n  Y", None),
            Some(2)
        );
        assert_eq!(
            // Also if the blank line is longer than the others.
            super::count_common_leading_spaces("\n  X\n    \n  Y", None),
            Some(2)
        );
        assert_eq!(
            // Even if there are only blank lines, we should report the count.
            super::count_common_leading_spaces("\n  \n  ", None),
            Some(2)
        );
    }
}
