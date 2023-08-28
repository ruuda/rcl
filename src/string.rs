// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Utilities for working with strings.

use crate::error::{IntoParseError, ParseError};
use crate::source::Span;

pub type Result<T> = std::result::Result<T, ParseError>;

/// Convert a string literal into the string it represents.
///
/// Expects the input span to not contain the surrounding quotes of the string
/// literal.
pub fn unescape(input: &str, span: Span) -> Result<String> {
    let mut output = String::with_capacity(span.len());
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
                offset += i + consumed + 2;
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

    Ok(output)
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
                    let err = span.error("Unclosed '\\u[' escape sequence, expected '}'.");
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
                        .error("Expected hex digits between '{}' in '\\u' escape sequence.");
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
                                '\\u{xxxxxx}' instead of a surrogate pair.",
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

// Note, most testing is done through golden tests, not unit tests.
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
}
