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
                let (ch, consumed) = parse_unicode_escape(&input[i + 1..], span.trim_start(i + 1))?;
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
    if input.as_bytes()[0] == b'{' {
        // We are parsing a {}-delimited code point. (This is an extension of
        // what json allows, similar to Rust escape sequences.)
        let len = match input.find('}') {
            None => {
                let err = span.error("Unclosed '\\u{' escape sequence, expected '}'.");
                return Err(err);
            }
            Some(n) => n,
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
    } else {
        // We are parsing a json-style Unicode escape sequence with 4 hex digits.
        let err = span
            .take(4)
            .error("Expected four hex digits after '\\u' escape sequence.");
        if input.len() < 4 {
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
                            "For code points beyond U+FFFF, use '\\u{xxxxxx}' \
                            instead of a surrogate pair.",
                        );
                    Err(err)
                }
            },
        }
    }
}
