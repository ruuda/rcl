// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! A syntax highlighter that acts on the token stream.

use std::io::{Result, Write};

use crate::lexer::{Lexeme, Token};

fn get_color(token: &Token, token_bytes: &[u8]) -> &'static str {
    let red = "\x1b[31m";
    let green = "\x1b[32m";
    let blue = "\x1b[34m";
    let cyan = "\x1b[36m";
    let white = "\x1b[37m";
    let magenta = "\x1b[35m";
    let reset = "\x1b[0m";

    match token {
        Token::Space | Token::Blank => reset,
        Token::LineComment => white,
        Token::NumBinary | Token::NumHexadecimal | Token::NumDecimal => cyan,
        Token::Ident => match token_bytes {
            // Give the builtins a different color.
            // TODO: Only when preceded by a dot, when they are methods.
            b"contains" | b"except" | b"fold" | b"get" | b"group_by" | b"key_by" | b"keys"
            | b"len" | b"parse_int" | b"reverse" | b"split" | b"split_lines" | b"std"
            | b"values" => red,
            _ => blue,
        },

        Token::QuoteOpen(..) | Token::QuoteClose | Token::StringInner => cyan,

        Token::HoleOpen | Token::HoleClose => red,

        Token::Escape(..) => magenta,

        Token::KwAnd
        | Token::KwAssert
        | Token::KwElse
        | Token::KwFalse
        | Token::KwFor
        | Token::KwIf
        | Token::KwIn
        | Token::KwLet
        | Token::KwNot
        | Token::KwNull
        | Token::KwOr
        | Token::KwThen
        | Token::KwTrace
        | Token::KwTrue => green,

        _ => reset,
    }
}

pub fn highlight(out: &mut dyn Write, tokens: &[Lexeme], input: &str) -> Result<()> {
    let input = input.as_bytes();
    let mut end = 0;
    let mut color = "";

    for (token, span) in tokens {
        let token_bytes = &input[span.start()..span.end()];
        let new_color = get_color(token, token_bytes);
        if new_color != color {
            out.write_all(new_color.as_bytes())?;
            color = new_color;
        }

        // Insignificant space is not represented explicitly as a token, we can
        // infer it from a gap in the byte range.
        if span.start() > end {
            out.write_all(&input[end..span.start()])?;
        }

        out.write_all(token_bytes)?;
        end = span.end();
    }

    out.write_all(&input[end..])?;

    Ok(())
}
