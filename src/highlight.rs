// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! A syntax highlighter that acts on the token stream.

use std::io::{Result, Write};

use crate::lexer::{Lexeme, Token};

fn get_color(token: &Token) -> &'static str {
    let red = "\x1b[31m";
    let green = "\x1b[32m";
    let blue = "\x1b[34m";
    let cyan = "\x1b[36m";
    let white = "\x1b[37m";
    let reset = "\x1b[0m";

    match token {
        Token::Space | Token::Blank => reset,
        Token::LineComment => white,
        Token::Quoted(..) => red,
        Token::FormatOpen(..) | Token::FormatInner(..) | Token::FormatClose(..) => red,
        Token::NumBinary | Token::NumHexadecimal | Token::NumDecimal => cyan,
        Token::Ident => reset,
        Token::KwAnd
        | Token::KwElse
        | Token::KwFalse
        | Token::KwFor
        | Token::KwIf
        | Token::KwIn
        | Token::KwLet
        | Token::KwNot
        | Token::KwOr
        | Token::KwThen
        | Token::KwTrue => green,
        _ => blue,
    }
}

pub fn highlight(out: &mut dyn Write, tokens: &[Lexeme], input: &str) -> Result<()> {
    let input = input.as_bytes();
    let mut end = 0;
    let mut color = "";

    for (token, span) in tokens {
        let new_color = get_color(token);
        if new_color != color {
            out.write_all(new_color.as_bytes())?;
            color = new_color;
        }

        // Insignificant space is not represented explicitly as a token, we can
        // infer it from a gap in the byte range.
        if span.start() > end {
            out.write_all(&input[end..span.start()])?;
        }

        out.write_all(&input[span.start()..span.end()])?;
        end = span.end();
    }

    out.write_all(&input[end..])?;

    Ok(())
}
