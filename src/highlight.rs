// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! A syntax highlighter that acts on the token stream.

use crate::lexer::{Lexeme, Token};
use crate::markup::{Markup, MarkupString};

fn get_markup(token: &Token) -> Markup {
    match token {
        Token::LineComment => Markup::Comment,
        Token::NumBinary | Token::NumHexadecimal | Token::NumDecimal => Markup::Number,
        Token::QuoteOpen(..) | Token::QuoteClose | Token::StringInner => Markup::String,
        Token::HoleOpen | Token::HoleClose | Token::Escape(..) => Markup::Escape,
        Token::Ident => Markup::Field,

        Token::KwAnd
        | Token::KwAssert
        | Token::KwElse
        | Token::KwFalse
        | Token::KwFor
        | Token::KwIf
        | Token::KwImport
        | Token::KwIn
        | Token::KwLet
        | Token::KwNot
        | Token::KwNull
        | Token::KwOr
        | Token::KwTrace
        | Token::KwTrue => Markup::Keyword,

        _ => Markup::None,
    }
}

pub fn highlight<'a>(tokens: &[Lexeme], input: &'a str) -> MarkupString<'a> {
    let mut out = MarkupString::new();
    let mut end = 0;

    for (token, span) in tokens {
        // Insignificant space is not represented explicitly as a token, we can
        // infer it from a gap in the byte range.
        if span.start() > end {
            out.push(&input[end..span.start()], Markup::None);
        }

        let markup = get_markup(token);
        let string = &input[span.start()..span.end()];
        out.push(string, markup);

        end = span.end();
    }

    // Add a trailing newline, which is lost during the parsing if there is any.
    out.push("\n", Markup::None);

    out
}
