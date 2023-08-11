// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use crate::error::ParseError;
use crate::source::{DocId, Span};

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Token {
    /// A sequence of ascii whitespace not significant for reformatting.
    ///
    /// Note, this token gets discarded from the result, it is there for
    /// internal use in the lexer on only.
    Space,

    /// A sequence of ascii whitespace that contains at least two newlines.
    Blank,

    /// A comment that starts with `//` and runs until the end of the line.
    ///
    /// Excludes the newline itself.
    LineComment,

    /// A sequence of ascii alphanumeric or _, not starting with a digit.
    Ident,

    /// A string enclosed in double `"`.
    DoubleQuoted,

    /// A string enclosed in triple double quotes `"""`.
    TripleQuoted,

    /// `false`
    KwFalse,

    /// `for`
    KwFor,

    /// `if`
    KwIf,

    /// `in`
    KwIn,

    /// `let`
    KwLet,

    /// `not`
    KwNot,

    /// `true`
    KwTrue,

    /// `(`
    LParen,

    /// `)`
    RParen,

    /// `[`
    LBracket,

    /// `]`
    RBracket,

    /// `{`
    LBrace,

    /// `}`
    RBrace,

    /// `!`
    Bang,

    /// `,`
    Comma,

    /// `.`
    Dot,

    /// `:`
    Colon,

    /// `;`
    Semicolon,

    /// `=`
    Eq,

    /// `|`
    Pipe,

    /// `+`
    Plus,
}

pub type Lexeme = (Token, Span);

/// Lex an input document into tokens.
pub fn lex(doc: DocId, input: &str) -> Result<Vec<Lexeme>> {
    let mut tokens = Vec::new();
    let mut lexer = Lexer::new(doc, input);
    while lexer.start < lexer.input.len() {
        match lexer.next()? {
            // We drop non-significant whitespace in the lexer to simplify the
            // parser. Blank lines we do keep, because we want to preserve them
            // when autoformatting.
            (Token::Space, _) => continue,
            lexeme => tokens.push(lexeme),
        };
    }
    Ok(tokens)
}

struct Lexer<'a> {
    input: &'a str,
    doc: DocId,
    start: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(doc: DocId, input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            doc,
            start: 0,
        }
    }

    /// Return a span from the current cursor location and advance the cursor.
    fn span(&mut self, len: usize) -> Span {
        // We could turn this into a proper error and report it, but it would
        // make things really tedious. It's one of those things where if you
        // don't fix it a fuzzer will force you to, except in this case we need
        // an input larger than 4 GiB before it happens, so we can get away with
        // a panic.
        assert!(
            len <= u32::MAX as usize,
            "The lexer does not support spans longer than 4 GiB. Byte offset: {}",
            self.start,
        );
        let start = self.start;
        self.start += len;
        Span {
            start,
            len: len as u32,
            doc: self.doc,
        }
    }

    /// Build a parse error at the current cursor location.
    fn error_while<F: FnMut(u8) -> bool, T>(
        &mut self,
        include: F,
        message: &'static str,
    ) -> Result<T> {
        let error = ParseError {
            span: self.take_while(include),
            message,
            note: None,
            help: None,
        };
        Err(error)
    }

    /// Count until `include` returns false.
    fn take_while<F: FnMut(u8) -> bool>(&mut self, mut include: F) -> Span {
        let input = &self.input.as_bytes()[self.start..];
        self.span(input.iter().take_while(|ch| include(**ch)).count())
    }

    /// Lex one token. The input must not be empty.
    fn next(&mut self) -> Result<Lexeme> {
        let input = &self.input.as_bytes()[self.start..];

        debug_assert!(
            !input.is_empty(),
            "Must have input before continuing to lex."
        );

        if input.starts_with(b"//") {
            return Ok(self.lex_in_line_comment());
        }

        if input.starts_with(b"\"\"\"") {
            return self.lex_in_triple_quote();
        }

        if input[0] == b'"' {
            return self.lex_in_double_quote();
        }

        if input[0] == b'_' {
            return Ok(self.lex_in_ident());
        }

        if input[0] == b'_' || input[0].is_ascii_alphabetic() {
            return Ok(self.lex_in_ident());
        }

        if input[0].is_ascii_whitespace() {
            return Ok(self.lex_in_space());
        }

        if input[0].is_ascii_punctuation() {
            return self.lex_in_punct();
        }

        if input[0].is_ascii_control() {
            return self.error_while(
                |ch| ch.is_ascii_control(),
                "Control characters are not supported here.",
            );
        }

        if input[0] > 127 {
            // Multi-byte sequences of non-ascii code points are fine in
            // strings and comments, but not in the source code where we
            // expect identifiers.
            return self.error_while(
                |ch| ch > 127,
                "Non-ascii characters are not supported here.",
            );
        }

        unreachable!(
            "We should have handled all bytes, but we forgot {} (0x{:02x})",
            char::from_u32(input[0] as u32).unwrap(),
            input[0],
        );
    }

    fn lex_in_space(&mut self) -> Lexeme {
        let span = self.take_while(|ch| ch.is_ascii_whitespace());
        let is_blank = span
            .resolve(self.input)
            .as_bytes()
            .iter()
            .filter(|ch| **ch == b'\n')
            .nth(1)
            .is_some();
        if is_blank {
            (Token::Blank, span)
        } else {
            (Token::Space, span)
        }
    }

    fn lex_in_ident(&mut self) -> Lexeme {
        let span =
            self.take_while(|ch| false || ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'-');
        let ident = span.resolve(self.input);
        let token = match ident {
            "false" => Token::KwFalse,
            "for" => Token::KwFor,
            "if" => Token::KwIf,
            "in" => Token::KwIn,
            "let" => Token::KwLet,
            "not" => Token::KwNot,
            "true" => Token::KwTrue,
            _ => Token::Ident,
        };
        (token, span)
    }

    fn lex_in_line_comment(&mut self) -> Lexeme {
        (Token::LineComment, self.take_while(|ch| ch != b'\n'))
    }

    fn lex_in_double_quote(&mut self) -> Result<Lexeme> {
        let input = &self.input.as_bytes()[self.start..];

        // Skip over the initial opening quote.
        for (i, &ch) in input.iter().enumerate().skip(1) {
            // Indexing does not go out of bounds here because we start at 1.
            if ch == b'"' && input[i - 1] == b'\\' {
                // An escaped quote should not end the token.
                continue;
            }
            if ch == b'"' {
                return Ok((Token::DoubleQuoted, self.span(i + 1)));
            }
        }

        self.error_while(
            |_| true,
            "Unexpected end of input, string literal is not closed.",
        )
    }

    fn lex_in_triple_quote(&mut self) -> Result<Lexeme> {
        let input = &self.input.as_bytes()[self.start..];
        let mut n_consecutive = 0;

        // Skip over the initial opening quotes.
        for (i, &ch) in input.iter().enumerate().skip(3) {
            // Indexing does not go out of bounds here because we start past 1.
            if ch == b'"' && input[i - 1] == b'\\' {
                // An escaped quote should not end the token.
                continue;
            }
            if ch == b'"' {
                n_consecutive += 1;
                if n_consecutive == 3 {
                    return Ok((Token::TripleQuoted, self.span(i + 1)));
                }
            } else {
                n_consecutive = 0;
            }
        }

        self.error_while(
            |_| true,
            "Unexpected end of input, string literal is not closed.",
        )
    }

    fn lex_in_punct(&mut self) -> Result<Lexeme> {
        debug_assert!(self.start < self.input.len());

        let token = match self.input.as_bytes()[self.start] {
            // For those characters, we have single-character tokens.
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'[' => Token::LBracket,
            b']' => Token::RBracket,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'!' => Token::Bang,
            b',' => Token::Comma,
            b'.' => Token::Dot,
            b':' => Token::Colon,
            b';' => Token::Semicolon,
            b'=' => Token::Eq,
            b'|' => Token::Pipe,
            b'+' => Token::Plus,
            q => {
                eprintln!("TODO: {}", q);
                let error = ParseError {
                    span: self.span(1),
                    message: "Unrecognized punctuation here.",
                    note: None,
                    help: None,
                };
                return Err(error);
            }
        };
        Ok((token, self.span(1)))
    }
}
