// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use crate::source::{DocId, Span};
use crate::error::SyntaxError;

pub type Result<T> = std::result::Result<T, SyntaxError>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Token {
    /// A sequence of ascii whitespace.
    Space,

    /// A comment that starts with `//` and runs until the end of the line.
    ///
    /// Excludes the newline itself.
    LineComment,

    /// A sequence of ascii alphanumeric or _, not starting with a digit.
    Ident,

    /// A string enclosed in double quotes.
    DoubleQuoted,

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
}

struct Lexer<'a> {
    input: &'a str,
    doc: DocId,
    start: usize,
    tokens: Vec<(Token, Span)>,
}

/// Lex a document.
pub fn lex(doc: DocId, input: &str) -> Result<Vec<(Token, Span)>> {
    Lexer::new(doc, input).run()
}

impl<'a> Lexer<'a> {
    pub fn new(doc: DocId, input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            doc,
            start: 0,
            tokens: Vec::new(),
        }
    }

    /// Return a span from the current cursor location.
    fn span(&self, len: usize) -> Span {
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
        Span {
            start: self.start,
            len: len as u32,
            doc: self.doc,
        }
    }

    fn push(&mut self, token: Token, len: usize) {
        self.tokens.push((token, self.span(len)));
    }

    /// Build a parse error at the current cursor location.
    fn error_while<F: FnMut(u8) -> bool, T>(
        &self,
        mut include: F,
        message: &'static str,
    ) -> Result<T> {
        let input = &self.input.as_bytes()[self.start..];
        let len = input.iter().take_while(|ch| include(**ch)).count();
        let error = SyntaxError {
            span: self.span(len),
            message: message,
            note: None,
        };
        Err(error)
    }

    /// Include the first `n_prefix` bytes, then until `include` returns false.
    fn lex_prefix_while<F: FnMut(u8) -> bool>(
        &mut self,
        n_prefix: usize,
        mut include: F,
        token: Token,
    ) -> usize {
        let input = &self.input.as_bytes()[self.start..];
        let len = input[n_prefix..].iter().take_while(|ch| include(**ch)).count();
        self.push(token, len + n_prefix);
        self.start + len + n_prefix
    }

    /// Lex until `include` returns false.
    fn lex_while<F: FnMut(u8) -> bool>(&mut self, include: F, token: Token) -> usize {
        let n_prefix = 0;
        self.lex_prefix_while(n_prefix, include, token)
    }

    /// Lex the entire input document, return the tokens.
    fn run(mut self) -> Result<Vec<(Token, Span)>> {
        while self.start < self.input.len() {
            self.start = self.lex_base()?;
        }
        Ok(self.tokens)
    }

    fn lex_base(&mut self) -> Result<usize> {
        let input = &self.input.as_bytes()[self.start..];

        debug_assert!(input.len() > 0, "Must have input before continuing to lex.");

        if input.starts_with(b"\"") {
            return self.lex_in_double_quote();
        }

        if input.starts_with(b"//") {
            return Ok(self.lex_in_line_comment());
        }

        if input[0].is_ascii_whitespace() {
            return Ok(self.lex_in_space());
        }

        if input[0].is_ascii_alphabetic() || input[0].is_ascii_digit() {
            return Ok(self.lex_in_ident());
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

    fn lex_in_space(&mut self) -> usize {
        self.lex_while(|ch| ch.is_ascii_whitespace(), Token::Space)
    }

    fn lex_in_ident(&mut self) -> usize {
        self.lex_while(|ch| ch.is_ascii_alphanumeric() || ch == b'_', Token::Ident)
    }

    fn lex_in_line_comment(&mut self) -> usize {
        self.lex_while(|ch| ch != b'\n', Token::LineComment)
    }

    fn lex_in_double_quote(&mut self) -> Result<usize> {
        let input = &self.input.as_bytes()[self.start..];

        // Skip over the initial opening quote.
        for (i, &ch) in input.iter().enumerate().skip(1) {
            // Indexing does not go out of bounds here because we start at 1.
            if ch == b'"' && input[i - 1] == b'\\' {
                // An escaped quote should not end the token.
                continue;
            }
            if ch == b'"' {
                self.push(Token::DoubleQuoted, i + 1);
                return Ok(self.start + i + 1);
            }
        }

        let error = SyntaxError {
            span: self.span(input.len()),
            message: "Unexpected end of input, string literal is not closed.",
            note: None,
        };
        Err(error)
    }

    fn lex_in_punct(&mut self) -> Result<usize> {
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
            q => {
                eprintln!("TODO: {}", q);
                let error = SyntaxError {
                    span: self.span(1),
                    message: "Unrecognized punctuation here.",
                    note: None,
                };
                return Err(error);
            }
        };
        self.push(token, 1);
        Ok(self.start + 1)
    }
}
