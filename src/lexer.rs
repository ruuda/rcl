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

    /// A sequence of ascii alphanumeric or _, not starting with a digit.
    Ident,

    /// A string enclosed in double quotes.
    DoubleQuoted,
}

#[derive(Debug)]
enum State {
    Base,
    InSpace,
    InIdent,
    InLineComment,
    InDoubleQuote,
    Done,
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

    /// Lex the entire input document, return the tokens.
    fn run(mut self) -> Result<Vec<(Token, Span)>> {
        let mut state = State::Base;
        loop {
            let (start, next_state) = match state {
                State::Base => self.lex_base()?,
                State::InSpace => self.lex_in_space()?,
                State::InIdent => self.lex_in_ident()?,
                State::InLineComment => self.lex_in_line_comment()?,
                State::InDoubleQuote => self.lex_in_double_quote()?,
                State::Done => break,
            };
            self.start = start;
            state = next_state;
        }

        Ok(self.tokens)
    }

    fn lex_base(&mut self) -> Result<(usize, State)> {
        let input = &self.input.as_bytes()[self.start..];

        if input.len() == 0 {
            return Ok((self.start, State::Done));
        }

        if input.starts_with(b"\"") {
            return Ok((self.start, State::InDoubleQuote));
        }

        if input.starts_with(b"//") {
            return Ok((self.start, State::InLineComment));
        }

        if input[0].is_ascii_whitespace() {
            return Ok((self.start, State::InSpace));
        }

        if input[0].is_ascii_alphabetic() || input[0].is_ascii_digit() {
            return Ok((self.start, State::InIdent));
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
}
