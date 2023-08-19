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

    /// A double-quoted f-string until its first hole, e.g. `f" str {`.
    FormatDoubleOpen,

    /// A double-quoted f-string between two holes, e.g. `} str {`.
    FormatDoubleInner,

    /// A double-quoted f-string after the last hole, e.g. `} str"`.
    FormatDoubleClose,

    /// A hexadecimal integer literal prefixed by `0x`.
    NumHexadecimal,

    /// A binary integer literal prefixed by `0b`.
    NumBinary,

    /// A decimal number literal, same as allowed by json.
    NumDecimal,

    /// `and`
    KwAnd,

    /// `false`
    KwFalse,

    /// `for`
    KwFor,

    /// `else`
    KwElse,

    /// `if`
    KwIf,

    /// `in`
    KwIn,

    /// `let`
    KwLet,

    /// `not`
    KwNot,

    /// `or`
    KwOr,

    /// `then`
    KwThen,

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

    /// `<`
    Lt,

    /// `>`
    Gt,

    /// `<=`
    LtEq,

    /// `>=`
    GtEq,

    /// `=`
    Eq,

    /// `!`
    Bang,

    /// `*`
    Star,

    /// `+`
    Plus,

    /// `,`
    Comma,

    /// `.`
    Dot,

    /// `:`
    Colon,

    /// `;`
    Semicolon,

    /// `|`
    Pipe,
}

/// The an element of the string interpolation stack.
enum Delimiter {
    /// Inside a `{}` in expression context.
    Brace,
    /// Inside a `()` in expression context.
    Paren,
    /// Inside a `[]` in expression context.
    Bracket,
    /// Inside a hole in an `f"`-quoted string.
    HoleDouble,
    /// Inside a hole in an `f"""`-quoted string.
    HoleTriple,
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

    /// A stack of delimiters to determine what we are parsing.
    ///
    /// Also the offset at which we encountered it.
    delimiters: Vec<(Delimiter, usize)>,
}

impl<'a> Lexer<'a> {
    pub fn new(doc: DocId, input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            doc,
            start: 0,
            delimiters: Vec::new(),
        }
    }

    /// Return a span from the current cursor location and advance the cursor.
    fn span(&mut self, len: usize) -> Span {
        let start = self.start;
        self.start += len;
        Span::new(self.doc, start, start + len)
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

    /// Build an error of the given length at the current cursor location.
    fn error<T>(&mut self, len: usize, message: &'static str) -> Result<T> {
        let error = ParseError {
            span: self.span(len),
            message,
            note: None,
            help: None,
        };
        Err(error)
    }

    /// Build an error of length 1, with a note at the given offset.
    fn error_with_note<T>(
        &mut self,
        message: &'static str,
        note_offset: usize,
        note: &'static str,
    ) -> Result<T> {
        let error = ParseError {
            span: self.span(1),
            message,
            note: Some((Span::new(self.doc, note_offset, note_offset + 1), note)),
            help: None,
        };
        Err(error)
    }

    /// Take `skip` bytes, and then more until `include` returns false.
    fn skip_take_while<F: FnMut(u8) -> bool>(&mut self, skip: usize, mut include: F) -> Span {
        let input = &self.input.as_bytes()[self.start + skip..];
        self.span(input.iter().take_while(|ch| include(**ch)).count() + skip)
    }

    /// Take until `include` returns false.
    fn take_while<F: FnMut(u8) -> bool>(&mut self, include: F) -> Span {
        self.skip_take_while(0, include)
    }

    /// Push a delimiter on thes stack. Does not advance the cursor.
    fn push_delimiter(&mut self, delimiter: Delimiter) {
        self.delimiters.push((delimiter, self.start));
    }

    /// Pop a closing delimiter while verifying that it is the right one.
    fn pop_delimiter(&mut self) -> Result<Delimiter> {
        let actual_end = self.input.as_bytes()[self.start];

        let top = match self.delimiters.pop() {
            None => match actual_end {
                b')' => return self.error(1, "Found unmatched ')'."),
                b'}' => return self.error(1, "Found unmatched '}'."),
                b']' => return self.error(1, "Found unmatched ']'."),
                invalid => unreachable!("Invalid byte for `pop_delimiter`: 0x{:x}", invalid),
            },
            Some(t) => t,
        };
        let expected_end = match top.0 {
            Delimiter::Paren => b')',
            Delimiter::Brace => b'}',
            Delimiter::Bracket => b']',
            Delimiter::HoleDouble => b'}',
            Delimiter::HoleTriple => b'}',
        };

        if actual_end == expected_end {
            return Ok(top.0);
        }

        match expected_end {
            b')' => self.error_with_note("Expected ')'.", top.1, "Unmatched '(' opened here."),
            b'}' => self.error_with_note("Expected '}'.", top.1, "Unmatched '{' opened here."),
            b']' => self.error_with_note("Expected ']'.", top.1, "Unmatched '[' opened here."),
            _ => unreachable!("End byte is one of the above three."),
        }
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

        if input.starts_with(b"f\"") {
            return self.lex_in_format_double_open();
        }

        if input[0] == b'"' {
            return self.lex_in_double_quote();
        }

        // What state to continue lexing in after the closing '}', depends on
        // whether it was closing a regular expression or a string
        // interpolation.
        if input[0] == b'}' {
            match self.pop_delimiter()? {
                Delimiter::HoleDouble => todo!("Continue f-quoted string."),
                Delimiter::HoleTriple => todo!("Continue triple-quoted f-string."),
                // If it was a regular delimiter, then we continue lexing
                // normally.
                _ => {}
            }
        }

        if input[0].is_ascii_digit() {
            return self.lex_in_number();
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
        let span = self.take_while(|ch| ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'-');
        let ident = span.resolve(self.input);
        let token = match ident {
            "and" => Token::KwAnd,
            "else" => Token::KwElse,
            "false" => Token::KwFalse,
            "for" => Token::KwFor,
            "if" => Token::KwIf,
            "in" => Token::KwIn,
            "let" => Token::KwLet,
            "not" => Token::KwNot,
            "or" => Token::KwOr,
            "then" => Token::KwThen,
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

    fn lex_in_format_double_open(&mut self) -> Result<Lexeme> {
        let input = &self.input.as_bytes()[self.start..];

        // Skip over the initial f and opening quote.
        for (i, &ch) in input.iter().enumerate().skip(2) {
            // Indexing does not go out of bounds here because we start at 1.
            if ch == b'"' && input[i - 1] == b'\\' {
                // An escaped quote should not end the token.
                continue;
            }
            if ch == b'{' && input[i - 1] == b'\\' {
                // An escaped { should not open a hole.
            }
            if ch == b'{' {
                self.push_delimiter(Delimiter::HoleDouble);
                return Ok((Token::FormatDoubleOpen, self.span(i + 2)));
            }
            if ch == b'"' {
                return self.error(
                    i + 2,
                    "This f-string has no holes, it can be a regular string.",
                );
            }
        }

        self.error_while(
            |_| true,
            "Unexpected end of input, string literal is not closed.",
        )
    }

    fn lex_in_number(&mut self) -> Result<Lexeme> {
        let mut input = &self.input.as_bytes()[self.start..];
        let mut n = 0;

        if input.starts_with(b"0b") {
            let span = self.skip_take_while(2, |ch| matches!(ch, b'_' | b'0' | b'1'));
            return Ok((Token::NumBinary, span));
        }

        if input.starts_with(b"0x") {
            let span = self.skip_take_while(
                2,
                |ch| matches!(ch, b'_' | b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F'),
            );
            return Ok((Token::NumHexadecimal, span));
        }

        // Allow a leading minus sign.
        if let Some(b'-') = input.first() {
            input = &input[1..];
            n += 1;
        }

        // Then we can have a 0, or a non-zero followed by more digits. We also
        // allow for numeric underscores.
        match input.first() {
            Some(b'0') => {
                input = &input[1..];
                n += 1;
            }
            Some(b'1'..=b'9') => {
                let m = input
                    .iter()
                    .take_while(|ch| ch.is_ascii_digit() || **ch == b'_')
                    .count();
                input = &input[m..];
                n += m;
            }
            _ => unreachable!("Should not have begun parsing number."),
        }

        // Then optionally a dot followed by one or more digits ("fraction" in
        // the json spec). We additionally allow for numeric underscores.
        if let Some(b'.') = input.first() {
            if input[1..].first().map(|ch| ch.is_ascii_digit()) != Some(true) {
                self.start += n;
                return self.error_while(
                    |_| false,
                    "Expected a digit to follow the decimal point in this number.",
                );
            }

            let m = input[1..]
                .iter()
                .take_while(|ch| ch.is_ascii_digit() || **ch == b'_')
                .count();
            input = &input[1 + m..];
            n += m + 1;
        }

        // Then again optionally, an exponent.
        if let Some(b'e' | b'E') = input.first() {
            if let Some(b'+' | b'-') = input[1..].first() {
                input = &input[2..];
                n += 2;
            } else {
                input = &input[1..];
                n += 1;
            }

            if input.first().map(|ch| ch.is_ascii_digit()) != Some(true) {
                self.start += n;
                return self
                    .error_while(|_| false, "Expected a digit of the number's exponent here.");
            }

            n += input
                .iter()
                .take_while(|ch| ch.is_ascii_digit() || **ch == b'_')
                .count();
        }

        Ok((Token::NumDecimal, self.span(n)))
    }

    /// Lex a token that starts with ascii punctuation.
    fn lex_in_punct(&mut self) -> Result<Lexeme> {
        debug_assert!(self.start < self.input.len());

        if let Some(result) = self.lex_in_punct_digraph() {
            return Ok(result);
        }

        self.lex_in_punct_monograph()
    }

    /// Try to lex punctuation of two bytes in length.
    fn lex_in_punct_digraph(&mut self) -> Option<Lexeme> {
        let input = &self.input.as_bytes()[self.start..];

        if input.len() < 2 {
            return None;
        }

        let token = match &input[..2] {
            b"<=" => Token::LtEq,
            b">=" => Token::GtEq,
            _ => return None,
        };

        Some((token, self.span(2)))
    }

    /// Try to lex punctuation of a single byte in length.
    fn lex_in_punct_monograph(&mut self) -> Result<Lexeme> {
        let token = match self.input.as_bytes()[self.start] {
            b'(' => {
                self.push_delimiter(Delimiter::Paren);
                Token::LParen
            }
            b'[' => {
                self.push_delimiter(Delimiter::Bracket);
                Token::LBracket
            }
            b'{' => {
                self.push_delimiter(Delimiter::Brace);
                Token::LBrace
            }
            b')' => {
                self.pop_delimiter()?;
                Token::RParen
            }
            b']' => {
                self.pop_delimiter()?;
                Token::RBracket
            }
            b'}' => {
                // Note, here we do not pop the delimiter, we have already
                // popped it before when deciding whether it was the end of an
                // interpolation hole or a regular bracket.
                Token::RBrace
            }
            b'<' => Token::Lt,
            b'>' => Token::Gt,
            b'=' => Token::Eq,
            b'!' => Token::Bang,
            b'*' => Token::Star,
            b'+' => Token::Plus,
            b',' => Token::Comma,
            b'.' => Token::Dot,
            b':' => Token::Colon,
            b';' => Token::Semicolon,
            b'|' => Token::Pipe,
            _ => return self.error(1, "Unrecognized punctuation here."),
        };

        Ok((token, self.span(1)))
    }
}
