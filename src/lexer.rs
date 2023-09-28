// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use crate::error::{IntoParseError, ParseError};
use crate::source::{DocId, Span};

pub type Result<T> = std::result::Result<T, ParseError>;

/// What quote style a string literal is quoted in (`"` or `"""`).
///
/// Yes, the names _double_ and _triple_ are slightly misleading because the
/// triple variant has three times as many quotes as the double one. _Double_
/// refers to the double quote character (`"` as opposed to `'`) while _triple_
/// refers to the number of such characters.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum QuoteStyle {
    /// A string delimited by double quotes (`"`).
    Double,
    /// A string delimited by triple double quotes (`"""`).
    Triple,
}

impl QuoteStyle {
    /// The byte length of the quotes.
    pub fn len(&self) -> usize {
        match self {
            QuoteStyle::Double => 1,
            QuoteStyle::Triple => 3,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Token {
    /// A sequence of ascii whitespace not significant for reformatting.
    ///
    /// Note, this token gets discarded from the result, it is there for
    /// internal use in the lexer on only.
    Space,

    /// A sequence of ascii whitespace that contains at least two newlines.
    Blank,

    /// A line that starts with `#!`, and runs until the end of the line.
    ///
    /// Excludes the newline itself.
    Shebang,

    /// A comment that starts with `//` and runs until the end of the line.
    ///
    /// Excludes the newline itself.
    LineComment,

    /// A sequence of ascii alphanumeric or _, not starting with a digit.
    Ident,

    /// An opening quote of a string, either `"` or `"""`.
    QuoteOpenString(QuoteStyle),

    /// An opening quote of a format string, either `f"` or `f"""`.
    QuoteOpenFormat(QuoteStyle),

    /// A closing quote of a string, either `"` or `"""`.
    QuoteClose,

    /// An inner part of a string literal or format string.
    StringInner,

    /// A single-character escape sequence inside a string literal.
    ///
    /// Includes the backslash, e.g. `\n` or `\"`.
    EscapeSingle,

    /// An escape sequence that indicates a code point with 4 hex digits.
    ///
    /// For example `\u000a`.
    EscapeUnicode4,

    /// An escape sequence that indicates a code point enclosed in {}.
    ///
    /// For example `\u{1F574}`.
    EscapeUnicodeDelim,

    /// The `{` that opens a hole inside an f-string.
    HoleOpen,

    /// The `}` that closes a hole inside an f-string.
    HoleClose,

    /// A string enclosed in double or triple quotes.
    Quoted(QuoteStyle),

    /// An f-string until its first hole, e.g. `f" str {`.
    FormatOpen(QuoteStyle),

    /// An f-string between two holes, e.g. `} str {`.
    FormatInner(QuoteStyle),

    /// An f-string after the last hole, e.g. `} str"`.
    FormatClose(QuoteStyle),

    /// A hexadecimal integer literal prefixed by `0x`.
    NumHexadecimal,

    /// A binary integer literal prefixed by `0b`.
    NumBinary,

    /// A decimal number literal, same as allowed by json.
    NumDecimal,

    /// `and`
    KwAnd,

    /// `assert`
    KwAssert,

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

    /// `null`
    KwNull,

    /// `or`
    KwOr,

    /// `then`
    KwThen,

    /// `trace`
    KwTrace,

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

    /// `==` (two equals signs)
    Eq2,

    /// `!=`
    Neq,

    /// `=` (a single equals sign)
    Eq1,

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

/// A state in the state stack of the lexer.
#[derive(Debug)]
enum State {
    /// Inside a `{}` in expression context.
    Brace,
    /// Inside a `()` in expression context.
    Paren,
    /// Inside a `[]` in expression context.
    Bracket,
    /// Inside a string literal.
    String(QuoteStyle),
    /// Inside an f-string (in the string part, not the hole part).
    Format(QuoteStyle),
    /// Inside a hole in an f-string.
    Hole,
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
    lexer.report_unclosed_delimiters()?;
    Ok(tokens)
}

struct Lexer<'a> {
    input: &'a str,
    doc: DocId,
    start: usize,

    /// A stack of states to determine what we are parsing.
    ///
    /// Also the span at which we entered this state.
    state: Vec<(Span, State)>,
}

impl<'a> Lexer<'a> {
    pub fn new(doc: DocId, input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            doc,
            start: 0,
            state: Vec::new(),
        }
    }

    /// Return a span from the current cursor location and advance the cursor.
    fn span(&mut self, len: usize) -> Span {
        let start = self.start;
        let len = len.min(self.input.len() - start);
        self.start += len;
        Span::new(self.doc, start, start + len)
    }

    /// Build a parse error at the current cursor location.
    fn error_while<F: FnMut(u8) -> bool, T>(
        &mut self,
        include: F,
        message: &'static str,
    ) -> Result<T> {
        Err(self.take_while(include).error(message))
    }

    /// Build an error of the given length at the current cursor location.
    fn error<T>(&mut self, len: usize, message: &'static str) -> Result<T> {
        Err(self.span(len).error(message))
    }

    /// Build an error at the given span, with a note at the given span.
    fn error_with_note<T>(
        &mut self,
        error_span: Span,
        message: &'static str,
        note_span: Span,
        note: &'static str,
    ) -> Result<T> {
        let error = ParseError {
            span: error_span,
            message,
            note: Some((note_span, note)),
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

    /// Return the top of the state stack (without popping).
    fn top_state(&self) -> Option<&State> {
        self.state.last().map(|(_off, state)| state)
    }

    /// Pop a closing delimiter while verifying that it is the right one.
    fn pop_delimiter(&mut self) -> Result<State> {
        let actual_end = self.input.as_bytes()[self.start];

        let top = match self.state.pop() {
            None => match actual_end {
                b')' => return self.error(1, "Found unmatched ')'."),
                b'}' => return self.error(1, "Found unmatched '}'."),
                b']' => return self.error(1, "Found unmatched ']'."),
                invalid => unreachable!("Invalid byte for `pop_delimiter`: 0x{:x}", invalid),
            },
            Some(t) => t,
        };
        let expected_end = match top.1 {
            State::Paren => b')',
            State::Brace => b'}',
            State::Bracket => b']',
            State::Hole => b'}',
            bad => panic!("Should not call `pop_delimiter` from {bad:?}."),
        };

        if actual_end == expected_end {
            return Ok(top.1);
        }

        let s = self.span(1);
        match expected_end {
            b')' => self.error_with_note(s, "Expected ')'.", top.0, "Unmatched '(' opened here."),
            b'}' => self.error_with_note(s, "Expected '}'.", top.0, "Unmatched '{' opened here."),
            b']' => self.error_with_note(s, "Expected ']'.", top.0, "Unmatched '[' opened here."),
            _ => unreachable!("End byte is one of the above three."),
        }
    }

    /// Lex one token. The input must not be empty.
    fn next(&mut self) -> Result<Lexeme> {
        match self.state.last() {
            Some((_, State::String(style))) => self.next_in_string(*style),
            Some((_, State::Format(style))) => self.next_in_format(*style),
            _ => self.next_normal(),
        }
    }

    fn next_normal(&mut self) -> Result<Lexeme> {
        let input = &self.input.as_bytes()[self.start..];

        debug_assert!(
            !input.is_empty(),
            "Must have input before continuing to lex."
        );

        if input.starts_with(b"#!") {
            return Ok(self.lex_in_shebang());
        }

        if input.starts_with(b"//") {
            return Ok(self.lex_in_line_comment());
        }

        if input.starts_with(b"f\"\"\"") {
            let style = QuoteStyle::Triple;
            let span = self.span(4);
            self.state.push((span, State::Format(style)));
            return Ok((Token::QuoteOpenFormat(style), span));
        }

        if input.starts_with(b"\"\"\"") {
            let style = QuoteStyle::Triple;
            let span = self.span(3);
            self.state.push((span, State::String(style)));
            return Ok((Token::QuoteOpenString(style), span));
        }

        if input.starts_with(b"f\"") {
            let style = QuoteStyle::Double;
            let span = self.span(2);
            self.state.push((span, State::Format(style)));
            return Ok((Token::QuoteOpenFormat(style), span));
        }

        if input[0] == b'"' {
            let style = QuoteStyle::Double;
            let span = self.span(1);
            self.state.push((span, State::String(style)));
            return Ok((Token::QuoteOpenString(style), span));
        }

        // What state to continue lexing in after the closing '}', depends on
        // whether it was closing an expression or a string interpolation.
        if input[0] == b'}' {
            if let Some(State::Hole) = self.top_state() {
                self.state.pop();
                return Ok((Token::HoleClose, self.span(1)));
            }
            // If it was a regular delimiter, then we continue lexing
            // normally.
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
            "assert" => Token::KwAssert,
            "and" => Token::KwAnd,
            "else" => Token::KwElse,
            "false" => Token::KwFalse,
            "for" => Token::KwFor,
            "if" => Token::KwIf,
            "in" => Token::KwIn,
            "let" => Token::KwLet,
            "not" => Token::KwNot,
            "null" => Token::KwNull,
            "or" => Token::KwOr,
            "then" => Token::KwThen,
            "trace" => Token::KwTrace,
            "true" => Token::KwTrue,
            _ => Token::Ident,
        };
        (token, span)
    }

    fn lex_in_shebang(&mut self) -> Lexeme {
        let contents = self.take_while(|ch| ch != b'\n');
        (Token::Shebang, contents)
    }

    fn lex_in_line_comment(&mut self) -> Lexeme {
        (Token::LineComment, self.take_while(|ch| ch != b'\n'))
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
            b"==" => Token::Eq2,
            b"!=" => Token::Neq,
            _ => return None,
        };

        Some((token, self.span(2)))
    }

    /// Try to lex punctuation of a single byte in length.
    fn lex_in_punct_monograph(&mut self) -> Result<Lexeme> {
        let span = self.span(1);
        let token = match self.input.as_bytes()[self.start] {
            b'(' => {
                self.state.push((span, State::Paren));
                Token::LParen
            }
            b'[' => {
                self.state.push((span, State::Bracket));
                Token::LBracket
            }
            b'{' => {
                self.state.push((span, State::Brace));
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
            b'=' => Token::Eq1,
            b'!' => Token::Bang,
            b'*' => Token::Star,
            b'+' => Token::Plus,
            b',' => Token::Comma,
            b'.' => Token::Dot,
            b':' => Token::Colon,
            b';' => Token::Semicolon,
            b'|' => Token::Pipe,
            b'#' => {
                return span
                    .error("Unrecognized punctuation here.")
                    .with_help("Comments are written with '//', not with '#'.")
                    .err();
            }
            _ => return span.error("Unrecognized punctuation here.").err(),
        };

        Ok((token, span))
    }

    /// Continue lexing inside a regular (non-format) string.
    fn next_in_string(&mut self, style: QuoteStyle) -> Result<Lexeme> {
        if let Some(lexeme) = self.lex_in_string(style)? {
            return Ok(lexeme);
        }
        // We know that one character is not a \ or " for sure, then we take
        // until one is, or until a newline. This ensures that we break up the
        // inner parts of the string into lines, where every '\n' occurs at the
        // start of a line. This makes it easy to strip the leading whitespace
        // later in """-strings.
        let span = self.skip_take_while(1, |ch| !matches!(ch, b'\\' | b'"' | b'\n'));
        Ok((Token::StringInner, span))
    }

    /// Continue lexing inside a format string.
    fn next_in_format(&mut self, style: QuoteStyle) -> Result<Lexeme> {
        if let Some(lexeme) = self.lex_in_string(style)? {
            return Ok(lexeme);
        }

        // This does not go index out of bounds; all continuations should have
        // at least one byte, and `lex_in_string` also checks this.
        if self.input.as_bytes()[self.start] == b'{' {
            let span = self.span(1);
            self.state.push((span, State::Hole));
            return Ok((Token::HoleOpen, span));
        }

        // Same as in `next_in_string`, but also stopping at `{`.
        let span = self.skip_take_while(1, |ch| !matches!(ch, b'\\' | b'"' | b'{' | b'\n'));
        Ok((Token::StringInner, span))
    }

    /// Lex the cases in a string literal shared between strings and f-strings.
    fn lex_in_string(&mut self, style: QuoteStyle) -> Result<Option<Lexeme>> {
        let input = &self.input.as_bytes()[self.start..];

        debug_assert!(
            !input.is_empty(),
            "Must have input before continuing to lex."
        );

        if input.starts_with(b"\\u{") {
            return Ok(Some((
                Token::EscapeUnicodeDelim,
                self.take_while(|ch| ch != b'}'),
            )));
        }
        if input.starts_with(b"\\u") {
            return Ok(Some((Token::EscapeUnicode4, self.span(6))));
        }
        if input[0] == b'\\' {
            // Note, even if the document ends and we get only one byte, that
            // will get reported as an unclosed string at the end of lexing, so
            // if lexing succeeds, all EscapeSingle tokens have length 2.
            return Ok(Some((Token::EscapeSingle, self.span(2))));
        }

        match style {
            QuoteStyle::Double if input[0] == b'"' => Ok(Some((Token::QuoteClose, self.span(1)))),
            QuoteStyle::Triple if input.starts_with(b"\"\"\"") => {
                Ok(Some((Token::QuoteClose, self.span(3))))
            }
            _ => Ok(None),
        }
    }

    fn report_unclosed_delimiters(&mut self) -> Result<()> {
        let top = match self.state.pop() {
            None => return Ok(()),
            Some(t) => t,
        };

        let span = self.span(0);

        let err = match top.1 {
            State::Paren => span
                .error("Expected ')'.")
                .with_note(top.0, "Unmatched '(' opened here."),
            State::Brace => span
                .error("Expected '}'.")
                .with_note(top.0, "Unmatched '{' opened here."),
            State::Bracket => span
                .error("Expected ']'.")
                .with_note(top.0, "Unmatched '[' opened here."),
            State::Hole => span
                .error("Expected '}' to close f-string hole.")
                .with_note(top.0, "Unmatched '{' opened here."),
            State::String(..) => span
                .error("Unexpected end of input, string literal is not closed.")
                .with_note(top.0, "String literal opened here."),
            State::Format(..) => span
                .error("Unexpected end of input, f-string is not closed.")
                .with_note(top.0, "Format string opened here."),
        };
        Err(err)
    }
}
