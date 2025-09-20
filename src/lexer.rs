// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The lexer splits a string into a sequence of tokens.

use crate::error::{IntoError, Result};
use crate::source::{DocId, Span};

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

/// An escape sequence inside a string literal.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Escape {
    /// A single-character escape sequence, e.g. `\n` or `\\`.
    Single,
    /// A `\u` escape sequence followed by 4 hex digits.
    Unicode4,
    /// A `\u{...}` escape sequence.
    UnicodeDelim,
}

/// Whether a string literal is a format string or a regular string.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum StringPrefix {
    /// No prefix, just the quotes: a regular string literal.
    None,
    /// An `f` prefix that indicates a format string.
    Format,
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

    /// An opening quote of a string, either `"`, `f"`, `"""`, or `f"""`.
    QuoteOpen(StringPrefix, QuoteStyle),

    /// A closing quote of a string, either `"` or `"""`.
    QuoteClose,

    /// An inner part of a string literal or format string.
    StringInner,

    /// An escape sequence inside a string literal.
    Escape(Escape),

    /// The `{` that opens a hole inside an f-string.
    HoleOpen,

    /// The `}` that closes a hole inside an f-string.
    HoleClose,

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

    /// `import`
    KwImport,

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

    /// `->`
    ThinArrow,

    /// `=>`
    FatArrow,

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

    /// `-`
    Minus,

    /// `.`
    Dot,

    /// `..`
    DotDot,

    /// `...`
    DotDotDot,

    /// `/`
    Slash,

    /// `:`
    Colon,

    /// `;`
    Semicolon,

    /// `|`
    Pipe,

    /// End of file.
    ///
    /// This token is not returned by the lexer, but it's used internally in the
    /// parser to return when it runs out of tokens, to avoid having to use
    /// `Option` everywhere.
    Eof,
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
            (token, span) => {
                debug_assert!(
                    input.is_char_boundary(span.start()),
                    // coverage:off -- Error not expected to be hit.
                    "Start of {token:?} is not a char boundary.",
                    // coverage:on
                );
                debug_assert!(
                    input.is_char_boundary(span.end()),
                    // coverage:off -- Error not expected to be hit.
                    "Start of {token:?} is not a char boundary.",
                    // coverage:on
                );
                tokens.push((token, span));
            }
        };
    }
    lexer.report_unclosed_delimiters()?;
    Ok(tokens)
}

/// Return whether a given string is a keyword.
pub fn is_keyword(ident: &str) -> bool {
    Lexer::get_keyword_or_ident(ident) != Token::Ident
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

    /// Take at least `min_len` bytes from the input, rounding up to a character boundary.
    ///
    /// If we already know it is safe to slice a given length, then `span` is
    /// better to use, but if we haven't inspected all bytes in the span, then
    /// we may be slicing a code point in half, which would be bad because when
    /// we try to use it, slicing the span out will panic.
    fn span_char(&mut self, min_len: usize) -> Span {
        debug_assert!(self.start + min_len <= self.input.len());
        let mut n = min_len;
        while !self.input.is_char_boundary(self.start + n) {
            n += 1;
        }
        self.span(n)
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
    fn pop_delimiter(&mut self, span: Span) -> Result<State> {
        let actual_end = self.input.as_bytes()[span.start()];

        let top = match self.state.pop() {
            None => match actual_end {
                b')' => return span.error("Found unmatched ')'.").err(),
                b'}' => return span.error("Found unmatched '}'.").err(),
                b']' => return span.error("Found unmatched ']'.").err(),
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

        let err = match expected_end {
            b')' => span
                .error("Expected ')'.")
                .with_note(top.0, "Unmatched '(' opened here."),
            b'}' => span
                .error("Expected '}'.")
                .with_note(top.0, "Unmatched '{' opened here."),
            b']' => span
                .error("Expected ']'.")
                .with_note(top.0, "Unmatched '[' opened here."),
            _ => unreachable!("End byte is one of the above three."),
        };
        err.err()
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
            // coverage:off -- Error not expected to be hit.
            "Must have input before continuing to lex.",
            // coverage:on
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
            return Ok((Token::QuoteOpen(StringPrefix::Format, style), span));
        }

        if input.starts_with(b"\"\"\"") {
            let style = QuoteStyle::Triple;
            let span = self.span(3);
            self.state.push((span, State::String(style)));
            return Ok((Token::QuoteOpen(StringPrefix::None, style), span));
        }

        if input.starts_with(b"f\"") {
            let style = QuoteStyle::Double;
            let span = self.span(2);
            self.state.push((span, State::Format(style)));
            return Ok((Token::QuoteOpen(StringPrefix::Format, style), span));
        }

        if input[0] == b'"' {
            let style = QuoteStyle::Double;
            let span = self.span(1);
            self.state.push((span, State::String(style)));
            return Ok((Token::QuoteOpen(StringPrefix::None, style), span));
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
            return self
                .take_while(|ch| ch.is_ascii_control())
                .error("Control characters are not supported here.")
                .err();
        }

        if input[0] > 127 {
            // Multi-byte sequences of non-ascii code points are fine in
            // strings and comments, but not in the source code where we
            // expect identifiers.
            return self
                .take_while(|ch| ch > 127)
                .error("Non-ascii characters are not supported here.")
                .err();
        }

        // coverage:off -- Code not expected to be reached.
        unreachable!(
            "We should have handled all bytes, but we forgot {} (0x{:02x})",
            char::from_u32(input[0] as u32).unwrap(),
            input[0],
        );
        // coverage:on
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

    /// Return which keyword the string is, or identifier otherwise.
    fn get_keyword_or_ident(ident: &str) -> Token {
        match ident {
            "and" => Token::KwAnd,
            "assert" => Token::KwAssert,
            "else" => Token::KwElse,
            "false" => Token::KwFalse,
            "for" => Token::KwFor,
            "if" => Token::KwIf,
            "import" => Token::KwImport,
            "in" => Token::KwIn,
            "let" => Token::KwLet,
            "not" => Token::KwNot,
            "null" => Token::KwNull,
            "or" => Token::KwOr,
            "trace" => Token::KwTrace,
            "true" => Token::KwTrue,
            _ => Token::Ident,
        }
    }

    fn lex_in_ident(&mut self) -> Lexeme {
        let span = self.take_while(|ch| ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'-');
        let ident = span.resolve(self.input);
        let token = Lexer::get_keyword_or_ident(ident);
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
            let mut has_digit = false;
            let span = self.skip_take_while(2, |ch| match ch {
                b'_' => true,
                b'0' | b'1' => {
                    has_digit = true;
                    true
                }
                _ => false,
            });
            if !has_digit {
                // We need at least one digit after the prefix.
                return span
                    .error("Expected a binary digit after 0b in this number.")
                    .err();
            }
            return Ok((Token::NumBinary, span));
        }

        if input.starts_with(b"0x") {
            let mut has_digit = false;
            let span = self.skip_take_while(2, |ch| match ch {
                b'_' => true,
                b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' => {
                    has_digit = true;
                    true
                }
                _ => false,
            });
            if !has_digit {
                return span
                    .error("Expected a hexadecimal digit after 0x in this number.")
                    .err();
            }
            return Ok((Token::NumHexadecimal, span));
        }

        // The json spec allows a leading minus sign in numbers. We allow that
        // too, but it's parsed as a separate `Token::Minus`, so we don't need
        // to consider it here.

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
                return self
                    .span(1)
                    .error("Expected a digit to follow the decimal point in this number.")
                    .err();
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
                    .span(0)
                    .error("Expected a digit of the number's exponent here.")
                    .err();
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

        // There is one punctuation trigraph.
        if self.input.as_bytes()[self.start..].starts_with(b"...") {
            return Ok((Token::DotDotDot, self.span(3)));
        }

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
            b"->" => Token::ThinArrow,
            b"=>" => Token::FatArrow,
            b".." => Token::DotDot,
            _ => return None,
        };

        Some((token, self.span(2)))
    }

    /// Try to lex punctuation of a single byte in length.
    fn lex_in_punct_monograph(&mut self) -> Result<Lexeme> {
        let span = self.span(1);
        let token = match self.input.as_bytes()[span.start()] {
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
                self.pop_delimiter(span)?;
                Token::RParen
            }
            b']' => {
                self.pop_delimiter(span)?;
                Token::RBracket
            }
            b'}' => {
                self.pop_delimiter(span)?;
                Token::RBrace
            }
            b'<' => Token::Lt,
            b'>' => Token::Gt,
            b'=' => Token::Eq1,
            b'!' => Token::Bang,
            b'*' => Token::Star,
            b'+' => Token::Plus,
            b',' => Token::Comma,
            b'-' => Token::Minus,
            b'.' => Token::Dot,
            b'/' => Token::Slash,
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
            // coverage:off -- Error not expected to be hit.
            "Must have input before continuing to lex.",
            // coverage:on
        );

        if input.starts_with(b"\\u{") {
            // Take only hex digits, stop at anything else, even if we haven't
            // encountered the closing `}` yet. If we only stopped at `}`, then
            // we might continue past the closing quote of the string, and that
            // would result in very puzzling errors.
            let mut span = self.skip_take_while(3, |ch| ch.is_ascii_hexdigit());
            let input = &self.input.as_bytes()[self.start..];
            if !input.is_empty() && input[0] == b'}' {
                span = self.span(1).union(span);
                return Ok(Some((Token::Escape(Escape::UnicodeDelim), span)));
            } else {
                return span
                    .trim_start(span.len())
                    .error("Expected '}' to close Unicode escape sequence.")
                    .err();
            }
        }
        if input.starts_with(b"\\u") {
            // Also here, we cannot blindly take 6 bytes, because in the error
            // case, that might eat the closing quote and lead to very puzzling
            // errors.
            let input = &self.input.as_bytes()[self.start..];
            let n = input
                .iter()
                .skip(2)
                .take_while(|ch| ch.is_ascii_hexdigit())
                .take(4)
                .count();
            if n == 4 {
                return Ok(Some((Token::Escape(Escape::Unicode4), self.span(6))));
            } else {
                return self
                    .span(2 + n)
                    .error("Expected four hex digits after '\\u' Unicode escape sequence.")
                    .with_help(
                        "You can also use up to six hex digits enclosed in '{}'. \
                        For example '\\u{1F574}' or '\\u{0a}'.",
                    )
                    .err();
            }
        }
        if input[0] == b'\\' {
            // Due to the way the golden tests work, we can't make an input that
            // has an EOF right after the \, there will always be a newline.
            // coverage:off
            if input.len() == 1 {
                return self
                    .span(1)
                    .error("Unexpected end of input in escape sequence.")
                    .err();
            }
            // coverage:on

            // Special-case newlines for two reasons:
            // 1. So we can report a special message that \ does not escape the
            //    end of the line.
            // 2. So we don't end up with newlines in the escape sequences,
            //    because the pretty-printer does not expect those.
            if input[1] == b'\r' || input[1] == b'\n' {
                return self
                    .span(2)
                    .error("Invalid escape sequence.")
                    .with_help(
                        "To break a long string across lines, break it into \
                        multiple strings and concatenate them with '+'.",
                    )
                    .err();
            }

            // We cannot just take the next character, because it may not lie
            // inside a code point boundary.
            let span = self.span_char(2);
            return match span.len() {
                2 => Ok(Some((Token::Escape(Escape::Single), span))),
                _ => span.error("Invalid escape sequence.").err(),
            };
        }

        match style {
            QuoteStyle::Double if input[0] == b'"' => {
                self.state.pop();
                Ok(Some((Token::QuoteClose, self.span(1))))
            }
            QuoteStyle::Triple if input.starts_with(b"\"\"\"") => {
                self.state.pop();
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
                .error("Expected '}' here to close format string hole.")
                .with_note(top.0, "Unmatched '{' opened here."),
            State::String(..) => span
                .error("Unexpected end of input, string literal is not closed.")
                .with_note(top.0, "String literal opened here."),
            State::Format(..) => span
                .error("Unexpected end of input, format string is not closed.")
                .with_note(top.0, "Format string opened here."),
        };
        err.err()
    }
}
