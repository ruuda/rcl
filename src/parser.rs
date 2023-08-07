// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use crate::cst::{Expr, NonCode, Prefixed};
use crate::error::ParseError;
use crate::lexer::{self, Token};
use crate::source::{DocId, Span};

pub type Result<T> = std::result::Result<T, ParseError>;

/// Parse an input document into a concrete syntax tree.
pub fn parse(doc: DocId, input: &str) -> Result<Prefixed<Expr>> {
    let tokens = lexer::lex(doc, input)?;
    let mut parser = Parser::new(doc, input, &tokens);
    parser.parse_prefixed_expr()
}

struct Parser<'a> {
    doc: DocId,
    input: &'a str,
    tokens: &'a [(Token, Span)],
    cursor: usize,

    /// The unclosed opening brackets (all of `()`, `[]`, `{}`) encountered.
    bracket_stack: Vec<(Token, Span)>,

    /// The last known valid location where a comment was allowed.
    ///
    /// This is used in error reporting to provide a hint for where to place the
    /// comment.
    comment_anchor: Span,
}

impl<'a> Parser<'a> {
    pub fn new(doc: DocId, input: &'a str, tokens: &'a [(Token, Span)]) -> Parser<'a> {
        Parser {
            doc,
            input,
            tokens,
            cursor: 0,
            bracket_stack: Vec::new(),
            comment_anchor: Span {
                doc,
                start: 0,
                len: 0,
            },
        }
    }

    /// Build a parse error at the current cursor location.
    fn error<T>(&self, message: &'static str) -> Result<T> {
        let err = ParseError {
            span: self.peek_span(),
            message,
            note: None,
        };

        Err(err)
    }

    /// Build a parse error at the current cursor location, and a note elsewhere.
    fn error_with_note<T>(
        &self,
        message: &'static str,
        note_span: Span,
        note: &'static str,
    ) -> Result<T> {
        self.error(message).map_err(|err| ParseError {
            note: Some((note, note_span)),
            ..err
        })
    }

    /// Return the token under the cursor, if there is one.
    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.cursor).map(|t| t.0)
    }

    /// Return the span under the cursor, or end of document otherwise.
    fn peek_span(&self) -> Span {
        self.tokens
            .get(self.cursor)
            .map(|t| t.1)
            .unwrap_or_else(|| Span {
                doc: self.doc,
                start: self.input.len(),
                len: 0,
            })
    }

    /// Advance the cursor by one token, consuming the token under the cursor.
    ///
    /// Returns the span of the consumed token.
    fn consume(&mut self) -> Span {
        let result = self.tokens[self.cursor].1;

        self.cursor += 1;
        debug_assert!(
            self.cursor <= self.tokens.len(),
            "Cursor should not go more than beyond the last token.",
        );

        result
    }

    /// Push an opening bracket onto the stack of brackets when inside a query.
    ///
    /// Consumes the token under the cursor.
    fn push_bracket(&mut self) {
        let start_token = self.tokens[self.cursor];
        self.consume();
        self.bracket_stack.push(start_token);
        match start_token.0 {
            Token::LBrace | Token::LParen | Token::LBracket => {}
            invalid => unreachable!("Invalid token for `push_bracket`: {:?}", invalid),
        };
    }

    /// Pop a closing bracket while verifying that it is the right one.
    ///
    /// Consumes the token under the cursor.
    fn pop_bracket(&mut self) -> Result<()> {
        let actual_end_token = self.tokens[self.cursor].0;
        let top = match self.bracket_stack.pop() {
            None => match actual_end_token {
                Token::RParen => return self.error("Found unmatched ')'."),
                Token::RBrace => return self.error("Found unmatched '}'."),
                Token::RBracket => return self.error("Found unmatched ']'."),
                invalid => unreachable!("Invalid token for `pop_bracket`: {:?}", invalid),
            },
            Some(t) => t,
        };
        let expected_end_token = match top.0 {
            Token::LParen => Token::RParen,
            Token::LBrace => Token::RBrace,
            Token::LBracket => Token::RBracket,
            invalid => unreachable!("Invalid token on bracket stack: {:?}", invalid),
        };

        if actual_end_token == expected_end_token {
            self.consume();
            return Ok(());
        }

        match expected_end_token {
            Token::RParen => {
                self.error_with_note("Expected ')'.", top.1, "Unmatched '(' opened here.")
            }
            Token::RBrace => {
                self.error_with_note("Expected '}'.", top.1, "Unmatched '{' opened here.")
            }
            Token::RBracket => {
                self.error_with_note("Expected ']'.", top.1, "Unmatched '[' opened here.")
            }
            _ => unreachable!("End token is one of the above three."),
        }
    }

    /// Eat comments and whitespace.
    ///
    /// This may advance the cursor even if it returns `None`, when the
    /// whitespace was significant enough to keep.
    fn parse_non_code(&mut self) -> Box<[NonCode]> {
        let mut result = Vec::new();

        loop {
            match self.peek() {
                Some(Token::LineComment) => result.push(NonCode::LineComment(self.consume())),
                Some(Token::Space) => {
                    let span = self.consume();
                    let contents = span.resolve(self.input).as_bytes();
                    if contents.iter().filter(|ch| **ch == b'\n').count() > 1 {
                        result.push(NonCode::Blank(span));
                    }
                }
                _ => {
                    // If it's not a space, then this is the last location where
                    // a comment could have been inserted. Record that, so we
                    // can suggest this place in case an invalid comment is
                    // encountered.
                    self.comment_anchor = self.peek_span();
                    self.comment_anchor.len = 0;
                    return result.into_boxed_slice();
                }
            }
        }
    }

    /// Skip over any non-code tokens.
    fn skip_non_code(&mut self) -> Result<()> {
        loop {
            match self.peek() {
                Some(Token::Space) => {
                    self.consume();
                }
                Some(Token::LineComment) => {
                    return self.error_with_note(
                        "A comment is not allowed here.",
                        self.comment_anchor,
                        "Try inserting the comment before this instead.",
                    );
                }
                _ => return Ok(()),
            }
        }
    }

    /// Expect an identifier.
    fn parse_ident(&mut self) -> Result<Span> {
        match self.peek() {
            Some(Token::Ident) => Ok(self.consume()),
            _ => self.error("Expected an identifier here."),
        }
    }

    pub fn parse_prefixed_expr(&mut self) -> Result<Prefixed<Expr>> {
        let prefix = self.parse_non_code();
        let expr = self.parse_expr()?;
        let result = Prefixed {
            prefix,
            inner: expr,
        };
        Ok(result)
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.skip_non_code()?;
        match self.peek() {
            Some(Token::KwLet) => self.parse_expr_let(),
            Some(Token::LBrace) => self.parse_expr_brace_lit(),
            _ => self.error("Expected an expression here."),
        }
    }

    fn parse_expr_let(&mut self) -> Result<Expr> {
        // Consume the `let` keyword.
        let let_ = self.consume();

        self.skip_non_code()?;
        let ident = self.parse_ident()?;

        self.skip_non_code()?;
        match self.peek() {
            Some(Token::Eq) => self.consume(),
            _ => return self.error("Expected '=' here."),
        };

        let value = self.parse_expr()?;

        self.skip_non_code()?;
        match self.peek() {
            Some(Token::Semicolon) => self.consume(),
            _ => {
                return self.error_with_note(
                    "Expected ';' here to close the let-binding.",
                    let_,
                    "Let-binding opened here.",
                );
            }
        };

        let body = self.parse_prefixed_expr()?;

        let result = Expr::Let {
            ident: ident,
            value: Box::new(value),
            body: Box::new(body),
        };
        Ok(result)
    }

    fn parse_expr_brace_lit(&mut self) -> Result<Expr> {
        unimplemented!("TODO; cursor is at {:?}", self.peek());
    }
}