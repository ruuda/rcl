// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use crate::cst::{Expr, NonCode, Prefixed, Seq};
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
    fn push_bracket(&mut self) -> Span {
        let start_token = self.tokens[self.cursor];
        let result = self.consume();
        self.bracket_stack.push(start_token);
        match start_token.0 {
            Token::LBrace | Token::LParen | Token::LBracket => {}
            invalid => unreachable!("Invalid token for `push_bracket`: {:?}", invalid),
        };
        result
    }

    /// Pop a closing bracket while verifying that it is the right one.
    ///
    /// Consumes the token under the cursor.
    fn pop_bracket(&mut self) -> Result<Span> {
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
            return Ok(self.consume());
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
                Some(Token::Blank) => result.push(NonCode::Blank(self.consume())),
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
                Some(Token::Blank) => {
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

    /// Consume the given token, report an error otherwise.
    fn parse_token(&mut self, expected: Token, error: &'static str) -> Result<Span> {
        match self.peek() {
            Some(token) if token == expected => Ok(self.consume()),
            _ => self.error(error),
        }
    }

    /// Consume the given token, report an error with note otherwise.
    fn parse_token_with_note(
        &mut self,
        expected: Token,
        error: &'static str,
        note_span: Span,
        note: &'static str,
    ) -> Result<Span> {
        match self.peek() {
            Some(token) if token == expected => Ok(self.consume()),
            _ => self.error_with_note(error, note_span, note),
        }
    }

    fn parse_prefixed<T, F>(&mut self, parse_inner: F) -> Result<Prefixed<T>>
    where
        F: Fn(&mut Self) -> Result<T>,
    {
        let prefix = self.parse_non_code();
        let inner = parse_inner(self)?;
        let result = Prefixed { prefix, inner };
        Ok(result)
    }

    pub fn parse_prefixed_expr(&mut self) -> Result<Prefixed<Expr>> {
        self.parse_prefixed(|s| s.parse_expr())
    }

    fn parse_expr(&mut self) -> Result<Expr> {
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
        self.parse_token(Token::Eq, "Expected '=' here.")?;

        self.skip_non_code()?;
        let value = self.parse_expr()?;

        self.skip_non_code()?;
        self.parse_token_with_note(
            Token::Semicolon,
            "Expected ';' here to close the let-binding.",
            let_,
            "Let-binding opened here.",
        )?;

        let body = self.parse_prefixed_expr()?;

        let result = Expr::Let {
            ident: ident,
            value: Box::new(value),
            body: Box::new(body),
        };

        Ok(result)
    }

    fn parse_expr_brace_lit(&mut self) -> Result<Expr> {
        let open = self.push_bracket();
        let seqs = self.parse_prefixed_seqs()?;
        let close = self.pop_bracket()?;

        let result = Expr::BraceLit {
            open,
            close,
            elements: seqs,
        };

        Ok(result)
    }

    fn parse_prefixed_seqs(&mut self) -> Result<Box<[Prefixed<Seq>]>> {
        let mut result = Vec::new();
        let mut expected_terminator = None;

        loop {
            let prefix = self.parse_non_code();
            match self.peek() {
                Some(Token::RBrace | Token::RBracket) => {
                    // TODO: In this case we lose the prefix that we parsed. So
                    // comments in an empty collection literal do not survive,
                    // need to find a way to disallow this in the first place.
                    // Maybe we could validate that the prefix does not contain
                    // comments, and error out if it does?
                    break;
                }
                tok if tok == expected_terminator => {
                    self.consume();
                    continue;
                }
                _ => {
                    let seq = self.parse_seq()?;
                    expected_terminator = match seq {
                        Seq::Elem { .. } => Some(Token::Comma),
                        Seq::AssocExpr { .. } => Some(Token::Comma),
                        Seq::AssocIdent { .. } => Some(Token::Semicolon),
                        Seq::Let { .. } => Some(Token::Comma),
                        Seq::For { .. } => Some(Token::Comma),
                        Seq::If { .. } => Some(Token::Comma),
                    };
                    let prefixed = Prefixed { prefix, inner: seq };
                    result.push(prefixed);
                }
            }
        }

        Ok(result.into_boxed_slice())
    }

    fn parse_seq(&mut self) -> Result<Seq> {
        // Here we have a lookahead of two tokens ... not great if we want to
        // keep the grammar simple, but for making the syntax prettier it is
        // worth some complications to allow { a = b; p = q } notation.
        let next1 = self.peek();
        let next2 = self.tokens.get(self.cursor + 1).map(|t| t.0);

        match (next1, next2) {
            // TODO: Would need to skip noncode here ... maybe it's better to
            // parse an expression, and re-interpret it later if it reads like a
            // variable access?
            (Some(Token::Ident), Some(Token::Eq)) => self.parse_seq_assoc_ident(),
            (Some(Token::KwLet), _) => self.parse_seq_let(),
            (Some(Token::KwFor), _) => self.parse_seq_for(),
            // Note, we can't distinguish an if seq from element seq with if
            // expr inside here, but that's acceptable. If you want an if
            // expr, you can put it in parentheses.
            (Some(Token::KwIf), _) => self.parse_seq_if(),
            _ => {
                let expr = self.parse_expr()?;
                self.skip_non_code()?;
                let result = match self.peek() {
                    Some(Token::Colon) => {
                        self.skip_non_code()?;
                        let value = self.parse_expr()?;
                        Seq::AssocExpr {
                            field: Box::new(expr),
                            value: Box::new(value),
                        }
                    }
                    _ => Seq::Elem {
                        value: Box::new(expr),
                    },
                };
                Ok(result)
            }
        }
    }

    fn parse_seq_assoc_ident(&mut self) -> Result<Seq> {
        let ident = self.consume();

        self.skip_non_code()?;
        self.parse_token(Token::Eq, "Expected '=' here.")?;

        self.skip_non_code()?;
        let value = self.parse_expr()?;

        let result = Seq::AssocIdent {
            field: ident,
            value: Box::new(value),
        };

        Ok(result)
    }

    fn parse_seq_let(&mut self) -> Result<Seq> {
        unimplemented!("TODO: Let");
    }

    fn parse_seq_for(&mut self) -> Result<Seq> {
        unimplemented!("TODO: For");
    }

    fn parse_seq_if(&mut self) -> Result<Seq> {
        unimplemented!("TODO: If");
    }
}
