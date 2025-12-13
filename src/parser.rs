// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The parser converts a sequence of tokens into a Concrete Syntax Tree.

use crate::cst::{
    BinOp, Chain, Expr, List, NonCode, Prefixed, Seq, SeqControl, Stmt, StringPart, Type, UnOp,
    Yield,
};
use crate::error::{Error, IntoError, Result};
use crate::lexer::{Lexeme, QuoteStyle, StringPrefix, Token};
use crate::pprint::{concat, Doc};
use crate::source::{DocId, Span};

/// Parse an input document into a concrete syntax tree.
pub fn parse(doc: DocId, input: &str, tokens: &[Lexeme]) -> Result<(Span, Expr)> {
    let mut parser = Parser::new(doc, input, tokens);

    // Comments at the start of the document are allowed, but the document
    // should not start with blank lines, those we drop.
    parser.skip_blanks();

    let (span, result) = parser.parse_expr()?;
    parser.parse_eof()?;
    Ok((span, result))
}

fn to_unop(token: Token) -> Option<UnOp> {
    match token {
        Token::KwNot => Some(UnOp::Not),
        Token::Minus => Some(UnOp::Neg),
        _ => None,
    }
}

fn to_binop(token: Token) -> Option<BinOp> {
    match token {
        Token::KwAnd => Some(BinOp::And),
        Token::KwOr => Some(BinOp::Or),
        Token::Pipe => Some(BinOp::Union),
        Token::Plus => Some(BinOp::Add),
        Token::Minus => Some(BinOp::Sub),
        Token::Star => Some(BinOp::Mul),
        Token::Slash => Some(BinOp::Div),
        Token::Lt => Some(BinOp::Lt),
        Token::Gt => Some(BinOp::Gt),
        Token::LtEq => Some(BinOp::LtEq),
        Token::GtEq => Some(BinOp::GtEq),
        Token::Eq2 => Some(BinOp::Eq),
        Token::Neq => Some(BinOp::Neq),
        _ => None,
    }
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

    /// The depth of parsing expressions and sequences, to prevent stack
    /// overflow.
    depth: u32,
}

impl<'a> Parser<'a> {
    pub fn new(doc: DocId, input: &'a str, tokens: &'a [(Token, Span)]) -> Parser<'a> {
        Parser {
            doc,
            input,
            tokens,
            cursor: 0,
            bracket_stack: Vec::new(),
            comment_anchor: Span::new(doc, 0, 0),
            depth: 0,
        }
    }

    /// Return the token under the cursor.
    fn peek(&self) -> Token {
        self.peek_n(0)
    }

    /// Return the next code token, ignoring whitespace and non-code.
    fn peek_past_non_code(&self) -> Token {
        self.tokens[self.cursor..]
            .iter()
            .filter(|t| !matches!(t.0, Token::Blank | Token::LineComment))
            .map(|t| t.0)
            .next()
            .unwrap_or(Token::Eof)
    }

    /// Return the token `offset` tokens after the cursor, if there is one.
    fn peek_n(&self, offset: usize) -> Token {
        self.tokens
            .get(self.cursor + offset)
            .map(|t| t.0)
            .unwrap_or(Token::Eof)
    }

    /// Return the span under the cursor, or end of document otherwise.
    fn peek_span(&self) -> Span {
        self.tokens
            .get(self.cursor)
            .map(|t| t.1)
            .unwrap_or(Span::new(self.doc, self.input.len(), self.input.len()))
    }

    /// Build a parse error at the current cursor location.
    fn error(&self, message: &'static str) -> Error {
        self.peek_span().error(message)
    }

    /// Advance the cursor by one token, consuming the token under the cursor.
    ///
    /// Returns the span of the consumed token.
    fn consume(&mut self) -> Span {
        let result = self.tokens[self.cursor].1;

        self.cursor += 1;
        debug_assert!(
            self.cursor <= self.tokens.len(),
            // coverage:off -- Error not expected to be hit.
            "Cursor should not go more than beyond the last token.",
            // coverage:on
        );

        result
    }

    fn increase_depth(&mut self) -> Result<()> {
        self.depth += 1;

        if self.depth >= 100 {
            return self
                .error("Parser recursion limit reached, please reduce nesting.")
                .err();
        }

        Ok(())
    }

    fn decrease_depth(&mut self) {
        debug_assert!(self.depth > 0, "Expression depth underflow.");
        self.depth -= 1;
    }

    /// Return the span from start until (but not including) the cursor, stripping trailing noncode.
    fn span_from(&self, start: Span) -> Span {
        let end = self.tokens[..self.cursor]
            .iter()
            .rev()
            .filter(|t| !matches!(t.0, Token::Blank | Token::LineComment))
            .map(|t| t.1.end())
            .next()
            .expect("If we pushed a start, we should find at least that.");
        Span::new(self.doc, start.start(), end)
    }

    /// Push an opening bracket onto the stack of brackets when inside a query.
    ///
    /// Consumes the token under the cursor.
    fn push_bracket(&mut self) -> Result<Span> {
        self.increase_depth()?;
        let start_token = self.tokens[self.cursor];
        let result = self.consume();
        self.bracket_stack.push(start_token);
        match start_token.0 {
            Token::LBrace | Token::LParen | Token::LBracket => {}
            invalid => unreachable!("Invalid token for `push_bracket`: {:?}", invalid),
        };
        Ok(result)
    }

    /// Pop a closing bracket while verifying that it is the right one.
    ///
    /// Consumes the token under the cursor.
    fn pop_bracket(&mut self) -> Result<Span> {
        self.decrease_depth();
        let actual_end_token = self.tokens.get(self.cursor).map(|t| t.0);
        let top = self
            .bracket_stack
            .pop()
            .expect("If brackets were unmatched, lexing would have failed.");
        let expected_end_token = match top.0 {
            Token::LParen => Token::RParen,
            Token::LBrace => Token::RBrace,
            Token::LBracket => Token::RBracket,
            invalid => unreachable!("Invalid token on bracket stack: {:?}", invalid),
        };

        if actual_end_token == Some(expected_end_token) {
            return Ok(self.consume());
        }

        // The lexer ensures matching brackets, but even in a document where
        // that is the case, we may still encounter a different token where the
        // parser expects a closing bracket. E.g. in `{1 1}`.
        let err = match expected_end_token {
            Token::RParen => self
                .error("Expected ')'.")
                .with_note(top.1, "Unmatched '(' opened here."),
            Token::RBrace => self
                .error("Expected '}'.")
                .with_note(top.1, "Unmatched '{' opened here."),
            Token::RBracket => self
                .error("Expected ']'.")
                .with_note(top.1, "Unmatched '[' opened here."),
            _ => unreachable!("End token is one of the above three."),
        };
        err.err()
    }

    /// Eat comments and whitespace.
    ///
    /// This may advance the cursor even if it returns `None`, when the
    /// whitespace was significant enough to keep.
    #[must_use]
    fn parse_non_code(&mut self) -> Box<[NonCode]> {
        let mut result = Vec::new();

        loop {
            match self.peek() {
                Token::LineComment => result.push(NonCode::LineComment(self.consume())),
                Token::Shebang => result.push(NonCode::Shebang(self.consume())),
                Token::Blank => result.push(NonCode::Blank(self.consume())),
                _ => {
                    // If it's not a space, then this is the last location where
                    // a comment could have been inserted. Record that, so we
                    // can suggest this place in case an invalid comment is
                    // encountered.
                    let anchor = self.peek_span();
                    self.comment_anchor = Span::new(self.doc, anchor.start(), anchor.start());
                    return result.into_boxed_slice();
                }
            }
        }
    }

    /// Skip over any blank line tokens.
    fn skip_blanks(&mut self) {
        while self.peek() == Token::Blank {
            self.consume();
        }
    }

    /// Skip over any non-code tokens.
    fn skip_non_code(&mut self) -> Result<()> {
        loop {
            match self.peek() {
                Token::Blank => {
                    self.consume();
                }
                Token::LineComment => {
                    return self
                        .error("A comment is not allowed here.")
                        .with_note(
                            self.comment_anchor,
                            "Try inserting the comment above this instead.",
                        )
                        .err();
                }
                Token::Shebang => {
                    return self
                        .error("A #!-line is not allowed here.")
                        .with_help("Try moving it to the top of the file instead.")
                        .err();
                }
                _ => return Ok(()),
            }
        }
    }

    /// Expect an identifier.
    fn parse_ident(&mut self) -> Result<Span> {
        match self.peek() {
            Token::Ident => Ok(self.consume()),
            _ => self.error("Expected an identifier here.").err(),
        }
    }

    /// Consume the given token, report an error otherwise.
    fn parse_token(&mut self, expected: Token, error: &'static str) -> Result<Span> {
        match self.peek() {
            token if token == expected => Ok(self.consume()),
            _ => self.error(error).err(),
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
            token if token == expected => Ok(self.consume()),
            _ => self.error(error).with_note(note_span, note).err(),
        }
    }

    /// Parse a top-level expression, which may start with a list of statements.
    fn parse_expr(&mut self) -> Result<(Span, Expr)> {
        // Increase the depth once, this depth applies to all statements
        // and also the expression.
        self.increase_depth()?;

        let mut statements = Vec::new();
        loop {
            let prefix = self.parse_non_code();
            let begin = self.peek_span();

            match self.peek() {
                Token::KwAssert | Token::KwLet | Token::KwTrace => {
                    let stmt = self.parse_stmt()?;
                    let prefixed = Prefixed {
                        prefix,
                        inner: stmt,
                    };
                    let span = self.span_from(begin);
                    statements.push((span, prefixed));
                }
                _ => {
                    let expr = self.parse_expr_no_stmt()?;
                    let span = self.span_from(begin);
                    self.decrease_depth();

                    // Do not make the CST deeper than it needs to be. If there
                    // are no statements, there is no need for a wrapping node.
                    if statements.is_empty() && prefix.is_empty() {
                        return Ok((span, expr));
                    }

                    let expr = Expr::Statements {
                        stmts: statements,
                        body_span: span,
                        body: Box::new(Prefixed {
                            prefix,
                            inner: expr,
                        }),
                    };

                    // We have a choice of what span to return here.
                    // What is the span for an expression preceded by statements?
                    // Does it include the statements or not? Let's say for now
                    // it does not, because the entire expression evaluates to
                    // its body anyway, so that is the span that matters. If it
                    // leads to confusing errors, we can re-evaluate this.
                    return Ok((span, expr));
                }
            }
        }
    }

    /// Parse an expression that is known to not be a statement.
    fn parse_expr_no_stmt(&mut self) -> Result<Expr> {
        match self.peek() {
            Token::KwIf => self.parse_expr_if(),
            _ => Ok(self.parse_expr_op()?.1),
        }
    }

    fn parse_expr_if(&mut self) -> Result<Expr> {
        // Consume the `if` keyword.
        let if_span = self.consume();

        // We do not allow non-code between the if and the condition, and for
        // the condition, we do not allow if or statements. There is no technical
        // need to limit this, we could use `parse_expr`, but the resulting CST
        // is a royal pain to format in a pleasant way. What to do with blank
        // lines, how do you indent? And `if if` looks confusing anyway. If you
        // want an expr there you can still do it, just put parens around it.
        self.skip_non_code()?;
        let (condition_span, condition) = self.parse_expr_op()?;

        self.skip_non_code()?;
        self.parse_token(Token::Colon, "Expected ':' after the condition.")?;
        let (then_span, then_body) = self.parse_expr()?;

        self.skip_non_code()?;
        self.parse_token_with_note(
            Token::KwElse,
            "Expected 'else' here.",
            if_span,
            "To match this 'if'.",
        )?;

        // Allow a colon directly after `else` (with no tokens in between), but
        // do not demand it. I was ambivalent about it in the past, up to RCL
        // 0.5.0 the syntax was to omit the colon, but I think it makes more
        // sense to have it. It should become mandatory in some future release,
        // but for now it can be optional. TODO: Make it mandatory.
        if self.peek() == Token::Colon {
            self.consume();
        }

        let (else_span, else_body) = self.parse_expr()?;

        let result = Expr::IfThenElse {
            condition_span,
            condition: Box::new(condition),
            then_span,
            then_body: Box::new(then_body),
            else_span,
            else_body: Box::new(else_body),
        };
        Ok(result)
    }

    fn parse_expr_import(&mut self) -> Result<(Span, Expr)> {
        // Consume the `import` keyword.
        let import_span = self.consume();
        let (path_span, path) = self.parse_expr()?;
        let result = Expr::Import {
            path_span,
            path: Box::new(path),
        };
        Ok((import_span.union(path_span), result))
    }

    /// Parse the statement under the cursor.
    #[inline]
    fn parse_stmt(&mut self) -> Result<Stmt> {
        match self.peek() {
            Token::KwAssert => self.parse_stmt_assert(),
            Token::KwLet => self.parse_stmt_let(),
            Token::KwTrace => self.parse_stmt_trace(),
            _ => panic!("Should only be called at 'assert', 'let', or 'trace'."),
        }
    }

    fn parse_stmt_assert(&mut self) -> Result<Stmt> {
        // Consume the `assert` keyword.
        let assert_span = self.consume();

        self.skip_non_code()?;
        let (condition_span, condition) = self.parse_expr()?;

        // After the condition is a colon, but if the user wrote a semicolon,
        // then explain that the message is not optional (unlike in Python).
        // For historical reasons, we also allow a comma instead of a colon:
        // up to version 0.10.0, RCL used a comma like Python.
        // TODO: Remove this compatibility, and make the comma an error. We can
        // keep a friendly error to point Pythonistas in the right direction.
        // TODO: Be sure to update the Tree-sitter grammar when changing this.
        self.skip_non_code()?;
        match self.peek() {
            Token::Colon => self.consume(),
            Token::Comma => self.consume(),
            Token::Semicolon => {
                return self
                    .error("Expected ':' here between the assertion condition and message.")
                    .with_help(
                        "An assertion has the form 'assert <condition>: <message>;'. \
                        The message is not optional.",
                    )
                    .err();
            }
            _ => {
                return self
                    .error("Expected ':' here between the assertion condition and message.")
                    .err()
            }
        };

        self.skip_non_code()?;
        let (message_span, message) = self.parse_expr()?;

        self.skip_non_code()?;
        self.parse_token_with_note(
            Token::Semicolon,
            "Expected ';' here to close the assertion.",
            assert_span,
            "Assertion opened here.",
        )?;

        let result = Stmt::Assert {
            condition_span,
            condition: Box::new(condition),
            message_span,
            message: Box::new(message),
        };

        Ok(result)
    }

    fn parse_stmt_let(&mut self) -> Result<Stmt> {
        // Consume the `let` keyword.
        let let_ = self.consume();

        self.skip_non_code()?;
        let ident = self.parse_ident()?;

        // Parse the optional type signature, and then the '='.
        self.skip_non_code()?;
        let type_: Option<Box<Type>> = match self.peek() {
            Token::Colon => {
                self.consume();
                self.skip_non_code()?;
                let type_ = self.parse_type_expr()?;
                // After the type annotation, only `=` is valid, but if we see
                // something that looks like it might be part of a function
                // type, educate the user about how to do that.
                match self.peek() {
                    Token::Eq1 => self.consume(),
                    Token::FatArrow => {
                        return self
                            .error("Expected '=' after type annotation.")
                            .with_help(
                                "Function types require parentheses \
                                and use '->' instead of '=>', e.g. '(String) -> Bool'.",
                            )
                            .err();
                    }
                    Token::ThinArrow => {
                        return self
                            .error("Expected '=' after type annotation.")
                            .with_help(
                                "Function types require parentheses, e.g. '(String) -> Bool'.",
                            )
                            .err();
                    }
                    _ => return self.error("Expected '=' after type annotation.").err(),
                };
                Some(Box::new(type_))
            }
            Token::Eq1 => {
                self.consume();
                None
            }
            _ => return self.error("Expected '=' or ':' here.").err(),
        };

        self.skip_non_code()?;
        let (value_span, value) = self.parse_expr()?;

        self.skip_non_code()?;
        self.parse_token_with_note(
            Token::Semicolon,
            "Expected ';' here to close the let-binding.",
            let_,
            "Let-binding opened here.",
        )?;

        let result = Stmt::Let {
            ident,
            type_,
            value_span,
            value: Box::new(value),
        };

        Ok(result)
    }

    fn parse_stmt_trace(&mut self) -> Result<Stmt> {
        // Consume the `trace` keyword.
        let trace_span = self.consume();

        self.skip_non_code()?;
        let (message_span, message) = self.parse_expr()?;

        self.skip_non_code()?;
        self.parse_token_with_note(
            Token::Semicolon,
            "Expected ';' here to close the trace expression.",
            trace_span,
            "Trace opened here.",
        )?;

        let result = Stmt::Trace {
            message_span,
            message: Box::new(message),
        };

        Ok(result)
    }

    /// Return an error with hint if there is a known bad unary operator under the cursor.
    fn check_bad_unop(&self) -> Result<()> {
        if self.peek() == Token::Bang {
            return self
                .error("Invalid operator. Negation is written with keyword 'not' instead of '!'.")
                .err();
        }
        Ok(())
    }

    /// Check if we should parse a function (`true`) or a different expression.
    ///
    /// There is an ambiguity in e.g. `(x)`, which could be an identifier in
    /// parens as an expression, or it could be the argument list of a lambda,
    /// and we don't know that until we see the token after it. So look ahead
    /// until we either see a `=>` and we know it's a lambda, or until we see
    /// some violation and we know it's not a lambda.
    ///
    /// TODO: This is getting complex. I should consider using a prefix token
    /// to disambiguate lambdas after all.
    ///
    /// TODO II: A better way to handle this: when closing a matching ), write
    /// to a side buffer the index of the token next to the opening (. That way,
    /// when we have a (, we can jump ahead to the closing ), and peek what
    /// comes after that closing ).
    fn look_ahead_is_function(&mut self) -> bool {
        let mut offset = 0;
        match self.peek() {
            Token::Ident => offset = 1,
            Token::LParen => {
                // Find the next closing paren, and continue parsing from there.
                // We don't have to be exact here, because this is only used to
                // look ahead to see if we should parse a lambda or expr. We
                // don't consider unbalanced parens, and we also don't return
                // false early even if we see a token that would be invalid for
                // a lambda. We do this to get more helpful errors, e.g. if you
                // write `(x, [y]) => x + y`, then it still looks like the
                // intent was a lambda and we can error on the `[`, rather than
                // trying to parse an expression and failing on the `,`.
                for i in 1.. {
                    match self.peek_n(i) {
                        Token::RParen => {
                            offset = i + 1;
                            break;
                        }
                        Token::Eof => unreachable!("The lexer returns balanced parens."),
                        _ => continue,
                    }
                }
            }
            _ => return false,
        };
        for i in offset.. {
            match self.peek_n(i) {
                Token::LineComment => continue,
                Token::Blank => continue,
                Token::FatArrow => return true,
                _ => return false,
            }
        }
        unreachable!("We'd run out of input before the loop ends.")
    }

    /// Try parsing a lambda function expression.
    fn parse_expr_function(&mut self) -> Result<(Span, Expr)> {
        let begin = self.peek_span();
        let args = match self.peek() {
            Token::Ident => {
                let prefixed = Prefixed {
                    prefix: [].into(),
                    inner: self.consume(),
                };
                List {
                    elements: [prefixed].into(),
                    suffix: [].into(),
                    trailing_comma: false,
                }
            }
            Token::LParen => {
                self.push_bracket()?;
                let args = self.parse_function_args()?;
                self.pop_bracket()?;
                args
            }
            _ => panic!("Should only call `parse_expr_function` on a lambda."),
        };

        self.skip_non_code()?;
        self.parse_token(Token::FatArrow, "Expected '=>' here.")?;
        let (body_span, body) = self.parse_expr()?;

        let result = Expr::Function {
            args,
            body_span,
            body: Box::new(body),
        };
        Ok((self.span_from(begin), result))
    }

    fn parse_expr_op(&mut self) -> Result<(Span, Expr)> {
        // First we check all the rules for prefix unary operators.
        self.check_bad_unop()?;

        if to_unop(self.peek()).is_some() {
            return self.parse_expr_unop();
        }

        // Instead of an operator chain, it could still be an import or lambda,
        // and those cannot be followed by operators, they return here.
        if self.look_ahead_is_function() {
            return self.parse_expr_function();
        }
        if self.peek() == Token::KwImport {
            return self.parse_expr_import();
        }

        let (mut lhs_span, mut result) = self.parse_expr_not_op()?;

        // We might have binary operators following. If we find one, then
        // all the other ones must be of the same type, to avoid unclear
        // situations like whether "a and b or c" means "(a and b) or c"
        // or "a and (b or c)".
        let mut allowed_op = None;
        let mut allowed_span = None;
        loop {
            match to_binop(self.peek_past_non_code()) {
                Some(op) if allowed_op.is_none() || allowed_op == Some(op) => {
                    self.skip_non_code()?;
                    let span = self.consume();
                    self.skip_non_code()?;
                    self.check_bad_unop()?;

                    // In a chain of binary operators, if we have an unop, it
                    // may only occur in the final right-hand side, not in the
                    // middle of the chain. We don't need any special handling
                    // for that, because unops cannot contain binops, so if we
                    // end up trying to parse an unop here, if a binop follows,
                    // that would be reported from `parse_expr_unop`.
                    let (rhs_span, rhs) = match to_unop(self.peek()) {
                        Some(..) => self.parse_expr_unop()?,
                        None => self.parse_expr_not_op()?,
                    };

                    allowed_span = Some(span);
                    allowed_op = Some(op);
                    result = Expr::BinOp {
                        op,
                        op_span: span,
                        lhs_span,
                        lhs: Box::new(result),
                        rhs_span,
                        rhs: Box::new(rhs),
                    };
                    lhs_span = lhs_span.union(rhs_span);
                }
                Some(_op) => {
                    return self.error(
                        "Parentheses are needed to clarify the precedence of this operator.",
                    ).with_note(
                        allowed_span.expect("If we are here, allowed_span must be set."),
                        "Without parentheses, it is not clear whether this operator should take precedence.",
                    ).err();
                }
                _ => return Ok((lhs_span, result)),
            }
        }
    }

    fn parse_expr_unop(&mut self) -> Result<(Span, Expr)> {
        let op = to_unop(self.peek()).expect("Should only call this with unop under cursor.");
        let span = self.consume();
        self.skip_non_code()?;
        self.check_bad_unop()?;

        // Nested unary expressions are okay.
        let (body_span, body) = if to_unop(self.peek()).is_some() {
            self.parse_expr_unop()?
        } else {
            self.parse_expr_not_op()?
        };

        let result = Expr::UnOp {
            op_span: span,
            op,
            body_span,
            body: Box::new(body),
        };

        // Check if the expression is followed by a binary operator. This is
        // not allowed in the grammar on purpose to force parens to clarify
        // precedence, but if we don't check for it, then the resulting
        // parse error is confusing, about unexpected content after the end
        // of the expression/document.
        self.skip_non_code()?;
        if to_binop(self.peek()).is_some() {
            return self
                .error("Parentheses are needed to clarify the precedence of this operator.")
                .with_note(
                    span,
                    "Without parentheses, it is not clear whether this operator \
                    applies only to the left-hand side, or the full expression.",
                )
                .err();
        }

        let result_span = span.until(body_span);
        Ok((result_span, result))
    }

    fn parse_expr_not_op(&mut self) -> Result<(Span, Expr)> {
        let begin = self.peek_span();
        let base_expr = self.parse_expr_term()?;
        let mut inner_span;

        let mut chain = Vec::new();

        loop {
            inner_span = self.span_from(begin);
            match self.peek_past_non_code() {
                Token::LParen => {
                    self.skip_non_code()?;
                    let open = self.push_bracket()?;
                    let args = self.parse_call_args()?;
                    let close = self.pop_bracket()?;
                    let chain_expr = Chain::Call { open, close, args };
                    chain.push((inner_span, chain_expr));
                }
                Token::LBracket => {
                    self.skip_non_code()?;
                    let open = self.push_bracket()?;
                    let (index_span, index) = self.parse_expr()?;
                    let close = self.pop_bracket()?;
                    let chain_expr = Chain::Index {
                        open,
                        close,
                        index_span,
                        index: Box::new(index),
                    };
                    chain.push((inner_span, chain_expr));
                }
                Token::Dot => {
                    self.skip_non_code()?;
                    self.consume();
                    self.skip_non_code()?;
                    let field = self.parse_token(Token::Ident, "Expected an identifier here.")?;
                    let chain_expr = Chain::Field { field };
                    chain.push((inner_span, chain_expr));
                }
                _ => {
                    // If it's not any of those cases, then the chain ends here.
                    // If we have no chained expressions then keep the CST small,
                    // if we have then we wrap it into a Chain node.
                    if chain.is_empty() {
                        return Ok((inner_span, base_expr));
                    } else {
                        let expr = Expr::Chain {
                            base_expr: Box::new(base_expr),
                            chain,
                        };
                        return Ok((inner_span, expr));
                    }
                }
            }
        }
    }

    fn parse_expr_term(&mut self) -> Result<Expr> {
        match self.peek() {
            Token::LBrace => {
                let open = self.push_bracket()?;
                let elements = self.parse_seqs()?;
                let close = self.pop_bracket()?;
                let result = Expr::BraceLit {
                    open,
                    close,
                    elements,
                };
                Ok(result)
            }
            Token::LBracket => {
                let open = self.push_bracket()?;
                let elements = self.parse_seqs()?;
                let close = self.pop_bracket()?;
                let result = Expr::BracketLit {
                    open,
                    close,
                    elements,
                };
                Ok(result)
            }
            Token::LParen => {
                let open = self.push_bracket()?;
                let (body_span, body) = self.parse_expr()?;
                let close = self.pop_bracket()?;
                let result = Expr::Parens {
                    open,
                    close,
                    body_span,
                    body: Box::new(body),
                };
                Ok(result)
            }
            Token::QuoteOpen(prefix, style) => self.parse_string(prefix, style),
            Token::KwNull => Ok(Expr::NullLit(self.consume())),
            Token::KwTrue => Ok(Expr::BoolLit(self.consume(), true)),
            Token::KwFalse => Ok(Expr::BoolLit(self.consume(), false)),
            Token::NumHexadecimal => Ok(Expr::NumHexadecimal(self.consume())),
            Token::NumBinary => Ok(Expr::NumBinary(self.consume())),
            Token::NumDecimal => Ok(Expr::NumDecimal(self.consume())),
            Token::Ident => Ok(Expr::Var(self.consume())),

            // Some tokens are valid starts of an expression, but just not at
            // the term level. For those, we can recommend the user to wrap
            // everything in parens, because then it would be allowed.
            Token::KwLet | Token::KwAssert | Token::KwTrace | Token::KwIf => self
                .error("Expected a term here.")
                .with_help("If this should be an expression, try wrapping it in parentheses.")
                .err(),

            // The parser jargon is that we expect a *term* here, which is more
            // narrow than "expression", but users should not need to know the
            // names of every AST node to make sense of error messages. (Looking
            // at you PHP, T_PAAMAYIM_NEKUDOTAYIM ...)
            _ => self.error("Expected an expression here.").err(),
        }
    }

    /// Consume a string inner span, ensuring that multiline strings start with a newline.
    ///
    /// If we allowed content between the opening quote and the first line break,
    /// the following string would be ambiguous:
    /// ```text
    /// let ambiguous = """  foo
    ///     bar""";
    /// let candidate1 = "  foo\n    bar";
    /// let candidate2 = "foo\n  bar";
    /// let candidate3 = "  foo\nbar";
    /// ```
    /// Which of the candidates do you expect the ambiguous string to be equal
    /// to? To avoid such confusion, we do not allow this.
    ///
    /// There is a form we *could* allow: strings with no line breaks at all.
    /// ```text
    /// """"It's too bad she won't live!", said Gaff."""
    /// ```
    /// would be unambiguous, and it may be nice to not have to escape the `"`.
    /// We might support this at a later time, but it makes handling of the
    /// string literals messy, so for now we enforce the line break.
    ///
    /// In addition, this method ensures that if two [`StringPart::String`] are
    /// consecutive, the second one starts with a line break. The lexer may break
    /// up the string into multiple tokens, but here we merge them again. It
    /// simplifies the formatter when it can assume that consecutive string
    /// parts are really separate lines.
    fn parse_string_inner(&mut self, style: QuoteStyle, into: &mut Vec<StringPart>) -> Result<()> {
        let inner = self.consume();
        let inner_str = inner.resolve(self.input);

        let is_new_line = !inner_str.is_empty() && inner_str.as_bytes()[0] == b'\n';

        if style == QuoteStyle::Triple && into.is_empty() && !is_new_line {
            return inner
                .error("Expected a line break after the \"\"\". Move this to the next line.")
                .err();
        }

        match into.last_mut() {
            Some(StringPart::String(span)) if !is_new_line => *span = span.union(inner),
            _ => into.push(StringPart::String(inner)),
        }

        Ok(())
    }

    fn parse_string(&mut self, prefix: StringPrefix, style: QuoteStyle) -> Result<Expr> {
        let open = self.consume();
        let mut parts = Vec::new();
        let mut has_hole = false;

        loop {
            match self.peek() {
                Token::StringInner => {
                    self.parse_string_inner(style, &mut parts)?;
                }
                Token::Escape(esc) => {
                    parts.push(StringPart::Escape(self.consume(), esc));
                }
                Token::HoleOpen => {
                    // Consume the opening `{`.
                    let hole_open = self.consume();
                    let (span, expr) = self.parse_expr()?;
                    parts.push(StringPart::Hole(span, expr));
                    self.parse_token_with_note(
                        Token::HoleClose,
                        "Expected '}' here to close format string hole.",
                        hole_open,
                        "Unmatched '{' opened here.",
                    )?;
                    has_hole = true;
                }
                Token::QuoteClose => {
                    let close = self.consume();
                    if prefix == StringPrefix::Format && !has_hole {
                        let span = open.union(close);
                        return span
                            .error("This format string has no holes, it can be a regular string.")
                            .err();
                    }
                    let result = Expr::StringLit {
                        prefix,
                        style,
                        open,
                        close,
                        parts,
                    };
                    return Ok(result);
                }
                invalid => panic!("The lexer should not have produced {invalid:?} in a string."),
            }
        }
    }

    /// Parse arguments in a lambda function definition.
    fn parse_function_args(&mut self) -> Result<List<Prefixed<Span>>> {
        let mut result = Vec::new();
        let mut trailing_comma = false;

        loop {
            let prefix = self.parse_non_code();
            if self.peek() == Token::RParen {
                let final_result = List {
                    elements: result.into_boxed_slice(),
                    suffix: prefix,
                    trailing_comma,
                };
                return Ok(final_result);
            }

            let ident = self.parse_ident()?;
            let prefixed = Prefixed {
                prefix,
                inner: ident,
            };
            result.push(prefixed);
            trailing_comma = false;

            self.skip_non_code()?;
            match self.peek() {
                Token::RParen => continue,
                Token::Comma => {
                    self.consume();
                    trailing_comma = true;
                    continue;
                }
                _ => {
                    // If we don't find a separator, nor the end of the args,
                    // that's an error. We can report an unmatched bracket
                    // as the problem, because it is.
                    self.pop_bracket()?;
                    unreachable!("pop_bracket should have failed.");
                }
            }
        }
    }

    /// Parse arguments in a function call.
    fn parse_call_args(&mut self) -> Result<List<(Span, Expr)>> {
        let mut result = Vec::new();
        let mut trailing_comma = false;

        loop {
            if self.peek_past_non_code() == Token::RParen {
                let suffix = self.parse_non_code();
                let final_result = List {
                    elements: result.into_boxed_slice(),
                    suffix,
                    trailing_comma,
                };
                return Ok(final_result);
            }

            result.push(self.parse_expr()?);
            trailing_comma = false;

            self.skip_non_code()?;
            match self.peek() {
                Token::RParen => continue,
                Token::Comma => {
                    self.consume();
                    trailing_comma = true;
                    continue;
                }
                _ => {
                    // If we don't find a separator, nor the end of the args,
                    // that's an error. We can report an unmatched bracket
                    // as the problem, because it is.
                    self.pop_bracket()?;
                    unreachable!("pop_bracket should have failed.");
                }
            }
        }
    }

    /// Parse sequence elements.
    ///
    /// This corresponds to `seqs` in the grammar, but it is slightly different
    /// from the rule there to be able to incorporate noncode.
    fn parse_seqs(&mut self) -> Result<List<Seq>> {
        let mut result = Vec::new();
        let mut trailing_comma = false;

        let mut prefix = self.parse_non_code();

        loop {
            if matches!(self.peek(), Token::RBrace | Token::RBracket) {
                let final_result = List {
                    elements: result.into_boxed_slice(),
                    suffix: prefix,
                    trailing_comma,
                };
                return Ok(final_result);
            }

            result.push(self.parse_seq(prefix)?);
            prefix = self.parse_non_code();
            trailing_comma = false;

            match self.peek() {
                Token::RBrace | Token::RBracket => continue,
                Token::Comma => {
                    self.consume();
                    trailing_comma = true;

                    // Any non-code after the comma is the prefix of the next
                    // seq. If we already had a prefix, then there is non-code
                    // between the previous seq and the comma. We shouldn't
                    // really allow that, but since we parsed it, it's too late
                    // to fail, so we'll move it over the comma instead.
                    if prefix.is_empty() {
                        prefix = self.parse_non_code();
                    } else {
                        let mut pfx = prefix.into_vec();
                        // When we concatenate two non-codes, we should not
                        // create two consecutive blanks, as that would create
                        // an idempotency issue in the formatter. We drop all
                        // blanks just before the comma.
                        while let Some(NonCode::Blank(..)) = pfx.last() {
                            pfx.pop();
                        }
                        pfx.extend(self.parse_non_code().into_vec());
                        prefix = pfx.into_boxed_slice();
                    }

                    continue;
                }
                // All of the next tokens are unexpected, but we add special
                // errors for them to help the user along.
                Token::Semicolon => {
                    return self.error("Expected ',' instead of ';' here.").err();
                }
                Token::KwElse => {
                    return self
                        .pop_bracket()
                        .expect_err("We are in a seq.")
                        .with_help(concat! {
                            "Inside a comprehension, '"
                            Doc::highlight("if")
                            "' controls the loop, there is no '" Doc::highlight("else") "' part."
                            Doc::Sep
                            "To use an if-else expression inside a comprehension, "
                            "enclose the expression in parentheses."
                        })
                        .err();
                }
                // If we don't find a separator, nor the end of the collection
                // literal, that's an error. We can report an unmatched bracket
                // as the problem, because it is. The pop will fail. If we see
                // an '=' maybe the user tried to make a key-value mapping and
                // we can report a better error.
                Token::Eq1 => {
                    return self
                        .pop_bracket()
                        .expect_err("We are in a seq.")
                        .with_help(concat! {
                            "To use '"
                            Doc::highlight("key = value")
                            "' record notation, the left-hand side must be an identifier."
                            Doc::Sep
                            "When that is not possible, use json-style '"
                            Doc::highlight("\"key\": value")
                            "' instead."
                        })
                        .err();
                }
                _ => {
                    self.pop_bracket()?;
                    unreachable!("pop_bracket should have failed.");
                }
            }
        }
    }

    /// Parse a single element (or comprehension) inside a collection literal.
    ///
    /// We attach the input prefix to the nearest place in the `Seq` where it
    /// belongs, which may be a control item, or the inner yield.
    fn parse_seq(&mut self, mut prefix: Box<[NonCode]>) -> Result<Seq> {
        let mut control_items = Vec::new();

        let body = loop {
            // Here we have a lookahead of two tokens ... not great if we want to
            // keep the grammar simple, but for making the syntax prettier it is
            // worth some complications to allow { a = b; p = q } notation.
            let next1 = self.peek();
            let next2 = self.peek_n(1);

            let control = match (next1, next2) {
                (Token::KwAssert | Token::KwLet | Token::KwTrace, _) => self.parse_seq_stmt()?,
                (Token::KwFor, _) => self.parse_seq_for()?,
                (Token::KwIf, _) => self.parse_seq_if()?,
                (Token::DotDot, _) => break self.parse_seq_unpack_elems()?,
                (Token::DotDotDot, _) => break self.parse_seq_unpack_assocs()?,
                (Token::Ident, Token::Eq1) => break self.parse_seq_assoc_ident()?,
                _ => break self.parse_seq_assoc_expr()?,
            };

            let item = Prefixed {
                prefix,
                inner: control,
            };
            control_items.push(item);
            prefix = self.parse_non_code();
        };

        let result = Seq {
            control: control_items.into_boxed_slice(),
            body: Prefixed {
                prefix,
                inner: body,
            },
        };

        Ok(result)
    }

    /// Parse `..xs` inside a `Seq`.
    fn parse_seq_unpack_elems(&mut self) -> Result<Yield> {
        let dotdot = self.consume();

        self.skip_non_code()?;
        let (collection_span, collection) = self.parse_expr()?;

        let result = Yield::UnpackElems {
            unpack_span: dotdot,
            collection_span,
            collection: Box::new(collection),
        };
        Ok(result)
    }

    /// Parse `...xs` inside a `Seq`.
    fn parse_seq_unpack_assocs(&mut self) -> Result<Yield> {
        let dotdotdot = self.consume();

        self.skip_non_code()?;
        let (collection_span, collection) = self.parse_expr()?;

        let result = Yield::UnpackAssocs {
            unpack_span: dotdotdot,
            collection_span,
            collection: Box::new(collection),
        };
        Ok(result)
    }

    /// Parse `ident = expr` inside a `Seq`.
    fn parse_seq_assoc_ident(&mut self) -> Result<Yield> {
        let ident = self.consume();

        self.skip_non_code()?;
        let op = self.parse_token(Token::Eq1, "Expected '=' here.")?;

        self.skip_non_code()?;
        let (value_span, value) = self.parse_expr()?;

        let result = Yield::AssocIdent {
            op_span: op,
            field: ident,
            value_span,
            value: Box::new(value),
        };

        Ok(result)
    }

    /// Parse `expr` or `expr: expr` inside a `Seq`.
    fn parse_seq_assoc_expr(&mut self) -> Result<Yield> {
        let (expr_span, expr) = self.parse_expr_op()?;
        let result = match self.peek_past_non_code() {
            Token::Colon => {
                self.skip_non_code()?;
                let op = self.consume();
                self.skip_non_code()?;
                let (value_span, value) = self.parse_expr()?;
                Yield::AssocExpr {
                    op_span: op,
                    field_span: expr_span,
                    field: Box::new(expr),
                    value_span,
                    value: Box::new(value),
                }
            }
            _ => Yield::Elem {
                span: expr_span,
                value: Box::new(expr),
            },
        };
        Ok(result)
    }

    fn parse_seq_stmt(&mut self) -> Result<SeqControl> {
        let stmt = self.parse_stmt()?;
        let result = SeqControl::Stmt { stmt };
        Ok(result)
    }

    fn parse_seq_for(&mut self) -> Result<SeqControl> {
        let _for = self.consume();

        // Parse the loop variables. Here a trailing comma is not allowed.
        let mut idents = Vec::new();
        loop {
            self.skip_non_code()?;
            let ident = self.parse_token(Token::Ident, "Expected identifier here.")?;
            idents.push(ident);

            self.skip_non_code()?;
            match self.peek() {
                Token::Comma => {
                    self.consume();
                    continue;
                }
                _ => break,
            }
        }

        self.skip_non_code()?;
        self.parse_token(Token::KwIn, "Expected 'in' here.")?;

        self.skip_non_code()?;
        let (collection_span, collection) = self.parse_expr_op()?;

        self.skip_non_code()?;
        self.parse_token(Token::Colon, "Expected ':' after the collection.")?;

        let result = SeqControl::For {
            idents: idents.into_boxed_slice(),
            collection_span,
            collection: Box::new(collection),
        };

        Ok(result)
    }

    fn parse_seq_if(&mut self) -> Result<SeqControl> {
        let _if = self.consume();

        // See also the note in `parse_expr_if` about why we don't allow
        // arbitrary expressions here.
        self.skip_non_code()?;
        let (condition_span, condition) = self.parse_expr_op()?;

        self.skip_non_code()?;
        self.parse_token(Token::Colon, "Expected ':' after the condition.")?;

        let result = SeqControl::If {
            condition_span,
            condition: Box::new(condition),
        };

        Ok(result)
    }

    /// Parse a type expression.
    fn parse_type_expr(&mut self) -> Result<Type> {
        // If it starts with a `(`, then that is the start of an argument list,
        // and we are parsing a function type.
        if self.peek() == Token::LParen {
            return self.parse_type_function();
        }

        // Otherwise, we definitely start with a term.
        let begin = self.peek_span();
        let term = self.parse_type_term()?;

        // Optionally, the term can be followed by `[` to instantiate a generic
        // type.
        self.skip_non_code()?;
        if let (Type::Term(name), Token::LBracket) = (&term, self.peek()) {
            self.push_bracket()?;
            let args = self.parse_types()?;
            self.pop_bracket()?;

            let type_apply = Type::Apply {
                span: self.span_from(begin),
                name: *name,
                args,
            };
            return Ok(type_apply);
        }

        // When it's not followed by a `[`, then this is a regular term.
        Ok(term)
    }

    /// Parse a function type that starts with a `(`.
    fn parse_type_function(&mut self) -> Result<Type> {
        let begin = self.peek_span();
        self.push_bracket()?;
        let args = self.parse_types()?;
        self.pop_bracket()?;
        self.skip_non_code()?;
        self.parse_token(Token::ThinArrow, "Expected '->' here in function type.")?;
        let result_type = self.parse_type_expr()?;
        let fn_type = Type::Function {
            span: self.span_from(begin),
            args,
            result: Box::new(result_type),
        };
        Ok(fn_type)
    }

    /// Parse a comma-delimited list of types with optional trailing comma.
    fn parse_types(&mut self) -> Result<List<Prefixed<Type>>> {
        let mut result = Vec::new();
        let mut trailing_comma = false;

        loop {
            let prefix = self.parse_non_code();
            if matches!(self.peek(), Token::RParen | Token::RBracket) {
                let final_result = List {
                    elements: result.into_boxed_slice(),
                    suffix: prefix,
                    trailing_comma,
                };
                return Ok(final_result);
            }

            let type_ = self.parse_type_expr()?;
            let prefixed = Prefixed {
                prefix,
                inner: type_,
            };
            result.push(prefixed);
            trailing_comma = false;

            self.skip_non_code()?;
            match self.peek() {
                Token::RParen | Token::RBracket => continue,
                Token::Comma => {
                    self.consume();
                    trailing_comma = true;
                    continue;
                }
                _ => {
                    // If we don't find a separator, nor the end of the list,
                    // that's an error. We can report an unmatched bracket
                    // as the problem, because it is.
                    self.pop_bracket()?;
                    unreachable!("pop_bracket should have failed.");
                }
            }
        }
    }

    fn parse_type_term(&mut self) -> Result<Type> {
        match self.peek() {
            Token::Ident => {
                let span = self.consume();
                Ok(Type::Term(span))
            }
            // TODO: Consider string literals as type terms too.
            _ => self.error("Expected a type here.").err(),
        }
    }

    /// Confirm that there is no trailing content left to parse.
    fn parse_eof(&mut self) -> Result<()> {
        self.skip_non_code()?;
        if self.peek() != Token::Eof {
            return self
                .error("Unexpected content after the main expression.")
                .err();
        }
        Ok(())
    }
}
