// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The parser converts a sequence of tokens into a Concrete Syntax Tree.

use crate::cst::{
    BinOp, Chain, Expr, NonCode, Prefixed, Seq, SpanPrefixedExpr, Stmt, StringPart, Type, UnOp,
};
use crate::error::{Error, IntoError, Result};
use crate::lexer::{Lexeme, QuoteStyle, StringPrefix, Token};
use crate::pprint::{concat, Doc};
use crate::source::{DocId, Span};

/// A collection of `T`s, and a non-code suffix.
type SuffixedElems<T> = (Box<[T]>, Box<[NonCode]>);

/// Parse an input document into a concrete syntax tree.
pub fn parse(doc: DocId, input: &str, tokens: &[Lexeme]) -> Result<SpanPrefixedExpr> {
    let mut parser = Parser::new(doc, input, tokens);

    // Comments at the start of the document are allowed, but the document
    // should not start with blank lines, those we drop.
    parser.skip_blanks();

    let (span, result) = parser.parse_prefixed_expr()?;
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

    /// Return the token under the cursor, if there is one.
    fn peek(&self) -> Option<Token> {
        // TODO: Peek should ignore whitespace and comments for most cases,
        // probably it should be the default.
        self.peek_n(0)
    }

    /// Return the token `offset` tokens after the cursor, if there is one.
    fn peek_n(&self, offset: usize) -> Option<Token> {
        self.tokens.get(self.cursor + offset).map(|t| t.0)
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
    fn parse_non_code(&mut self) -> Box<[NonCode]> {
        let mut result = Vec::new();

        loop {
            match self.peek() {
                Some(Token::LineComment) => result.push(NonCode::LineComment(self.consume())),
                Some(Token::Shebang) => result.push(NonCode::Shebang(self.consume())),
                Some(Token::Blank) => result.push(NonCode::Blank(self.consume())),
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
        while let Some(Token::Blank) = self.peek() {
            self.consume();
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
                    return self
                        .error("A comment is not allowed here.")
                        .with_note(
                            self.comment_anchor,
                            "Try inserting the comment above this instead.",
                        )
                        .err();
                }
                _ => return Ok(()),
            }
        }
    }

    /// Expect an identifier.
    fn parse_ident(&mut self) -> Result<Span> {
        match self.peek() {
            Some(Token::Ident) => Ok(self.consume()),
            _ => self.error("Expected an identifier here.").err(),
        }
    }

    /// Consume the given token, report an error otherwise.
    fn parse_token(&mut self, expected: Token, error: &'static str) -> Result<Span> {
        match self.peek() {
            Some(token) if token == expected => Ok(self.consume()),
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
            Some(token) if token == expected => Ok(self.consume()),
            _ => self.error(error).with_note(note_span, note).err(),
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

    pub fn parse_prefixed_expr(&mut self) -> Result<SpanPrefixedExpr> {
        let pf = self.parse_prefixed(|s| s.parse_expr())?;
        Ok((
            pf.inner.0,
            Prefixed {
                prefix: pf.prefix,
                inner: pf.inner.1,
            },
        ))
    }

    fn parse_expr(&mut self) -> Result<(Span, Expr)> {
        let begin = self.peek_span();
        // TODO: This depth limit is not sustainable if my lets are recursive.
        // We need tail calls or loops to handle them in the parser ...
        self.increase_depth()?;
        let result = match self.peek() {
            Some(Token::KwAssert | Token::KwLet | Token::KwTrace) => {
                let stmt = self.parse_stmt()?;
                let (body_span, body) = self.parse_prefixed_expr()?;
                Expr::Stmt {
                    stmt,
                    body_span,
                    body: Box::new(body),
                }
            }
            Some(Token::KwIf) => self.parse_expr_if()?,
            _ => self.parse_expr_op()?,
        };
        self.decrease_depth();

        Ok((self.span_from(begin), result))
    }

    fn parse_expr_if(&mut self) -> Result<Expr> {
        // Consume the `if` keyword.
        let if_span = self.consume();
        self.skip_non_code()?;
        let (condition_span, condition) = self.parse_expr()?;

        self.skip_non_code()?;
        self.parse_token(Token::Colon, "Expected ':' after the condition.")?;
        let (then_span, then_body) = self.parse_prefixed_expr()?;

        self.skip_non_code()?;
        self.parse_token_with_note(
            Token::KwElse,
            "Expected 'else' here.",
            if_span,
            "To match this 'if'.",
        )?;

        // If a user wrote `else:`, add friendly error to clarify that there
        // shouldn't be a colon. For symmetry with `if cond:`, one might expect
        // the colon to be there.
        if let Some(Token::Colon) = self.peek() {
            return self
                .consume()
                .error("Expected an expression after 'else'.")
                .with_help("In an if-else expression, there is no ':' after 'else'.")
                .err();
        }

        let (else_span, else_body) = self.parse_prefixed_expr()?;

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

    fn parse_expr_import(&mut self) -> Result<Expr> {
        // Consume the `import` keyword.
        let _import_span = self.consume();
        let (path_span, path) = self.parse_prefixed_expr()?;
        let result = Expr::Import {
            path_span,
            path: Box::new(path),
        };
        Ok(result)
    }

    /// Parse the statement under the cursor.
    #[inline]
    fn parse_stmt(&mut self) -> Result<Stmt> {
        match self.peek() {
            Some(Token::KwAssert) => self.parse_stmt_assert(),
            Some(Token::KwLet) => self.parse_stmt_let(),
            Some(Token::KwTrace) => self.parse_stmt_trace(),
            _ => panic!("Should only be called at 'assert', 'let', or 'trace'."),
        }
    }

    fn parse_stmt_assert(&mut self) -> Result<Stmt> {
        // Consume the `assert` keyword.
        let assert_span = self.consume();

        self.skip_non_code()?;
        let (condition_span, condition) = self.parse_expr()?;

        // After the condition is a comma, but if the user wrote a semicolon,
        // then explain that the message is not optional (unlike in Python).
        self.skip_non_code()?;
        match self.peek() {
            Some(Token::Comma) => self.consume(),
            Some(Token::Semicolon) => {
                return self
                    .error("Expected ',' here between the assertion condition and message.")
                    .with_help(
                        "An assertion has the form 'assert <condition>, <message>;'. \
                        The message is not optional.",
                    )
                    .err();
            }
            _ => {
                return self
                    .error("Expected ',' here between the assertion condition and message.")
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
            Some(Token::Colon) => {
                self.consume();
                self.skip_non_code()?;
                let type_ = self.parse_type_expr()?;
                // After the type annotation, only `=` is valid, but if we see
                // something that looks like it might be part of a function
                // type, educate the user about how to do that.
                match self.peek() {
                    Some(Token::Eq1) => self.consume(),
                    Some(Token::FatArrow) => {
                        return self
                            .error("Expected '=' after type annotation.")
                            .with_help(
                                "Function types require parentheses \
                                and use '->' instead of '=>', e.g. '(Int) -> Bool'.",
                            )
                            .err();
                    }
                    Some(Token::ThinArrow) => {
                        return self
                            .error("Expected '=' after type annotation.")
                            .with_help("Function types require parentheses, e.g. '(Int) -> Bool'.")
                            .err();
                    }
                    _ => return self.error("Expected '=' after type annotation.").err(),
                };
                Some(Box::new(type_))
            }
            Some(Token::Eq1) => {
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
        if let Some(Token::Bang) = self.peek() {
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
            Some(Token::Ident) => offset = 1,
            Some(Token::LParen) => {
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
                        Some(Token::RParen) => {
                            offset = i + 1;
                            break;
                        }
                        None => unreachable!("The lexer returns balanced parens."),
                        _ => continue,
                    }
                }
            }
            _ => return false,
        };
        for i in offset.. {
            match self.peek_n(i) {
                Some(Token::LineComment) => continue,
                Some(Token::Blank) => continue,
                Some(Token::FatArrow) => return true,
                _ => return false,
            }
        }
        unreachable!("We'd run out of input before the loop ends.")
    }

    /// Try parsing a lambda function expression.
    fn parse_expr_function(&mut self) -> Result<Expr> {
        let (args, suffix) = match self.peek() {
            Some(Token::Ident) => {
                let prefixed = Prefixed {
                    prefix: [].into(),
                    inner: self.consume(),
                };
                ([prefixed].into(), [].into())
            }
            Some(Token::LParen) => {
                self.push_bracket()?;
                let (args, suffix) = self.parse_function_args()?;
                self.pop_bracket()?;
                (args, suffix)
            }
            _ => panic!("Should only call `parse_expr_function` on a lambda."),
        };

        self.skip_non_code()?;
        self.parse_token(Token::FatArrow, "Expected '=>' here.")?;
        self.skip_non_code()?;
        let (body_span, body) = self.parse_expr()?;

        let result = Expr::Function {
            args,
            suffix,
            body_span,
            body: Box::new(body),
        };
        Ok(result)
    }

    fn parse_expr_op(&mut self) -> Result<Expr> {
        // First we check all the rules for prefix unary operators.
        self.check_bad_unop()?;

        if self.peek().and_then(to_unop).is_some() {
            let (_span, result) = self.parse_expr_unop()?;
            return Ok(result);
        }

        // Instead of an operator chain, it could still be an import or lambda,
        // and those cannot be followed by operators, they return here.
        if self.look_ahead_is_function() {
            return self.parse_expr_function();
        }
        if self.peek() == Some(Token::KwImport) {
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
            self.skip_non_code()?;
            match self.peek().and_then(to_binop) {
                Some(op) if allowed_op.is_none() || allowed_op == Some(op) => {
                    let span = self.consume();
                    self.skip_non_code()?;
                    let (rhs_span, rhs) = self.parse_expr_not_op()?;
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
                _ => return Ok(result),
            }
        }
    }

    fn parse_expr_unop(&mut self) -> Result<(Span, Expr)> {
        let op = self
            .peek()
            .and_then(to_unop)
            .expect("Should only call this with unop under cursor.");
        let span = self.consume();
        self.skip_non_code()?;
        self.check_bad_unop()?;

        // Nested unary expressions are okay.
        let (body_span, body) = if self.peek().and_then(to_unop).is_some() {
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
        if self.peek().and_then(to_binop).is_some() {
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
        // TODO: check for operators before, and report a pretty error
        // to clarify that parens must be used to disambiguate.

        let begin = self.peek_span();
        let base_expr = self.parse_expr_term()?;
        let mut inner_span;

        let mut chain = Vec::new();

        loop {
            inner_span = self.span_from(begin);
            self.skip_non_code()?;
            match self.peek() {
                Some(Token::LParen) => {
                    let open = self.push_bracket()?;
                    let (args, suffix) = self.parse_call_args()?;
                    let close = self.pop_bracket()?;
                    let chain_expr = Chain::Call {
                        open,
                        close,
                        args,
                        suffix,
                    };
                    chain.push((inner_span, chain_expr));
                }
                Some(Token::LBracket) => {
                    let open = self.push_bracket()?;
                    let (index_span, index) = self.parse_prefixed_expr()?;
                    let close = self.pop_bracket()?;
                    let chain_expr = Chain::Index {
                        open,
                        close,
                        index_span,
                        index: Box::new(index),
                    };
                    chain.push((inner_span, chain_expr));
                }
                Some(Token::Dot) => {
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
            Some(Token::LBrace) => {
                let open = self.push_bracket()?;
                let (elements, suffix) = self.parse_seqs()?;
                let close = self.pop_bracket()?;
                let result = Expr::BraceLit {
                    open,
                    close,
                    elements,
                    suffix,
                };
                Ok(result)
            }
            Some(Token::LBracket) => {
                let open = self.push_bracket()?;
                let (elements, suffix) = self.parse_seqs()?;
                let close = self.pop_bracket()?;
                let result = Expr::BracketLit {
                    open,
                    close,
                    elements,
                    suffix,
                };
                Ok(result)
            }
            Some(Token::LParen) => {
                let open = self.push_bracket()?;
                let (body_span, body) = self.parse_prefixed_expr()?;
                let close = self.pop_bracket()?;
                let result = Expr::Parens {
                    open,
                    close,
                    body_span,
                    body: Box::new(body),
                };
                Ok(result)
            }
            Some(Token::QuoteOpen(prefix, style)) => self.parse_string(prefix, style),
            Some(Token::KwNull) => Ok(Expr::NullLit(self.consume())),
            Some(Token::KwTrue) => Ok(Expr::BoolLit(self.consume(), true)),
            Some(Token::KwFalse) => Ok(Expr::BoolLit(self.consume(), false)),
            Some(Token::NumHexadecimal) => Ok(Expr::NumHexadecimal(self.consume())),
            Some(Token::NumBinary) => Ok(Expr::NumBinary(self.consume())),
            Some(Token::NumDecimal) => Ok(Expr::NumDecimal(self.consume())),
            Some(Token::Ident) => Ok(Expr::Var(self.consume())),
            _ => self.error("Expected a term here.").err(),
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
                Some(Token::StringInner) => {
                    self.parse_string_inner(style, &mut parts)?;
                }
                Some(Token::Escape(esc)) => {
                    parts.push(StringPart::Escape(self.consume(), esc));
                }
                Some(Token::HoleOpen) => {
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
                Some(Token::QuoteClose) => {
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
    fn parse_function_args(&mut self) -> Result<SuffixedElems<Prefixed<Span>>> {
        let mut result = Vec::new();

        loop {
            let prefix = self.parse_non_code();
            if self.peek() == Some(Token::RParen) {
                return Ok((result.into_boxed_slice(), prefix));
            }

            let ident = self.parse_ident()?;
            let prefixed = Prefixed {
                prefix,
                inner: ident,
            };
            result.push(prefixed);

            self.skip_non_code()?;
            match self.peek() {
                Some(Token::RParen) => continue,
                Some(Token::Comma) => {
                    self.consume();
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
    fn parse_call_args(&mut self) -> Result<SuffixedElems<SpanPrefixedExpr>> {
        let mut result = Vec::new();

        loop {
            let prefix = self.parse_non_code();
            if self.peek() == Some(Token::RParen) {
                return Ok((result.into_boxed_slice(), prefix));
            }

            let (span, expr) = self.parse_expr()?;
            let prefixed = Prefixed {
                prefix,
                inner: expr,
            };
            result.push((span, prefixed));

            self.skip_non_code()?;
            match self.peek() {
                Some(Token::RParen) => continue,
                Some(Token::Comma) => {
                    self.consume();
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
    fn parse_seqs(&mut self) -> Result<SuffixedElems<Prefixed<Seq>>> {
        let mut result = Vec::new();

        loop {
            let prefix = self.parse_non_code();
            if matches!(self.peek(), Some(Token::RBrace | Token::RBracket)) {
                return Ok((result.into_boxed_slice(), prefix));
            }

            let (_span, seq) = self.parse_seq()?;

            let prefixed = Prefixed { prefix, inner: seq };
            result.push(prefixed);

            self.skip_non_code()?;
            match self.peek() {
                Some(Token::RBrace | Token::RBracket) => continue,
                tok if tok == Some(Token::Comma) => {
                    self.consume();
                    continue;
                }
                // All of the next tokens are unexpected, but we add special
                // errors for them to help the user along.
                Some(Token::Semicolon) => {
                    return self.error("Expected ',' instead of ';' here.").err();
                }
                Some(Token::KwElse) => {
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
                Some(Token::Eq1) => {
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

    pub fn parse_prefixed_seq(&mut self) -> Result<(Span, Prefixed<Seq>)> {
        let ps = self.parse_prefixed(|s| s.parse_seq())?;
        Ok((
            ps.inner.0,
            Prefixed {
                prefix: ps.prefix,
                inner: ps.inner.1,
            },
        ))
    }

    fn parse_seq(&mut self) -> Result<(Span, Seq)> {
        let begin = self.peek_span();

        // Here we have a lookahead of two tokens ... not great if we want to
        // keep the grammar simple, but for making the syntax prettier it is
        // worth some complications to allow { a = b; p = q } notation.
        let next1 = self.peek();
        let next2 = self.peek_n(1);

        let result = match (next1, next2) {
            // TODO: Would need to skip noncode here ... maybe it's better to
            // parse an expression, and re-interpret it later if it reads like a
            // variable access?
            (Some(Token::Ident), Some(Token::Eq1)) => self.parse_seq_assoc_ident()?,
            (Some(Token::KwAssert | Token::KwLet | Token::KwTrace), _) => {
                let stmt = self.parse_stmt()?;
                let (body_span, body) = self.parse_prefixed_seq()?;
                Seq::Stmt {
                    stmt,
                    body_span,
                    body: Box::new(body),
                }
            }
            (Some(Token::KwFor), _) => self.parse_seq_for()?,
            (Some(Token::KwIf), _) => self.parse_seq_if()?,
            _ => {
                let before = self.peek_span();
                let expr = self.parse_expr_op()?;
                // TODO: This span is not necessarily minimal, it may include
                // whitespace.
                let expr_span = before.until(self.peek_span());
                self.skip_non_code()?;
                match self.peek() {
                    Some(Token::Colon) => {
                        let op = self.consume();
                        self.skip_non_code()?;
                        let (value_span, value) = self.parse_expr()?;
                        Seq::AssocExpr {
                            op_span: op,
                            field_span: expr_span,
                            field: Box::new(expr),
                            value_span,
                            value: Box::new(value),
                        }
                    }
                    _ => Seq::Elem {
                        span: expr_span,
                        value: Box::new(expr),
                    },
                }
            }
        };

        Ok((self.span_from(begin), result))
    }

    fn parse_seq_assoc_ident(&mut self) -> Result<Seq> {
        let ident = self.consume();

        self.skip_non_code()?;
        let op = self.parse_token(Token::Eq1, "Expected '=' here.")?;

        self.skip_non_code()?;
        let (value_span, value) = self.parse_expr()?;

        let result = Seq::AssocIdent {
            op_span: op,
            field: ident,
            value_span,
            value: Box::new(value),
        };

        Ok(result)
    }

    fn parse_seq_for(&mut self) -> Result<Seq> {
        let _for = self.consume();

        // Parse the loop variables. Here a trailing comma is not allowed.
        let mut idents = Vec::new();
        loop {
            self.skip_non_code()?;
            let ident = self.parse_token(Token::Ident, "Expected identifier here.")?;
            idents.push(ident);

            self.skip_non_code()?;
            match self.peek() {
                Some(Token::Comma) => {
                    self.consume();
                    continue;
                }
                _ => break,
            }
        }

        self.skip_non_code()?;
        self.parse_token(Token::KwIn, "Expected 'in' here.")?;

        self.skip_non_code()?;
        let (collection_span, collection) = self.parse_expr()?;

        self.skip_non_code()?;
        self.parse_token(Token::Colon, "Expected ':' after the collection.")?;

        let (_body_span, body) = self.parse_prefixed_seq()?;

        let result = Seq::For {
            idents: idents.into_boxed_slice(),
            collection_span,
            collection: Box::new(collection),
            body: Box::new(body),
        };

        Ok(result)
    }

    fn parse_seq_if(&mut self) -> Result<Seq> {
        let _if = self.consume();

        self.skip_non_code()?;
        let (condition_span, condition) = self.parse_expr()?;

        self.skip_non_code()?;
        self.parse_token(Token::Colon, "Expected ':' after the condition.")?;

        let (_body_span, body) = self.parse_prefixed_seq()?;

        let result = Seq::If {
            condition_span,
            condition: Box::new(condition),
            body: Box::new(body),
        };

        Ok(result)
    }

    /// Parse a type expression.
    fn parse_type_expr(&mut self) -> Result<Type> {
        // If it starts with a `(`, then that is the start of an argument list,
        // and we are parsing a function type.
        if let Some(Token::LParen) = self.peek() {
            return self.parse_type_function();
        }

        // Otherwise, we definitely start with a term.
        let begin = self.peek_span();
        let term = self.parse_type_term()?;

        // Optionally, the term can be followed by `[` to instantiate a generic
        // type.
        self.skip_non_code()?;
        if let (Type::Term(name), Some(Token::LBracket)) = (&term, self.peek()) {
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
    fn parse_types(&mut self) -> Result<Box<[Prefixed<Type>]>> {
        let mut result = Vec::new();
        loop {
            let prefix = self.parse_non_code();
            if matches!(self.peek(), Some(Token::RParen | Token::RBracket)) {
                // TODO: In this case we lose the prefix that we parsed.
                // See also the comment in `parse_seqs`.
                return Ok(result.into_boxed_slice());
            }

            let type_ = self.parse_type_expr()?;
            let prefixed = Prefixed {
                prefix,
                inner: type_,
            };
            result.push(prefixed);

            self.skip_non_code()?;
            match self.peek() {
                Some(Token::RParen | Token::RBracket) => continue,
                Some(Token::Comma) => {
                    self.consume();
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
            Some(Token::Ident) => {
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
        if self.peek().is_some() {
            return self
                .error("Unexpected content after the main expression.")
                .err();
        }
        Ok(())
    }
}
