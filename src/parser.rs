// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use crate::cst::{BinOp, Expr, FormatHole, NonCode, Prefixed, Seq, UnOp};
use crate::error::{IntoParseError, ParseError};
use crate::lexer::{self, QuoteStyle, Token};
use crate::source::{DocId, Span};

pub type Result<T> = std::result::Result<T, ParseError>;

/// Parse an input document into a concrete syntax tree.
pub fn parse(doc: DocId, input: &str) -> Result<(Span, Prefixed<Expr>)> {
    let tokens = lexer::lex(doc, input)?;
    let mut parser = Parser::new(doc, input, &tokens);

    // Comments at the start of the document are allowed, but the document
    // should not start with blank lines, those we drop.
    parser.skip_blanks();

    let (span, result) = parser.parse_prefixed_expr()?;
    parser.parse_eof()?;
    Ok((span, result))
}

fn to_unop(token: Token) -> Option<UnOp> {
    match token {
        Token::KwNot => Some(UnOp::Neg),
        _ => None,
    }
}

fn to_binop(token: Token) -> Option<BinOp> {
    match token {
        Token::KwAnd => Some(BinOp::And),
        Token::KwOr => Some(BinOp::Or),
        Token::Pipe => Some(BinOp::Union),
        Token::Plus => Some(BinOp::Add),
        Token::Star => Some(BinOp::Mul),
        Token::Lt => Some(BinOp::Lt),
        Token::Gt => Some(BinOp::Gt),
        Token::LtEq => Some(BinOp::LtEq),
        Token::GtEq => Some(BinOp::GtEq),
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

    /// A stack of the starting points of the spans that we are currently parsing.
    ///
    /// This is used to provide full enclosing spans of parts of expressions.
    /// For example, the condition of an if-then-else may itself be a complex
    /// expression, and the CST nodes of the condition don't expose a single
    /// span that covers it entirely, so we track this separately.
    span_stack: Vec<usize>,
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
            span_stack: Vec::new(),
        }
    }

    /// Build a parse error at the current cursor location.
    fn error<T>(&self, message: &'static str) -> Result<T> {
        Err(self.peek_span().error(message))
    }

    /// Build a parse error at the current cursor location, and a note elsewhere.
    fn error_with_note<T>(
        &self,
        message: &'static str,
        note_span: Span,
        note: &'static str,
    ) -> Result<T> {
        self.error(message)
            .map_err(|err| err.with_note(note_span, note))
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

    fn increase_depth(&mut self) -> Result<()> {
        self.depth += 1;

        if self.depth >= 100 {
            return self.error("Parser recursion limit reached, please reduce nesting.");
        }

        Ok(())
    }

    fn decrease_depth(&mut self) {
        debug_assert!(self.depth > 0, "Expression depth underflow.");
        self.depth -= 1;
    }

    /// Record the starting position of some span that we will later pop.
    fn begin_span(&mut self) {
        self.span_stack.push(self.peek_span().start());
    }

    /// Pop off the span stack and return the span that covers the source since the matching push.
    fn end_span(&mut self) -> Span {
        let start = self.span_stack.pop().expect("Span stack underflow.");
        let end = self.tokens[..self.cursor]
            .iter()
            .rev()
            .filter(|t| !matches!(t.0, Token::Space | Token::LineComment))
            .map(|t| t.1.end())
            .next()
            .expect("If we pushed a start, we should find at least that.");
        Span::new(self.doc, start, end)
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
                    return self.error_with_note(
                        "A comment is not allowed here.",
                        self.comment_anchor,
                        "Try inserting the comment above this instead.",
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

    pub fn parse_prefixed_expr(&mut self) -> Result<(Span, Prefixed<Expr>)> {
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
        self.begin_span();
        // TODO: This depth limit is not sustainable if my lets are recursive.
        // We need tail calls or loops to handle them in the parser ...
        self.increase_depth()?;
        let result = match self.peek() {
            Some(Token::KwLet) => self.parse_expr_let(),
            Some(Token::KwIf) => self.parse_expr_if(),
            _ => self.parse_expr_op(),
        };
        self.decrease_depth();

        match result {
            Ok(expr) => Ok((self.end_span(), expr)),
            Err(err) => Err(err),
        }
    }

    fn parse_expr_if(&mut self) -> Result<Expr> {
        // Consume the `if` keyword.
        let _if = self.consume();
        let (condition_span, condition) = self.parse_prefixed_expr()?;

        self.skip_non_code()?;
        self.parse_token(Token::KwThen, "Expected 'then' here.")?;
        let (then_span, then_body) = self.parse_prefixed_expr()?;

        self.skip_non_code()?;
        self.parse_token(Token::KwElse, "Expected 'else' here.")?;
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

    fn parse_expr_let(&mut self) -> Result<Expr> {
        // Consume the `let` keyword.
        let let_ = self.consume();

        self.skip_non_code()?;
        let ident = self.parse_ident()?;

        self.skip_non_code()?;
        self.parse_token(Token::Eq, "Expected '=' here.")?;

        self.skip_non_code()?;
        let (value_span, value) = self.parse_expr()?;

        self.skip_non_code()?;
        self.parse_token_with_note(
            Token::Semicolon,
            "Expected ';' here to close the let-binding.",
            let_,
            "Let-binding opened here.",
        )?;

        let (body_span, body) = self.parse_prefixed_expr()?;

        let result = Expr::Let {
            ident,
            value_span,
            value: Box::new(value),
            body_span,
            body: Box::new(body),
        };

        Ok(result)
    }

    /// Return an error with hint if there is a known bad unary operator under the cursor.
    fn check_bad_unop(&self) -> Result<()> {
        if let Some(Token::Bang) = self.peek() {
            return self
                .error("Invalid operator. Negation is written with keyword 'not' instead of '!'.");
        }
        Ok(())
    }

    fn parse_expr_op(&mut self) -> Result<Expr> {
        // First we check all the rules for prefix unary operators.
        self.check_bad_unop()?;
        if self.peek().and_then(to_unop).is_some() {
            return self.parse_expr_unop();
        }

        // If it was not a prefix unary operator, then we certainly have one
        // not_op, but we may have an operator afterwards.
        let mut result = self.parse_expr_not_op()?;

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
                    let rhs = self.parse_expr_not_op()?;
                    allowed_span = Some(span);
                    allowed_op = Some(op);
                    result = Expr::BinOp {
                        op,
                        op_span: span,
                        lhs: Box::new(result),
                        rhs: Box::new(rhs),
                    };
                }
                Some(_op) => {
                    return self.error_with_note(
                        "Parentheses are needed to clarify the precedence of this operator.",
                        allowed_span.expect("If we are here, allowed_span must be set."),
                        "Without parentheses, it is not clear whether this operator should take precedence.",
                    );
                }
                _ => return Ok(result),
            }
        }
    }

    fn parse_expr_unop(&mut self) -> Result<Expr> {
        let op = self
            .peek()
            .and_then(to_unop)
            .expect("Should only call this with unop under cursor.");
        let span = self.consume();
        self.skip_non_code()?;
        self.check_bad_unop()?;

        // Nested unary expressions are okay.
        let body = if self.peek().and_then(to_unop).is_some() {
            self.parse_expr_unop()?
        } else {
            self.parse_expr_not_op()?
        };

        let result = Expr::UnOp {
            op,
            op_span: span,
            body: Box::new(body),
        };

        // Check if the expression is followed by a binary operator. This is
        // not allowed in the grammar on purpose to force parens to clarify
        // precedence, but if we don't check for it, then the resulting
        // parse error is confusing, about unexpected content after the end
        // of the expression/document.
        self.skip_non_code()?;
        if self.peek().and_then(to_binop).is_some() {
            return self.error_with_note(
                "Parentheses are needed to clarify the precedence of this operator.",
                span,
                "Without parentheses, it is not clear whether this operator \
                applies only to the left-hand side, or the full expression.",
            );
        }

        Ok(result)
    }

    fn parse_expr_not_op(&mut self) -> Result<Expr> {
        // TODO: check for operators before, and report a pretty error
        // to clarify that parens must be used to disambiguate.
        let before = self.peek_span();
        let mut result = self.parse_expr_term()?;
        // TODO: This span is not necessarily minimal, it may include whitespace.
        loop {
            self.skip_non_code()?;
            match self.peek() {
                Some(Token::LParen) => {
                    let result_span = before.until(self.peek_span());
                    let open = self.push_bracket()?;
                    let args = self.parse_call_args()?;
                    let close = self.pop_bracket()?;
                    result = Expr::Call {
                        open,
                        close,
                        args,
                        function_span: result_span,
                        function: Box::new(result),
                    };
                }
                Some(Token::Dot) => {
                    self.consume();
                    self.skip_non_code()?;
                    let field = self.parse_token(Token::Ident, "Expected an identifier here.")?;
                    result = Expr::Field {
                        field,
                        inner: Box::new(result),
                    };
                }
                _ => return Ok(result),
            }
        }
    }

    fn parse_expr_term(&mut self) -> Result<Expr> {
        match self.peek() {
            Some(Token::LBrace) => {
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
            Some(Token::LBracket) => {
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
            Some(Token::FormatOpen(style)) => self.parse_format_string(style),
            Some(Token::KwNull) => Ok(Expr::NullLit(self.consume())),
            Some(Token::KwTrue) => Ok(Expr::BoolLit(self.consume(), true)),
            Some(Token::KwFalse) => Ok(Expr::BoolLit(self.consume(), false)),
            Some(Token::Quoted(style)) => Ok(Expr::StringLit(style, self.consume())),
            Some(Token::NumHexadecimal) => Ok(Expr::NumHexadecimal(self.consume())),
            Some(Token::NumBinary) => Ok(Expr::NumBinary(self.consume())),
            Some(Token::NumDecimal) => Ok(Expr::NumDecimal(self.consume())),
            Some(Token::Ident) => Ok(Expr::Var(self.consume())),
            _ => self.error("Expected a term here."),
        }
    }

    /// Parse a format string (`f"` or `f"""`).
    fn parse_format_string(&mut self, style: QuoteStyle) -> Result<Expr> {
        debug_assert_eq!(self.peek(), Some(Token::FormatOpen(style)));

        let begin = self.consume();
        let mut holes = Vec::new();
        let mut prefix = begin;

        loop {
            self.skip_non_code()?;
            let (span, body) = self.parse_expr()?;
            self.skip_non_code()?;

            let is_close = match self.peek() {
                Some(Token::FormatInner(..)) => false,
                Some(Token::FormatClose(..)) => true,
                _ => {
                    return self.error_with_note(
                        "Unclosed hole in f-string, expected '}' here.",
                        prefix.trim_start(prefix.len() - 1),
                        "Hole opened here.",
                    )
                }
            };

            let suffix = self.consume();
            let hole = FormatHole { span, body, suffix };
            holes.push(hole);
            prefix = suffix;

            if is_close {
                let result = Expr::FormatString {
                    style,
                    begin,
                    holes,
                };
                return Ok(result);
            }
        }
    }

    /// Parse arguments in a function call.
    fn parse_call_args(&mut self) -> Result<Box<[Prefixed<Expr>]>> {
        let mut result = Vec::new();

        loop {
            let prefix = self.parse_non_code();
            if self.peek() == Some(Token::RParen) {
                // TODO: In this case we lose the prefix that we parsed.
                // See also the comment in `parse_seqs` below.
                return Ok(result.into_boxed_slice());
            }

            let (_span, expr) = self.parse_expr()?;
            let prefixed = Prefixed {
                prefix,
                inner: expr,
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

    /// Parse sequence elements.
    ///
    /// This corresponds to `seqs` in the grammar, but it is slightly different
    /// from the rule there to be able to incorporate noncode, and also to be
    /// slightly more precise about which separator we allow after a particular
    /// seq, which is easy to do here but difficult to express in the grammar.
    fn parse_seqs(&mut self) -> Result<Box<[Prefixed<Seq>]>> {
        let mut result = Vec::new();

        loop {
            let prefix = self.parse_non_code();
            if matches!(self.peek(), Some(Token::RBrace | Token::RBracket)) {
                // TODO: In this case we lose the prefix that we parsed. So
                // comments in an empty collection literal do not survive,
                // need to find a way to disallow this in the first place.
                // Maybe we could validate that the prefix does not contain
                // comments, and error out if it does?
                return Ok(result.into_boxed_slice());
            }

            let seq = self.parse_seq()?;
            let expected_terminator = match seq {
                Seq::Elem { .. } => Token::Comma,
                Seq::AssocExpr { .. } => Token::Comma,
                Seq::AssocIdent { .. } => Token::Semicolon,
                Seq::Let { .. } => Token::Comma,
                Seq::For { .. } => Token::Comma,
                Seq::If { .. } => Token::Comma,
            };

            let prefixed = Prefixed { prefix, inner: seq };
            result.push(prefixed);

            self.skip_non_code()?;
            match self.peek() {
                Some(Token::RBrace | Token::RBracket) => continue,
                tok if tok == Some(expected_terminator) => {
                    self.consume();
                    continue;
                }
                Some(Token::Semicolon) if expected_terminator == Token::Comma => {
                    return self.error("Expected ',' instead of ';' here.");
                }
                Some(Token::Comma) if expected_terminator == Token::Semicolon => {
                    return self.error("Expected ';' instead of ',' here.");
                }
                // If we don't find a separator, nor the end of the collection
                // literal, that's an error. We can report an unmatched bracket
                // as the problem, because it is. The pop will fail. If we see
                // an '=' maybe the user tried to make a key-value mapping and
                // we can report a better error.
                Some(Token::Eq) => {
                    let mut err = self.pop_bracket().expect_err("We are in a seq.");
                    err.help = Some(
                        "To use 'key = value;' record syntax, \
                        the left-hand side must be an identifier.\n\
                        When that is not possible, \
                        use json-style '\"key\": value' instead.",
                    );
                    return Err(err);
                }
                _ => {
                    self.pop_bracket()?;
                    unreachable!("pop_bracket should have failed.");
                }
            }
        }
    }

    pub fn parse_prefixed_seq(&mut self) -> Result<Prefixed<Seq>> {
        self.parse_prefixed(|s| s.parse_seq())
    }

    fn parse_seq(&mut self) -> Result<Seq> {
        // Here we have a lookahead of two tokens ... not great if we want to
        // keep the grammar simple, but for making the syntax prettier it is
        // worth some complications to allow { a = b; p = q } notation.
        let next1 = self.peek();
        let next2 = self.peek_n(1);

        match (next1, next2) {
            // TODO: Would need to skip noncode here ... maybe it's better to
            // parse an expression, and re-interpret it later if it reads like a
            // variable access?
            (Some(Token::Ident), Some(Token::Eq)) => self.parse_seq_assoc_ident(),
            (Some(Token::KwLet), _) => self.parse_seq_let(),
            (Some(Token::KwFor), _) => self.parse_seq_for(),
            (Some(Token::KwIf), _) => self.parse_seq_if(),
            _ => {
                let before = self.peek_span();
                let expr = self.parse_expr_op()?;
                // TODO: This span is not necessarily minimal, it may include
                // whitespace.
                let expr_span = before.until(self.peek_span());
                self.skip_non_code()?;
                let result = match self.peek() {
                    Some(Token::Colon) => {
                        let op = self.consume();
                        self.skip_non_code()?;
                        let (value_span, value) = self.parse_expr()?;
                        Seq::AssocExpr {
                            op_span: op,
                            field: Box::new(expr),
                            value_span,
                            value: Box::new(value),
                        }
                    }
                    _ => Seq::Elem {
                        span: expr_span,
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
        let op = self.parse_token(Token::Eq, "Expected '=' here.")?;

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

    fn parse_seq_let(&mut self) -> Result<Seq> {
        let let_ = self.consume();

        self.skip_non_code()?;
        let ident = self.parse_token(Token::Ident, "Expected identifier here.")?;

        self.skip_non_code()?;
        self.parse_token(Token::Eq, "Expected '=' here.")?;

        self.skip_non_code()?;
        let (value_span, value) = self.parse_expr()?;

        self.skip_non_code()?;
        self.parse_token_with_note(
            Token::Semicolon,
            "Expected ';' here to close the let-binding.",
            let_,
            "Let-binding opened here.",
        )?;

        let body = self.parse_prefixed_seq()?;

        let result = Seq::Let {
            ident,
            value_span,
            value: Box::new(value),
            body: Box::new(body),
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

        let body = self.parse_prefixed_seq()?;

        let result = Seq::For {
            idents: idents.into_boxed_slice(),
            collection_span,
            collection: Box::new(collection),
            body: Box::new(body),
        };

        Ok(result)
    }

    fn parse_seq_if(&mut self) -> Result<Seq> {
        let if_ = self.consume();

        self.skip_non_code()?;
        let (condition_span, condition) = self.parse_expr()?;

        self.skip_non_code()?;

        // Parse the colon, but also add a hint if this looks like an
        // if-then-else.
        match self.peek() {
            Some(Token::Colon) => self.consume(),
            Some(Token::KwThen) => {
                return self.error_with_note(
                    "Expected ':' here.",
                    if_,
                    "This 'if' is part of a comprehension. \
                    For an if-then-else expression, enclose the expression in parentheses.",
                );
            }
            _ => return self.error("Expected ':' after the condition."),
        };

        let body = self.parse_prefixed_seq()?;

        let result = Seq::If {
            condition_span,
            condition: Box::new(condition),
            body: Box::new(body),
        };

        Ok(result)
    }

    /// Confirm that there is no trailing content left to parse.
    fn parse_eof(&mut self) -> Result<()> {
        self.skip_non_code()?;
        if self.peek().is_some() {
            return self.error("Unexpected content after the main expression.");
        }
        Ok(())
    }
}
