// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Abstraction converts a Concrete Syntax Tree into an Abstract Syntax Tree.
//!
//! It is responsible for a few things:
//!
//! * Forgetting about non-code such as blank lines and comments.
//! * Converting literals in the source code into values in the runtime.
//! * Removing syntactical differences (e.g. converting `k = v;` into `"k": v`).

use std::rc::Rc;

use crate::ast::{Expr as AExpr, FormatFragment, Seq as ASeq};
use crate::cst::Prefixed;
use crate::cst::{Expr as CExpr, FormatHole, Seq as CSeq};
use crate::error::Result;
use crate::lexer::QuoteStyle;
use crate::source::Span;
use crate::string;
use crate::todo_placeholder;

/// Abstract an expression.
pub fn abstract_expr(input: &str, expr: &Prefixed<CExpr>) -> Result<AExpr> {
    Abstractor::new(input).expr(&expr.inner)
}

/// The abstractor can convert CST nodes to AST nodes for a given document.
struct Abstractor<'a> {
    input: &'a str,
}

impl<'a> Abstractor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    /// Unescape the inner part of a string literal.
    pub fn unescape(&self, str_inner: Span) -> Result<Rc<str>> {
        Ok(string::unescape(self.input, str_inner)?.into())
    }

    /// Unescape a `"""`-quoted string literal.
    pub fn unescape_triple(&self, str_inner: Span) -> Result<Rc<str>> {
        let result: Result<String> = string::fold_triple_string_lines(
            self.input,
            str_inner,
            String::with_capacity(str_inner.len()),
            |mut result, span| {
                string::unescape_into(self.input, span, &mut result)?;
                Ok(result)
            },
        );
        Ok(result?.into())
    }

    /// Abstract a `f"`-quoted format string.
    fn format_string_double(&self, begin: Span, holes: &[FormatHole]) -> Result<AExpr> {
        let fragments: Result<Vec<_>> = string::fold_double_format_string(
            begin,
            holes,
            Vec::new(),
            |mut fragments, span| {
                let frag = FormatFragment {
                    span,
                    body: AExpr::StringLit(self.unescape(span)?),
                };
                fragments.push(frag);
                Ok(fragments)
            },
            |mut fragments, span, expr| {
                let frag = FormatFragment {
                    span,
                    body: self.expr(expr)?,
                };
                fragments.push(frag);
                Ok(fragments)
            },
        );
        Ok(AExpr::Format(fragments?))
    }

    /// Abstract a `f"""`-quoted format string.
    fn format_string_triple(&self, begin: Span, holes: &[FormatHole]) -> Result<AExpr> {
        let fragments: Result<Vec<_>> = string::fold_triple_format_string_lines(
            self.input,
            begin,
            holes,
            Vec::new(),
            |mut fragments, span| {
                let frag = FormatFragment {
                    span,
                    body: AExpr::StringLit(self.unescape(span)?),
                };
                fragments.push(frag);
                Ok(fragments)
            },
            |mut fragments, span, expr| {
                let frag = FormatFragment {
                    span,
                    body: self.expr(expr)?,
                };
                fragments.push(frag);
                Ok(fragments)
            },
        );
        Ok(AExpr::Format(fragments?))
    }

    /// Abstract an expression.
    pub fn expr(&self, expr: &CExpr) -> Result<AExpr> {
        let result = match expr {
            CExpr::Let {
                ident, value, body, ..
            } => AExpr::Let {
                ident: ident.resolve(self.input).into(),
                value: Box::new(self.expr(value)?),
                body: Box::new(self.expr(&body.inner)?),
            },

            CExpr::BraceLit { elements, .. } => AExpr::BraceLit(
                elements
                    .iter()
                    .map(|elem| self.seq(&elem.inner))
                    .collect::<Result<Vec<_>>>()?,
            ),

            CExpr::BracketLit { elements, .. } => AExpr::BracketLit(
                elements
                    .iter()
                    .map(|elem| self.seq(&elem.inner))
                    .collect::<Result<Vec<_>>>()?,
            ),

            CExpr::Parens { body, .. } => self.expr(&body.inner)?,

            CExpr::NullLit(_span) => AExpr::NullLit,

            CExpr::BoolLit(_span, b) => AExpr::BoolLit(*b),

            CExpr::StringLit(style, span) => {
                // Cut off the string literal quotes.
                let len = style.len();
                let span_inner = span.trim_start(len).trim_end(len);

                match style {
                    QuoteStyle::Double => AExpr::StringLit(self.unescape(span_inner)?),
                    QuoteStyle::Triple => AExpr::StringLit(self.unescape_triple(span_inner)?),
                }
            }

            CExpr::FormatString {
                style: QuoteStyle::Double,
                begin,
                holes,
            } => self.format_string_double(*begin, holes)?,

            CExpr::FormatString {
                style: QuoteStyle::Triple,
                begin,
                holes,
            } => self.format_string_triple(*begin, holes)?,

            CExpr::NumHexadecimal(span) => {
                // Cut off the 0x, then parse the rest.
                let num_str = span.trim_start(2).resolve(self.input).replace('_', "");
                match i64::from_str_radix(&num_str, 16) {
                    Ok(i) => AExpr::IntegerLit(i),
                    Err(..) => todo_placeholder!(
                        "TODO: Handle overflow when parsing binary literal.",
                        AExpr::IntegerLit(0),
                    ),
                }
            }

            CExpr::NumBinary(span) => {
                // Cut off the 0b, then parse the rest.
                let num_str = span.trim_start(2).resolve(self.input).replace('_', "");
                match i64::from_str_radix(&num_str, 2) {
                    Ok(i) => AExpr::IntegerLit(i),
                    Err(..) => todo_placeholder!(
                        "TODO: Handle overflow when parsing binary literal.",
                        AExpr::IntegerLit(0),
                    ),
                }
            }

            CExpr::NumDecimal(span) => {
                let num_str = span.resolve(self.input).replace('_', "");
                match i64::from_str_radix(&num_str, 10) {
                    Ok(i) => AExpr::IntegerLit(i),
                    Err(..) => todo_placeholder!(
                        "TODO: Implement parsing of non-integer number literals.",
                        AExpr::IntegerLit(42),
                    ),
                }
            }

            CExpr::IfThenElse {
                condition_span,
                condition,
                then_body,
                else_body,
                ..
            } => AExpr::IfThenElse {
                condition_span: *condition_span,
                condition: Box::new(self.expr(&condition.inner)?),
                body_then: Box::new(self.expr(&then_body.inner)?),
                body_else: Box::new(self.expr(&else_body.inner)?),
            },

            CExpr::Var(span) => AExpr::Var {
                span: *span,
                ident: span.resolve(self.input).into(),
            },

            CExpr::Field { inner, field } => AExpr::Field {
                inner: Box::new(self.expr(inner)?),
                field: field.resolve(self.input).into(),
                field_span: *field,
            },

            CExpr::Call {
                open,
                function,
                function_span,
                args,
                ..
            } => AExpr::Call {
                open: *open,
                function_span: *function_span,
                function: Box::new(self.expr(function)?),
                args: args
                    .iter()
                    .map(|a| self.expr(&a.inner))
                    .collect::<Result<Vec<_>>>()?,
            },

            CExpr::UnOp { op, op_span, body } => AExpr::UnOp {
                op: *op,
                op_span: *op_span,
                body: Box::new(self.expr(body)?),
            },

            CExpr::BinOp {
                op,
                op_span,
                lhs,
                rhs,
            } => AExpr::BinOp {
                op: *op,
                op_span: *op_span,
                lhs: Box::new(self.expr(lhs)?),
                rhs: Box::new(self.expr(rhs)?),
            },
        };
        Ok(result)
    }

    /// Abstract a sequence element.
    pub fn seq(&self, seq: &CSeq) -> Result<ASeq> {
        let result = match seq {
            CSeq::Elem { span, value } => ASeq::Elem {
                span: *span,
                value: Box::new(self.expr(value)?),
            },

            CSeq::AssocExpr {
                op_span,
                field,
                value,
                ..
            } => ASeq::Assoc {
                op_span: *op_span,
                key: Box::new(self.expr(field)?),
                value: Box::new(self.expr(value)?),
            },

            CSeq::AssocIdent {
                op_span,
                field,
                value,
                ..
            } => {
                // We convert the `key = value` as if it had been written
                // `"key": value` so we can treat them uniformly from here on.
                let key_str = field.resolve(self.input);
                let key_expr = AExpr::StringLit(key_str.into());
                ASeq::Assoc {
                    op_span: *op_span,
                    key: Box::new(key_expr),
                    value: Box::new(self.expr(value)?),
                }
            }

            CSeq::Let {
                ident, value, body, ..
            } => ASeq::Let {
                ident: ident.resolve(self.input).into(),
                value: Box::new(self.expr(value)?),
                body: Box::new(self.seq(&body.inner)?),
            },

            CSeq::For {
                idents,
                collection_span,
                collection,
                body,
            } => ASeq::For {
                idents_span: idents
                    .iter()
                    .copied()
                    .reduce(|x, y| x.union(y))
                    .expect("Parser should have produced at least one ident."),
                idents: idents
                    .iter()
                    .map(|span| span.resolve(self.input).into())
                    .collect(),
                collection_span: *collection_span,
                collection: Box::new(self.expr(collection)?),
                body: Box::new(self.seq(&body.inner)?),
            },

            CSeq::If {
                condition_span,
                condition,
                body,
            } => ASeq::If {
                condition_span: *condition_span,
                condition: Box::new(self.expr(condition)?),
                body: Box::new(self.seq(&body.inner)?),
            },
        };
        Ok(result)
    }
}
