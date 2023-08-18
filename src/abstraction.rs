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

use crate::ast::{Expr as AExpr, Seq as ASeq};
use crate::cst::Prefixed;
use crate::cst::{Expr as CExpr, Seq as CSeq};
use crate::todo_placeholder;

/// Abstract an expression.
pub fn abstract_expr(input: &str, expr: &Prefixed<CExpr>) -> AExpr {
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

    /// Abstract an expression.
    pub fn expr(&self, expr: &CExpr) -> AExpr {
        match expr {
            CExpr::Let {
                ident, value, body, ..
            } => AExpr::Let {
                ident: ident.resolve(self.input).into(),
                value: Box::new(self.expr(value)),
                body: Box::new(self.expr(&body.inner)),
            },

            CExpr::BraceLit { elements, .. } => {
                AExpr::BraceLit(elements.iter().map(|elem| self.seq(&elem.inner)).collect())
            }

            CExpr::BracketLit { elements, .. } => {
                AExpr::BracketLit(elements.iter().map(|elem| self.seq(&elem.inner)).collect())
            }

            CExpr::Parens { body, .. } => self.expr(&body.inner),

            CExpr::BoolLit(_span, b) => AExpr::BoolLit(*b),

            CExpr::StringLit(span) => {
                // Cut off the string literal quotes.
                // TODO: Write a proper parser for string literals that handles
                // escape codes.
                let span_inner = span.trim_start(1).trim_end(1);
                AExpr::StringLit(span_inner.resolve(self.input).into())
            }

            CExpr::StringLitTriple(span) => {
                // Cut off the string literal quotes.
                // TODO: Write a proper parser for string literals that handles
                // escape codes, and for this one, strip the leading whitespace.
                let span_inner = span.trim_start(3).trim_end(3);
                AExpr::StringLit(span_inner.resolve(self.input).into())
            }

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
                condition: Box::new(self.expr(condition)),
                body_then: Box::new(self.expr(&then_body.inner)),
                body_else: Box::new(self.expr(&else_body.inner)),
            },

            CExpr::Var(span) => AExpr::Var {
                span: *span,
                ident: span.resolve(self.input).into(),
            },

            CExpr::Field { inner, field } => AExpr::Field {
                inner: Box::new(self.expr(inner)),
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
                function: Box::new(self.expr(function)),
                args: args.iter().map(|a| self.expr(&a.inner)).collect(),
            },

            CExpr::UnOp { op, op_span, body } => AExpr::UnOp {
                op: *op,
                op_span: *op_span,
                body: Box::new(self.expr(body)),
            },

            CExpr::BinOp {
                op,
                op_span,
                lhs,
                rhs,
            } => AExpr::BinOp {
                op: *op,
                op_span: *op_span,
                lhs: Box::new(self.expr(lhs)),
                rhs: Box::new(self.expr(rhs)),
            },
        }
    }

    /// Abstract a sequence element.
    pub fn seq(&self, seq: &CSeq) -> ASeq {
        match seq {
            CSeq::Elem { span, value } => ASeq::Elem {
                span: *span,
                value: Box::new(self.expr(value)),
            },

            CSeq::AssocExpr {
                op_span,
                field,
                value,
                ..
            } => ASeq::Assoc {
                op_span: *op_span,
                key: Box::new(self.expr(field)),
                value: Box::new(self.expr(value)),
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
                    value: Box::new(self.expr(value)),
                }
            }

            CSeq::Let {
                ident, value, body, ..
            } => ASeq::Let {
                ident: ident.resolve(self.input).into(),
                value: Box::new(self.expr(value)),
                body: Box::new(self.seq(&body.inner)),
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
                collection: Box::new(self.expr(collection)),
                body: Box::new(self.seq(&body.inner)),
            },

            CSeq::If {
                condition_span,
                condition,
                body,
            } => ASeq::If {
                condition_span: *condition_span,
                condition: Box::new(self.expr(condition)),
                body: Box::new(self.seq(&body.inner)),
            },
        }
    }
}
