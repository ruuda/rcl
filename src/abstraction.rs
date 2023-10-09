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

use crate::ast::{Expr as AExpr, Expr, FormatFragment, Seq as ASeq, Stmt as AStmt, Yield};
use crate::cst::Prefixed;
use crate::cst::{Expr as CExpr, Seq as CSeq, Stmt as CStmt, StringPart};
use crate::error::{IntoError, Result};
use crate::lexer::QuoteStyle;
use crate::string;

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

    /// Abstract a string or format string.
    ///
    /// If possible we return `Expr::StringLit`, but if the string has holes, we
    /// return `Expr::Format`.
    fn string(&self, style: QuoteStyle, parts: &[StringPart]) -> Result<AExpr> {
        let n_strip = match style {
            QuoteStyle::Double => 0,
            QuoteStyle::Triple => string::count_common_leading_spaces(self.input, parts),
        };
        let mut fragments = Vec::new();
        let mut current = String::new();
        let mut current_span = None;

        for (i, part) in parts.iter().enumerate() {
            match part {
                StringPart::String(span) => {
                    let line = span.resolve(self.input);
                    if style == QuoteStyle::Triple && line.starts_with('\n') {
                        if i > 0 {
                            current.push('\n');
                        }
                        let n = line.len().min(1 + n_strip);
                        current.push_str(&line[n..]);
                    } else {
                        current.push_str(line);
                    }
                    current_span = Some(current_span.unwrap_or(*span).union(*span));
                }
                StringPart::Escape(span, esc) => {
                    string::unescape_into(self.input, *span, *esc, &mut current)?;
                    current_span = Some(current_span.unwrap_or(*span).union(*span));
                }
                StringPart::Hole(span, expr) => {
                    if !current.is_empty() {
                        fragments.push(FormatFragment {
                            span: current_span.expect("Must have string part before a hole."),
                            body: Expr::StringLit(current.into()),
                        });
                        current = String::new();
                        current_span = None;
                    }
                    fragments.push(FormatFragment {
                        span: *span,
                        body: self.expr(expr)?,
                    });
                }
            }
        }

        if fragments.is_empty() {
            // If we have no fragments, then we had no holes, and we can return
            // a regular string literal.
            Ok(Expr::StringLit(current.into()))
        } else {
            // If we have fragments, then we had holes, and we have to return
            // a format string.
            if !current.is_empty() {
                fragments.push(FormatFragment {
                    span: current_span.expect("If we have a fragment, we should have a span."),
                    body: Expr::StringLit(current.into()),
                });
            }
            Ok(AExpr::Format(fragments))
        }
    }

    /// Abstract a statement.
    pub fn stmt(&self, stmt: &CStmt) -> Result<AStmt> {
        let result = match stmt {
            CStmt::Let { ident, value, .. } => AStmt::Let {
                ident: ident.resolve(self.input).into(),
                value: Box::new(self.expr(value)?),
            },
            CStmt::Assert {
                condition_span,
                condition,
                message,
                ..
            } => AStmt::Assert {
                condition_span: *condition_span,
                condition: Box::new(self.expr(condition)?),
                message: Box::new(self.expr(message)?),
            },
            CStmt::Trace {
                trace_span,
                message,
                ..
            } => AStmt::Trace {
                trace_span: *trace_span,
                message: Box::new(self.expr(message)?),
            },
        };
        Ok(result)
    }

    /// Abstract an expression.
    pub fn expr(&self, expr: &CExpr) -> Result<AExpr> {
        let result = match expr {
            CExpr::Stmt { stmt, body, .. } => AExpr::Stmt {
                stmt: self.stmt(stmt)?,
                body: Box::new(self.expr(&body.inner)?),
            },

            CExpr::Import {
                import_span,
                path_span,
                path,
            } => AExpr::Import {
                import_span: *import_span,
                path_span: *path_span,
                path: Box::new(self.expr(&path.inner)?),
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

            CExpr::StringLit { style, parts, .. } => self.string(*style, parts)?,

            CExpr::NumHexadecimal(span) => {
                // Cut off the 0x, then parse the rest.
                let num_str = span.trim_start(2).resolve(self.input).replace('_', "");
                match i64::from_str_radix(&num_str, 16) {
                    Ok(i) => AExpr::IntegerLit(i),
                    Err(..) => {
                        let err = span.error("Overflow in integer literal.");
                        return Err(err.into());
                    }
                }
            }

            CExpr::NumBinary(span) => {
                // Cut off the 0b, then parse the rest.
                let num_str = span.trim_start(2).resolve(self.input).replace('_', "");
                match i64::from_str_radix(&num_str, 2) {
                    Ok(i) => AExpr::IntegerLit(i),
                    Err(..) => {
                        let err = span.error("Overflow in integer literal.");
                        return Err(err.into());
                    }
                }
            }

            CExpr::NumDecimal(span) => {
                // TODO: Handle floats.
                let num_str = span.resolve(self.input).replace('_', "");
                match i64::from_str_radix(&num_str, 10) {
                    Ok(i) => AExpr::IntegerLit(i),
                    Err(..) => {
                        let err = span.error("Overflow in integer literal.");
                        return Err(err.into());
                    }
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
            CSeq::Elem { span, value } => ASeq::Yield(Yield::Elem {
                span: *span,
                value: Box::new(self.expr(value)?),
            }),

            CSeq::AssocExpr {
                op_span,
                field,
                value,
                ..
            } => ASeq::Yield(Yield::Assoc {
                op_span: *op_span,
                key: Box::new(self.expr(field)?),
                value: Box::new(self.expr(value)?),
            }),

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
                ASeq::Yield(Yield::Assoc {
                    op_span: *op_span,
                    key: Box::new(key_expr),
                    value: Box::new(self.expr(value)?),
                })
            }

            CSeq::Stmt { stmt, body, .. } => ASeq::Stmt {
                stmt: self.stmt(stmt)?,
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
