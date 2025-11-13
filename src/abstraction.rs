// RCL -- A reasonable configuration language.
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

use crate::ast::{
    CallArg, Expr as AExpr, Expr, FormatFragment, Seq as ASeq, Stmt as AStmt, Type as AType,
    Yield as AYield,
};
use crate::cst::{
    Chain, Expr as CExpr, Seq as CSeq, SeqControl, Stmt as CStmt, StringPart, Type as CType,
    Yield as CYield,
};
use crate::decimal::Decimal;
use crate::error::{IntoError, Result};
use crate::lexer::QuoteStyle;
use crate::source::Span;
use crate::string;

/// Abstract an expression.
pub fn abstract_expr(input: &str, expr: &CExpr) -> Result<AExpr> {
    Abstractor::new(input).expr(expr)
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
            CStmt::Let {
                ident,
                type_,
                value_span,
                value,
                ..
            } => AStmt::Let {
                ident_span: *ident,
                ident: ident.resolve(self.input).into(),
                type_: match type_ {
                    None => None,
                    Some(t) => Some(Box::new(self.type_expr(t)?)),
                },
                value_span: *value_span,
                value: Box::new(self.expr(value)?),
            },
            CStmt::Assert {
                condition_span,
                condition,
                message_span,
                message,
            } => AStmt::Assert {
                condition_span: *condition_span,
                condition: Box::new(self.expr(condition)?),
                message_span: *message_span,
                message: Box::new(self.expr(message)?),
            },
            CStmt::Trace {
                message,
                message_span,
            } => AStmt::Trace {
                message_span: *message_span,
                message: Box::new(self.expr(message)?),
            },
        };
        Ok(result)
    }

    /// Abstract an expression.
    pub fn expr(&self, expr: &CExpr) -> Result<AExpr> {
        let result = match expr {
            CExpr::Statements {
                stmts,
                body_span,
                body,
            } => {
                // Convert the flat CST list of statements into a tree for the AST.
                // TODO: Should it be flat in the AST as well? Possibly it's not much
                // harder to implement, and it would help lift recursion limits.
                let mut body_span = *body_span;
                let mut body = self.expr(&body.inner)?;
                for (stmt_span, stmt) in stmts.iter().rev() {
                    body = AExpr::Stmt {
                        stmt: self.stmt(&stmt.inner)?,
                        body_span,
                        body: Box::new(body),
                    };
                    body_span = stmt_span.union(body_span);
                }
                body
            }

            CExpr::Import { path_span, path } => AExpr::Import {
                path_span: *path_span,
                path: Box::new(self.expr(path)?),
            },

            CExpr::BraceLit { open, elements, .. } => AExpr::BraceLit {
                open: *open,
                elements: elements
                    .elements
                    .iter()
                    .map(|elem| self.seq(elem))
                    .collect::<Result<Vec<_>>>()?,
            },

            CExpr::BracketLit { open, elements, .. } => AExpr::BracketLit {
                open: *open,
                elements: elements
                    .elements
                    .iter()
                    .map(|elem| self.seq(elem))
                    .collect::<Result<Vec<_>>>()?,
            },

            CExpr::Parens { body, .. } => self.expr(body)?,

            CExpr::NullLit(_span) => AExpr::NullLit,

            CExpr::BoolLit(_span, b) => AExpr::BoolLit(*b),

            CExpr::StringLit { style, parts, .. } => self.string(*style, parts)?,

            CExpr::NumHexadecimal(span) => {
                // Cut off the 0x, then parse the rest.
                let num_str = span.trim_start(2).resolve(self.input).replace('_', "");
                match i64::from_str_radix(&num_str, 16) {
                    Ok(i) => AExpr::NumberLit(Decimal::from(i)),
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
                    Ok(i) => AExpr::NumberLit(Decimal::from(i)),
                    Err(..) => {
                        let err = span.error("Overflow in integer literal.");
                        return Err(err.into());
                    }
                }
            }

            CExpr::NumDecimal(span) => {
                let num_str = span.resolve(self.input);
                match Decimal::parse_str(num_str) {
                    Some(r) => AExpr::NumberLit(r.into()),
                    None => {
                        let err = span.error("Overflow in number literal.");
                        return Err(err.into());
                    }
                }
            }

            CExpr::IfThenElse {
                condition_span,
                condition,
                then_span,
                else_span,
                then_body,
                else_body,
                ..
            } => AExpr::IfThenElse {
                condition_span: *condition_span,
                span_then: *then_span,
                span_else: *else_span,
                condition: Box::new(self.expr(condition)?),
                body_then: Box::new(self.expr(then_body)?),
                body_else: Box::new(self.expr(else_body)?),
            },

            CExpr::Var(span) => AExpr::Var {
                span: *span,
                ident: span.resolve(self.input).into(),
            },

            CExpr::Function {
                args,
                body_span,
                body,
                ..
            } => AExpr::Function {
                args: args
                    .elements
                    .iter()
                    .map(|arg| (arg.inner, arg.inner.resolve(self.input).into()))
                    .collect(),
                body_span: *body_span,
                body: Box::new(self.expr(body)?),
            },

            CExpr::UnOp {
                op_span,
                op,
                body_span,
                body,
            } => AExpr::UnOp {
                op_span: *op_span,
                op: *op,
                body_span: *body_span,
                body: Box::new(self.expr(body)?),
            },

            CExpr::BinOp {
                op_span,
                op,
                lhs_span,
                lhs,
                rhs_span,
                rhs,
            } => AExpr::BinOp {
                op_span: *op_span,
                op: *op,
                lhs_span: *lhs_span,
                lhs: Box::new(self.expr(lhs)?),
                rhs_span: *rhs_span,
                rhs: Box::new(self.expr(rhs)?),
            },

            CExpr::Chain { base_expr, chain } => {
                let mut inner_expr = self.expr(base_expr)?;
                for (span, chain_expr) in chain.iter() {
                    inner_expr = self.chain(chain_expr, *span, inner_expr)?;
                }
                inner_expr
            }
        };
        Ok(result)
    }

    /// Abstract a sequence yield.
    pub fn yield_(&self, yield_: &CYield) -> Result<AYield> {
        let result = match yield_ {
            CYield::Elem { span, value } => AYield::Elem {
                span: *span,
                value: Box::new(self.expr(value)?),
            },

            CYield::AssocExpr {
                op_span,
                field_span,
                field,
                value_span,
                value,
            } => AYield::Assoc {
                op_span: *op_span,
                key_span: *field_span,
                value_span: *value_span,
                key: Box::new(self.expr(field)?),
                value: Box::new(self.expr(value)?),
            },

            CYield::AssocIdent {
                op_span,
                field,
                value_span,
                value,
            } => {
                // We convert the `key = value` as if it had been written
                // `"key": value` so we can treat them uniformly from here on.
                let key_str = field.resolve(self.input);
                let key_expr = AExpr::StringLit(key_str.into());
                AYield::Assoc {
                    op_span: *op_span,
                    key_span: *field,
                    value_span: *value_span,
                    key: Box::new(key_expr),
                    value: Box::new(self.expr(value)?),
                }
            }

            CYield::UnpackElems {
                unpack_span,
                collection_span,
                collection,
            } => AYield::UnpackElems {
                unpack_span: *unpack_span,
                collection_span: *collection_span,
                collection: Box::new(self.expr(collection)?),
                check_elem_type: None,
            },

            CYield::UnpackAssocs {
                unpack_span,
                collection_span,
                collection,
            } => AYield::UnpackAssocs {
                unpack_span: *unpack_span,
                collection_span: *collection_span,
                collection: Box::new(self.expr(collection)?),
            },
        };
        Ok(result)
    }

    /// Abstract a sequence element.
    pub fn seq(&self, seq: &CSeq) -> Result<ASeq> {
        let mut body = ASeq::Yield(self.yield_(&seq.body.inner)?);

        // We take the flat list of control items from the CST, and build
        // the linked list like tree used in the AST.
        for control in seq.control.iter().rev() {
            body = match &control.inner {
                SeqControl::Stmt { stmt } => ASeq::Stmt {
                    stmt: self.stmt(stmt)?,
                    body: Box::new(body),
                },
                SeqControl::For {
                    idents,
                    collection_span,
                    collection,
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
                    body: Box::new(body),
                },
                SeqControl::If {
                    condition_span,
                    condition,
                } => ASeq::If {
                    condition_span: *condition_span,
                    condition: Box::new(self.expr(condition)?),
                    body: Box::new(body),
                },
            }
        }

        Ok(body)
    }

    /// Abstract an element in a chained expression.
    ///
    /// This is the place where we convert the flat list of the CST (which is
    /// good for formatting) into a degenerate tree of nested inner nodes (which
    /// is a bit nicer for evaluation and typechecking).
    pub fn chain(&self, chained: &Chain, inner_span: Span, inner: AExpr) -> Result<AExpr> {
        let result = match chained {
            Chain::Field { field } => AExpr::Field {
                inner: Box::new(inner),
                inner_span,
                field: field.resolve(self.input).into(),
                field_span: *field,
            },

            Chain::Call {
                open, close, args, ..
            } => AExpr::Call {
                open: *open,
                close: *close,
                function_span: inner_span,
                function: Box::new(inner),
                args: args
                    .elements
                    .iter()
                    .map(|(span, a)| {
                        Ok(CallArg {
                            span: *span,
                            value: self.expr(a)?,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?,
            },

            Chain::Index {
                open,
                close,
                index,
                index_span,
            } => AExpr::Index {
                open: *open,
                close: *close,
                collection_span: inner_span,
                collection: Box::new(inner),
                index_span: *index_span,
                index: Box::new(self.expr(index)?),
            },
        };

        Ok(result)
    }

    /// Abstract a type expression.
    pub fn type_expr(&self, type_: &CType) -> Result<AType> {
        let result = match type_ {
            CType::Term(span) => AType::Term {
                span: *span,
                name: span.resolve(self.input).into(),
            },
            CType::Apply { span, name, args } => AType::Apply {
                span: *span,
                name: name.resolve(self.input).into(),
                args: args
                    .elements
                    .iter()
                    .map(|arg| self.type_expr(&arg.inner))
                    .collect::<Result<Box<_>>>()?,
            },
            CType::Function { span, args, result } => AType::Function {
                span: *span,
                args: args
                    .elements
                    .iter()
                    .map(|arg| self.type_expr(&arg.inner))
                    .collect::<Result<Box<_>>>()?,
                result: Box::new(self.type_expr(result)?),
            },
        };
        Ok(result)
    }
}
