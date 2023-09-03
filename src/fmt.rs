// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An auto-formatter for pretty-printing CST nodes.
//!
//! Note, currently this is an extremely naive toy formatter that formats
//! everything the long way. At some point I would like to implement a proper
//! line breaking algorithm, but for now this will suffice.

use crate::cst::{Expr, NonCode, Prefixed, Seq};
use crate::lexer::QuoteStyle;
use crate::pprint::{concat, group, indent, Config, Doc};
use crate::source::Span;

/// Write a formatted expression to the output.
pub fn format_expr(input: &str, expr: &Prefixed<Expr>, config: &Config) -> String {
    let formatter = Formatter::new(input);
    let doc = formatter.prefixed_expr(expr);
    doc.print(config)
}

struct Formatter<'a> {
    // TODO: This could all be more efficient if we resolved on bytestrings, so
    // the code point slicing check can be omitted.
    input: &'a str,
}

/// How to terminate an item in a collection literal.
#[derive(Copy, Clone)]
enum Separator {
    /// Unconditionally append a separator.
    Unconditional,
    /// Append the sparator only in tall mode.
    Trailer,
    /// Do not append a separator.
    None,
}

impl<'a> Formatter<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    pub fn span(&self, span: Span) -> Doc<'a> {
        span.resolve(self.input).into()
    }

    pub fn non_code(&self, nc: &[NonCode]) -> Doc<'a> {
        let mut result = Vec::new();

        for line in nc {
            match line {
                NonCode::Blank(..) => {
                    result.push(Doc::HardBreak);
                }
                NonCode::LineComment(span) => {
                    result.push(self.span(*span));
                    result.push(Doc::HardBreak);
                }
            }
        }

        Doc::Concat(result)
    }

    pub fn prefixed_expr(&self, expr: &Prefixed<Expr>) -> Doc<'a> {
        concat! {
            self.non_code(&expr.prefix)
            self.expr(&expr.inner)
        }
    }

    pub fn expr(&self, expr: &Expr) -> Doc<'a> {
        match expr {
            // TODO: Make let-chains a first class construct, so we can format
            // them either wide or tall.
            Expr::Let {
                ident, value, body, ..
            } => {
                concat! {
                    "let " self.span(*ident) " = " self.expr(value) ";"
                    Doc::Sep
                    self.prefixed_expr(&body)
                }
            }

            Expr::BraceLit { elements, .. } => {
                if elements.is_empty() {
                    Doc::str("{}")
                } else {
                    group! {
                        "{"
                            Doc::SoftBreak
                            indent! { self.seqs(elements) }
                            Doc::SoftBreak
                        "}"
                    }
                }
            }

            Expr::BracketLit { elements, .. } => {
                if elements.is_empty() {
                    Doc::str("[]")
                } else {
                    group! {
                        "["
                            Doc::SoftBreak
                            indent! { self.seqs(elements) }
                            Doc::SoftBreak
                        "]"
                    }
                }
            }

            Expr::Parens { body, .. } => {
                group! {
                    "("
                    Doc::SoftBreak
                    indent! { self.prefixed_expr(body) }
                    Doc::SoftBreak
                    ")"
                }
            }

            Expr::BoolLit(span, ..) => self.span(*span),

            Expr::StringLit(style, span) => match style {
                QuoteStyle::Double => self.span(*span),
                QuoteStyle::Triple => concat! {
                    Doc::HardBreak
                    // TODO: Write lines individually.
                    indent! { self.span(*span) }
                },
            },

            Expr::NumHexadecimal(span) => {
                // Normalize A-F to a-f.
                span.resolve(self.input).to_ascii_lowercase().into()
            }

            Expr::NumBinary(span) => self.span(*span),

            Expr::NumDecimal(span) => {
                // Normalize exponent E to e.
                span.resolve(self.input).to_ascii_lowercase().into()
            }

            Expr::Var(span) => self.span(*span),

            // TODO: Parse as vec with multiple dots, so we can toggle an entire
            // method chain as all-or-nothing wide or tall. For now, we just
            // don't line-wrap field access.
            Expr::Field { inner, field } => {
                concat! {
                    self.expr(inner) "." self.span(*field)
                }
            }

            Expr::Call { function, args, .. } => {
                concat! {
                    self.expr(function)
                    group! {
                        "("
                        Doc::SoftBreak
                        indent! {
                            Doc::join(
                                args.iter().map(|arg| self.prefixed_expr(arg)),
                                concat!{ "," Doc::Sep },
                            )
                            Doc::tall(",")
                        }
                        Doc::SoftBreak
                        ")"
                    }
                }
            }

            Expr::UnOp { op_span, body, .. } => {
                concat! {
                    self.span(*op_span)
                    Doc::Sep
                    self.expr(body)
                }
            }

            // TODO: Make this a collection in the parser, so we can toggle
            // operator chains into all-wide or all-tall but not mixed.
            Expr::BinOp {
                op_span, lhs, rhs, ..
            } => {
                group! {
                    self.expr(lhs)
                    Doc::Sep
                    self.span(*op_span)
                    " "
                    self.expr(rhs)
                }
            }

            todo => unimplemented!("Fmt not implemented for {todo:?}"),
        }
    }

    /// Emit the separator according to the termination mode.
    pub fn separator(&self, separator: &'a str, mode: Separator) -> Doc<'a> {
        match mode {
            Separator::Unconditional => Doc::str(separator),
            Separator::Trailer => Doc::tall(separator),
            Separator::None => Doc::empty(),
        }
    }

    pub fn seqs(&self, elements: &[Prefixed<Seq>]) -> Doc<'a> {
        let mut result = Vec::new();
        for (i, elem) in elements.iter().enumerate() {
            let is_last = i + 1 == elements.len();
            let sep_mode = match i {
                // For collections that contain a single seq, do not add a
                // separator, even when they are multi-line. It makes
                // comprehensions look weird, which are regularly multi-line but
                // only rarely are there multiple seqs in the collection.
                _ if elements.len() == 1 => Separator::None,
                _ if is_last => Separator::Trailer,
                _ => Separator::Unconditional,
            };
            result.push(self.non_code(&elem.prefix));
            result.push(self.seq(&elem.inner, sep_mode));
            if !is_last {
                result.push(Doc::Sep)
            }
        }
        Doc::Concat(result)
    }

    pub fn seq(&self, seq: &Seq, sep_mode: Separator) -> Doc<'a> {
        match seq {
            Seq::Elem { value, .. } => {
                concat! {
                    self.expr(value)
                    self.separator(",", sep_mode)
                }
            }

            Seq::AssocExpr { field, value, .. } => {
                concat! {
                    self.expr(field) ": " self.expr(value)
                    self.separator(",", sep_mode)
                }
            }

            Seq::AssocIdent { field, value, .. } => {
                concat! {
                    self.span(*field) " = " self.expr(value)
                    self.separator(";", sep_mode)
                }
            }

            Seq::Let {
                ident, value, body, ..
            } => {
                concat! {
                    "let " self.span(*ident) " = " self.expr(value) ";"
                    Doc::Sep
                    self.non_code(&body.prefix)
                    self.seq(&body.inner, sep_mode)
                }
            }

            Seq::For {
                idents,
                collection,
                body,
                ..
            } => {
                concat! {
                    "for "
                    // TODO: This does not use a proper sep, which means we
                    // cannot break this over multiple lines. But maybe that's
                    // okay.
                    Doc::join(
                        idents.iter().map(|ident| self.span(*ident)),
                        ", ".into(),
                    )
                    " in "
                    self.expr(collection)
                    ":"
                    Doc::Sep
                    self.non_code(&body.prefix)
                    self.seq(&body.inner, sep_mode)
                }
            }

            Seq::If {
                condition, body, ..
            } => {
                concat! {
                    "if "
                    self.expr(condition)
                    ":"
                    Doc::Sep
                    self.non_code(&body.prefix)
                    self.seq(&body.inner, sep_mode)
                }
            }
        }
    }
}
