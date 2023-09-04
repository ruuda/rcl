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

use std::io::Write;

use crate::cst::{Expr, NonCode, Prefixed, Seq};
use crate::lexer::QuoteStyle;
use crate::source::Span;

pub type Result = std::io::Result<()>;

/// Write a formatted expression to the output.
pub fn write_expr(input: &str, expr: &Prefixed<Expr>, out: &mut dyn Write) -> Result {
    let mut formatter = Formatter::new(input, out);
    formatter.write_prefixed_expr(expr)?;
    formatter.write_str("\n")
}

struct Formatter<'a> {
    // TODO: This could all be more efficient if we resolved on bytestrings, so
    // the code point slicing check can be omitted.
    input: &'a str,
    out: &'a mut dyn Write,
    indent: u32,
}

impl<'a> Formatter<'a> {
    pub fn new(input: &'a str, out: &'a mut dyn Write) -> Self {
        Self {
            input,
            out,
            indent: 0,
        }
    }

    pub fn write_indent(&mut self) -> Result {
        let spaces = [b' '; 64];
        debug_assert!((self.indent as usize) < spaces.len());
        self.out.write_all(&spaces[..self.indent as usize])
    }

    pub fn write_str(&mut self, s: &str) -> Result {
        self.out.write_all(s.as_bytes())
    }

    pub fn write_span(&mut self, span: Span) -> Result {
        self.out.write_all(span.resolve(self.input).as_bytes())
    }

    pub fn write_non_code(&mut self, nc: &[NonCode]) -> Result {
        for line in nc {
            match line {
                NonCode::Blank(..) => writeln!(self.out)?,
                NonCode::LineComment(span) => {
                    self.write_indent()?;
                    self.write_span(*span)?;
                    self.write_str("\n")?;
                }
            }
        }
        Ok(())
    }

    pub fn write_prefixed_expr(&mut self, expr: &Prefixed<Expr>) -> Result {
        self.write_non_code(&expr.prefix)?;
        self.write_expr(&expr.inner)?;
        Ok(())
    }

    pub fn write_expr(&mut self, expr: &Expr) -> Result {
        match expr {
            Expr::Let {
                ident, value, body, ..
            } => {
                self.write_str("let ")?;
                self.write_span(*ident)?;
                self.write_str(" = ")?;
                self.write_expr(value)?;
                self.write_str(";\n")?;
                self.write_non_code(&body.prefix)?;
                self.write_indent()?;
                self.write_expr(&body.inner)?;
            }

            Expr::BraceLit { elements, .. } => {
                if elements.is_empty() {
                    self.write_str("{}")?;
                } else {
                    self.write_str("{\n")?;
                    self.write_seqs(elements)?;
                    self.write_str("}")?;
                }
            }

            Expr::BracketLit { elements, .. } => {
                if elements.is_empty() {
                    self.write_str("[]")?;
                } else {
                    self.write_str("[\n")?;
                    self.write_seqs(elements)?;
                    self.write_str("]")?;
                }
            }

            Expr::BoolLit(span, ..) => {
                self.write_span(*span)?;
            }

            Expr::StringLit(style, span) => match style {
                QuoteStyle::Double => self.write_span(*span)?,
                QuoteStyle::Triple => {
                    self.write_str("\n")?;
                    self.indent += 2;
                    self.write_indent()?;
                    self.write_span(*span)?;
                    self.indent -= 2;
                }
            },

            Expr::Var(span) => {
                self.write_span(*span)?;
            }

            Expr::Field { inner, field } => {
                self.write_expr(inner)?;
                self.write_str(".")?;
                self.write_span(*field)?;
            }

            Expr::Call { function, args, .. } => {
                self.write_expr(function)?;
                self.write_str("(")?;
                let mut is_first = true;
                for arg in args.iter() {
                    if !is_first {
                        self.write_str(", ")?;
                    }
                    assert!(
                        arg.prefix.is_empty(),
                        "TODO: We can't format non-code here yet."
                    );
                    self.write_expr(&arg.inner)?;
                    is_first = false;
                }
                self.write_str(")")?;
            }

            Expr::UnOp { op_span, body, .. } => {
                self.write_span(*op_span)?;
                self.write_str(" ")?;
                self.write_expr(body)?;
            }

            Expr::BinOp {
                op_span, lhs, rhs, ..
            } => {
                self.write_expr(lhs)?;
                self.write_str(" ")?;
                self.write_span(*op_span)?;
                self.write_str(" ")?;
                self.write_expr(rhs)?;
            }
            todo => unimplemented!("Fmt not implemented for {todo:?}"),
        }
        Ok(())
    }

    pub fn write_seqs(&mut self, elements: &[Prefixed<Seq>]) -> Result {
        self.indent += 2;
        for elem in elements.iter() {
            self.write_non_code(&elem.prefix)?;
            self.write_indent()?;
            self.write_seq(&elem.inner)?;
        }
        self.indent -= 2;
        self.write_indent()
    }

    pub fn write_seq(&mut self, seq: &Seq) -> Result {
        match seq {
            Seq::Elem { value, .. } => {
                self.write_expr(value)?;
                self.write_str(",\n")?;
            }

            Seq::AssocExpr { field, value, .. } => {
                self.write_expr(field)?;
                self.write_str(": ")?;
                self.write_expr(value)?;
                self.write_str(",\n")?;
            }

            Seq::AssocIdent { field, value, .. } => {
                self.write_span(*field)?;
                self.write_str(" = ")?;
                self.write_expr(value)?;
                self.write_str(";\n")?;
            }

            Seq::Let {
                ident, value, body, ..
            } => {
                self.write_str("let ")?;
                self.write_span(*ident)?;
                self.write_str(" = ")?;
                self.write_expr(value)?;
                self.write_str(";\n")?;
                self.write_non_code(&body.prefix)?;
                self.write_indent()?;
                self.write_seq(&body.inner)?;
            }

            Seq::For {
                idents,
                collection,
                body,
                ..
            } => {
                self.write_str("for ")?;
                let mut is_first = true;
                for ident in idents.iter() {
                    if !is_first {
                        self.write_str(", ")?;
                    }
                    self.write_span(*ident)?;
                    is_first = false;
                }
                self.write_str(" in ")?;
                self.write_expr(collection)?;
                self.write_str(":\n")?;
                self.write_non_code(&body.prefix)?;
                self.write_indent()?;
                self.write_seq(&body.inner)?;
            }

            Seq::If {
                condition, body, ..
            } => {
                self.write_str("if ")?;
                self.write_expr(condition)?;
                self.write_str(":\n")?;
                self.write_non_code(&body.prefix)?;
                self.write_indent()?;
                self.write_seq(&body.inner)?;
            }
        }
        Ok(())
    }
}
