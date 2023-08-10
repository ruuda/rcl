// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An auto-formatter for pretty-printing CST nodes.

use std::io::{Write};

use crate::source::{Span};
use crate::cst::{Expr, NonCode, Prefixed, Seq};

pub type Result = std::io::Result<()>;

/// Write a formatted expression to the output.
pub fn write_expr(input: &str, expr: &Prefixed<Expr>, out: &mut dyn Write) -> Result {
    let mut formatter = Formatter::new(input, out);
    formatter.write_prefixed_expr(expr)
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

    pub fn write_span(&mut self, span: Span) -> Result {
        self.out.write_all(span.resolve(self.input).as_bytes())
    }

    pub fn write_non_code(&mut self, nc: &[NonCode]) -> Result {
        for line in nc {
            match line {
                NonCode::Blank(..) => writeln!(self.out)?,
                NonCode::LineComment(span) => {
                    self.write_indent()?;
                    writeln!(self.out, "{}", span.resolve(self.input))?;
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
            Expr::Let { ident, value, body } => {
                write!(self.out, "let ")?;
                self.write_span(*ident)?;
                write!(self.out, " = ")?;
                self.write_expr(value)?;
                writeln!(self.out, ";")?;
                self.write_non_code(&body.prefix)?;
                self.write_indent()?;
                self.write_expr(&body.inner)?;
            }
            todo => unimplemented!("Fmt not implemented for {todo:?}"),
        }
        Ok(())
    }
}
