// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An auto-formatter for pretty-printing CST nodes.
//!
//! The formatter converts the CST into a [`Doc`], which can subsequently be
//! pretty-printed for formatting.

use crate::cst::{Expr, NonCode, Prefixed, Seq, Stmt, StringPart};
use crate::lexer::{QuoteStyle, StringPrefix};
use crate::pprint::{concat, flush_indent, group, indent, Doc};
use crate::source::Span;
use crate::string;

/// Format a document.
pub fn format_expr<'a>(input: &'a str, expr: &'a Prefixed<Expr>) -> Doc<'a> {
    let formatter = Formatter::new(input);
    // Usually the entire thing is already wrapped in a group, but we need to
    // add one in case it is not, to enable wide formatting of expressions that
    // are not a group at the top level.
    Doc::Group(Box::new(formatter.prefixed_expr(expr)))
}

/// Helper so we can use methods for resolving spans against the input.
struct Formatter<'a> {
    // TODO: This could all be more efficient if we resolved on bytestrings, so
    // the code point slicing check can be omitted.
    input: &'a str,
}

impl<'a> Formatter<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    /// Format the span as-is. It should not contain newlines.
    pub fn span(&self, span: Span) -> Doc<'a> {
        span.resolve(self.input).into()
    }

    /// Format a span that may contain newlines using raw line breaks.
    pub fn raw_span(&self, span: Span) -> Doc<'a> {
        let mut inner = span.resolve(self.input);
        if !inner.contains('\n') {
            return inner.into();
        }

        let mut result = Vec::new();

        while let Some(i) = inner.find('\n') {
            result.push(inner[..i].into());
            result.push(Doc::RawBreak);
            inner = &inner[i + 1..];
        }

        result.push(inner.into());

        Doc::Concat(result)
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
                NonCode::Shebang(span) => {
                    result.push(self.span(*span));
                    result.push(Doc::HardBreak);
                }
            }
        }

        Doc::Concat(result)
    }

    /// Format a `"` or `f"` quoted string or format string.
    fn string_double(&self, open: &'static str, parts: &[StringPart]) -> Doc<'a> {
        let mut result = vec![open.into()];

        for part in parts {
            match part {
                StringPart::String(span) => {
                    result.push(self.raw_span(*span));
                }
                StringPart::Escape(span, _esc) => {
                    result.push(self.span(*span));
                }
                StringPart::Hole(_span, expr) => {
                    // TODO: Add soft breaks around the hole contents?
                    result.push("{".into());
                    result.push(self.expr(expr));
                    result.push("}".into());
                }
            }
        }

        result.push("\"".into());

        Doc::Concat(result)
    }

    /// Push a line that is part of a `"""` or `f"""` string literal.
    ///
    /// The line contents should not contain `\n`.
    fn push_string_line(&self, line: &'a str, out: &mut Vec<Doc<'a>>) {
        let mut line = line;
        let mut n_trailing_spaces = 0_u32;

        // If there are trailing spaces, the pretty-printer would eat them.
        // We can (and maybe should) fix this in the pretty printer, but
        // trailing spaces are a hazard to humans too, so instead of
        // preserving them verbatim, we escape them to make them visible.
        // This has the nice side effects of removing trailing whitespace,
        // so it doesn't get eaten.
        while !line.is_empty() && line.as_bytes()[line.len() - 1] == b' ' {
            line = &line[..line.len() - 1];
            n_trailing_spaces += 1;
        }

        out.push(line.into());
        for _ in 0..n_trailing_spaces {
            out.push(r"\u0020".into());
        }
    }

    /// Format a `"""` or `f"""` quoted string or format string.
    fn string_triple(&self, open: &'static str, parts: &[StringPart]) -> Doc<'a> {
        let n_strip = string::count_common_leading_spaces(self.input, parts);
        let mut result = vec![open.into()];

        for (i, part) in parts.iter().enumerate() {
            match part {
                StringPart::String(span) => {
                    let mut line = span.resolve(self.input);

                    // If we are at the start of a new line, strip the leading
                    // whitespace and emit the line break to the document.
                    if line.starts_with('\n') {
                        result.push(Doc::HardBreak);
                        line = span.trim_start(1 + n_strip).resolve(self.input);
                    }

                    // If we are at the end of a line, and there is a next line,
                    // then we need to be careful about how to emit the line
                    // without creating trailing whitespace.
                    match parts.get(i + 1) {
                        Some(StringPart::String(..)) => self.push_string_line(line, &mut result),
                        _ => result.push(line.into()),
                    }
                }
                StringPart::Escape(span, _esc) => {
                    result.push(self.span(*span));
                }
                StringPart::Hole(_span, expr) => {
                    // TODO: Add soft breaks around the hole contents?
                    result.push("{".into());
                    result.push(self.expr(expr));
                    result.push("}".into());
                }
            }
        }

        result.push("\"\"\"".into());

        flush_indent! { Doc::Concat(result) }
    }

    pub fn prefixed_expr(&self, expr: &Prefixed<Expr>) -> Doc<'a> {
        concat! {
            self.non_code(&expr.prefix)
            self.expr(&expr.inner)
        }
    }

    pub fn stmt(&self, stmt: &Stmt) -> Doc<'a> {
        // TODO: Make statement chains a first class construct, so we can format
        // them either wide or tall.
        match stmt {
            Stmt::Let { ident, value, .. } => {
                concat! {
                    "let " self.span(*ident) " = " self.expr(value) ";"
                }
            }
            Stmt::Assert {
                condition, message, ..
            } => {
                concat! {
                    "assert"
                    group! {
                        indent! {
                            Doc::Sep
                            self.expr(condition)
                            ","
                            Doc::Sep
                            self.expr(message)
                            ";"
                        }
                    }
                }
            }
            Stmt::Trace { message, .. } => {
                concat! { "trace " self.expr(message) ";" }
            }
        }
    }

    pub fn expr(&self, expr: &Expr) -> Doc<'a> {
        match expr {
            Expr::Stmt { stmt, body, .. } => {
                group! {
                    flush_indent! {
                        self.stmt(stmt)
                        Doc::Sep
                        self.non_code(&body.prefix)
                        self.expr(&body.inner)
                    }
                }
            }

            Expr::Import { path, .. } => {
                group! {
                    concat! {
                        "import"
                        indent! {
                            Doc::Sep
                            self.non_code(&path.prefix)
                            self.expr(&path.inner)
                        }
                    }
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

            Expr::NullLit(span) => self.span(*span),

            Expr::BoolLit(span, ..) => self.span(*span),

            Expr::StringLit {
                prefix: StringPrefix::None,
                style,
                parts,
                ..
            } => match style {
                QuoteStyle::Double => self.string_double("\"", parts),
                QuoteStyle::Triple => self.string_triple("\"\"\"", parts),
            },
            Expr::StringLit {
                prefix: StringPrefix::Format,
                style,
                parts,
                ..
            } => match style {
                QuoteStyle::Double => self.string_double("f\"", parts),
                QuoteStyle::Triple => self.string_triple("f\"\"\"", parts),
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

            Expr::IfThenElse {
                condition,
                then_body,
                else_body,
                ..
            } => {
                group! {
                    flush_indent! {
                        "if" Doc::Sep
                        indent! { self.prefixed_expr(condition) } Doc::Sep
                        "then" Doc::Sep
                        indent! { self.prefixed_expr(then_body) } Doc::Sep
                        "else" Doc::Sep
                        indent! { self.prefixed_expr(else_body) }
                    }
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
                    flush_indent! {
                        self.expr(lhs)
                        Doc::Sep
                        self.span(*op_span)
                        " "
                        self.expr(rhs)
                    }
                }
            }
        }
    }

    pub fn seqs(&self, elements: &[Prefixed<Seq>]) -> Doc<'a> {
        let mut result = Vec::new();
        for (i, elem) in elements.iter().enumerate() {
            let (elem_doc, sep_str) = self.seq(&elem.inner);

            // We wrap the inner Seq in a group, so you can have a collection
            // that consists of multiple comprehensions, and each one fits on
            // a line, so they are all formatted wide, but the collection itself
            // is formatted tall.
            result.push(self.non_code(&elem.prefix));
            result.push(group! { elem_doc });

            let is_last = i + 1 == elements.len();
            let sep_doc = match i {
                // For collections that contain a single seq, do not add a
                // separator, even when they are multi-line. It makes
                // comprehensions look weird, which are regularly multi-line but
                // only rarely are there multiple seqs in the collection.
                _ if elements.len() == 1 => Doc::empty(),
                _ if is_last => Doc::tall(sep_str),
                _ => Doc::str(sep_str),
            };
            result.push(sep_doc);

            if !is_last {
                result.push(Doc::Sep)
            }
        }
        Doc::Concat(result)
    }

    /// Format a sequence. Return that and the trailing separator.
    pub fn seq(&self, seq: &Seq) -> (Doc<'a>, &'static str) {
        match seq {
            Seq::Elem { value, .. } => (self.expr(value), ","),

            Seq::AssocExpr { field, value, .. } => {
                (concat! { self.expr(field) ": " self.expr(value) }, ",")
            }

            Seq::AssocIdent { field, value, .. } => {
                (concat! { self.span(*field) " = " self.expr(value) }, ",")
            }

            Seq::Stmt { stmt, body, .. } => {
                let (body_doc, sep) = self.seq(&body.inner);
                let result = concat! {
                    self.stmt(stmt)
                    Doc::Sep
                    self.non_code(&body.prefix)
                    body_doc
                };
                (result, sep)
            }

            Seq::For {
                idents,
                collection,
                body,
                ..
            } => {
                let (body_doc, sep) = self.seq(&body.inner);
                let result = concat! {
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
                    body_doc
                };
                (result, sep)
            }

            Seq::If {
                condition, body, ..
            } => {
                let (body_doc, sep) = self.seq(&body.inner);
                let result = concat! {
                    "if "
                    self.expr(condition)
                    ":"
                    Doc::Sep
                    self.non_code(&body.prefix)
                    body_doc
                };
                (result, sep)
            }
        }
    }
}
