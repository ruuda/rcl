// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An auto-formatter for pretty-printing CST nodes.
//!
//! The formatter converts the CST into a [`Doc`], which can subsequently be
//! pretty-printed for formatting.

use crate::cst::{Chain, Expr, NonCode, Prefixed, Seq, Stmt, StringPart, Type};
use crate::lexer::{QuoteStyle, StringPrefix};
use crate::markup::Markup;
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

    /// A soft break if the collection is not empty.
    ///
    /// This is used in collection literals. If there are elements, then we have
    /// a soft break between the opening delimiter and content, and between the
    /// content and closing delimiter. But if we have no content, then we need
    /// only one soft break.
    pub fn soft_break_if_not_empty<T>(&self, elems: &[T]) -> Doc<'a> {
        if elems.is_empty() {
            Doc::Empty
        } else {
            Doc::SoftBreak
        }
    }

    pub fn non_code(&self, nc: &[NonCode]) -> Doc<'a> {
        let mut result = Vec::new();

        for line in nc {
            match line {
                NonCode::Blank(..) => {
                    result.push(Doc::HardBreak);
                }
                NonCode::LineComment(span) => {
                    result.push(self.span(*span).with_markup(Markup::Comment));
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
        let mut result = vec![Doc::str(open).with_markup(Markup::String)];

        for part in parts {
            match part {
                StringPart::String(span) => {
                    result.push(self.raw_span(*span).with_markup(Markup::String));
                }
                StringPart::Escape(span, _esc) => {
                    result.push(self.span(*span).with_markup(Markup::Escape));
                }
                StringPart::Hole(_span, expr) => {
                    // TODO: Add soft breaks around the hole contents?
                    result.push(Doc::str("{").with_markup(Markup::Escape));
                    result.push(self.expr(expr));
                    result.push(Doc::str("}").with_markup(Markup::Escape));
                }
            }
        }

        result.push(Doc::str("\"").with_markup(Markup::String));

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

        if !line.is_empty() {
            out.push(Doc::str(line).with_markup(Markup::String));
        }

        for _ in 0..n_trailing_spaces {
            out.push(Doc::str(r"\u0020").with_markup(Markup::Escape));
        }
    }

    /// Format a `"""` or `f"""` quoted string or format string.
    fn string_triple(&self, open: &'static str, parts: &[StringPart]) -> Doc<'a> {
        let n_strip = string::count_common_leading_spaces(self.input, parts);
        let mut result = vec![Doc::str(open).with_markup(Markup::String)];

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
                        _ => result.push(Doc::from(line).with_markup(Markup::String)),
                    }
                }
                StringPart::Escape(span, _esc) => {
                    result.push(self.span(*span).with_markup(Markup::Escape));
                }
                StringPart::Hole(_span, expr) => {
                    // TODO: Add soft breaks around the hole contents?
                    result.push(Doc::str("{").with_markup(Markup::Escape));
                    result.push(self.expr(expr));
                    result.push(Doc::str("}").with_markup(Markup::Escape));
                }
            }
        }

        result.push(Doc::str("\"\"\"").with_markup(Markup::String));

        flush_indent! { Doc::Concat(result) }
    }

    pub fn prefixed_expr(&self, expr: &Prefixed<Expr>) -> Doc<'a> {
        concat! {
            self.non_code(&expr.prefix)
            self.expr(&expr.inner)
        }
    }

    pub fn prefixed_type(&self, type_: &Prefixed<Type>) -> Doc<'a> {
        concat! {
            self.non_code(&type_.prefix)
            self.type_(&type_.inner)
        }
    }

    pub fn stmt(&self, stmt: &Stmt) -> Doc<'a> {
        // TODO: Make statement chains a first class construct, so we can format
        // them either wide or tall.
        match stmt {
            Stmt::Let {
                ident,
                value,
                type_,
                ..
            } => {
                let mut result: Vec<Doc<'a>> = Vec::new();
                result.push(Doc::str("let").with_markup(Markup::Keyword));
                result.push(" ".into());
                result.push(self.span(*ident));
                if let Some(t) = type_ {
                    result.push(": ".into());
                    result.push(self.type_(t));
                }
                result.push(" = ".into());
                result.push(self.expr(value));
                result.push(";".into());
                Doc::Concat(result)
            }
            Stmt::Assert {
                condition, message, ..
            } => {
                concat! {
                    Doc::str("assert").with_markup(Markup::Keyword)
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
                concat! {
                    Doc::str("trace").with_markup(Markup::Keyword)
                    " "
                    self.expr(message) ";"
                }
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
                        Doc::str("import").with_markup(Markup::Keyword)
                        indent! {
                            Doc::Sep
                            self.non_code(&path.prefix)
                            self.expr(&path.inner)
                        }
                    }
                }
            }

            Expr::BraceLit {
                elements, suffix, ..
            } => {
                if elements.is_empty() && suffix.is_empty() {
                    Doc::str("{}")
                } else {
                    group! {
                        "{"
                        self.soft_break_if_not_empty(elements)
                        indent! { self.seqs(elements, suffix) }
                        "}"
                    }
                }
            }

            Expr::BracketLit {
                elements, suffix, ..
            } => {
                if elements.is_empty() && suffix.is_empty() {
                    Doc::str("[]")
                } else {
                    group! {
                        "["
                        self.soft_break_if_not_empty(elements)
                        indent! { self.seqs(elements, suffix) }
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
                Doc::string(span.resolve(self.input).to_ascii_lowercase())
                    .with_markup(Markup::Number)
            }

            Expr::NumBinary(span) => self.span(*span).with_markup(Markup::Number),

            Expr::NumDecimal(span) => {
                // Normalize exponent E to e.
                Doc::string(span.resolve(self.input).to_ascii_lowercase())
                    .with_markup(Markup::Number)
            }

            Expr::Var(span) => self.span(*span),

            Expr::IfThenElse {
                condition,
                then_body,
                else_body,
                ..
            } => {
                group! {
                    flush_indent! {
                        Doc::str("if").with_markup(Markup::Keyword)
                        " "
                        self.expr(condition) ":"
                        Doc::Sep
                        indent! { self.prefixed_expr(then_body) }
                        Doc::Sep
                        Doc::str("else").with_markup(Markup::Keyword)
                        Doc::Sep
                        indent! { self.prefixed_expr(else_body) }
                    }
                }
            }

            Expr::Function {
                args, suffix, body, ..
            } => {
                let args_doc: Doc = match args.len() {
                    0 => Doc::str("()"),
                    // Don't put parens around the argument if there is a single
                    // argument that has no comments on it. If it has comments,
                    // then we need the parens, because otherwise we might
                    // produce a syntax error in the output.
                    1 if args[0].prefix.is_empty() && suffix.is_empty() => self.span(args[0].inner),
                    _ => group! {
                        "("
                        Doc::SoftBreak
                        indent! {
                            Doc::join(
                                args.iter().map(|arg| concat! {
                                    self.non_code(&arg.prefix)
                                    self.span(arg.inner)
                                }),
                                concat!{ "," Doc::Sep },
                            )
                            Doc::tall(",")
                            Doc::SoftBreak
                            self.non_code(suffix)
                        }
                        ")"
                    },
                };
                concat! {
                    args_doc " => " self.expr(body)
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

            Expr::Chain {
                base_expr, chain, ..
            } => self.chain(base_expr, chain),
        }
    }

    /// Format a chained expression.
    pub fn chain(&self, base: &Expr, chain: &[(Span, Chain)]) -> Doc<'a> {
        // Every field should start a new line. When we have a call or index,
        // before the first field they go on the base, afterward they go on
        // "next", the part after base. The "next" part needs to be indented
        // but the base part is not.
        let mut group_base = Vec::new();
        let mut group_next = Vec::new();
        let mut group = &mut group_base;

        group.push(self.expr(base));

        for (_, chain_elem) in chain.iter() {
            match chain_elem {
                Chain::Field { field } => {
                    group = &mut group_next;
                    group.push(Doc::SoftBreak);
                    group.push(".".into());
                    group.push(self.span(*field));
                }
                Chain::Call { args, suffix, .. } => {
                    let call_doc = group! {
                        "("
                        Doc::SoftBreak
                        indent! {
                            Doc::join(
                                args.iter().map(|(_span, arg)| self.prefixed_expr(arg)),
                                concat!{ "," Doc::Sep },
                            )
                            Doc::tall(",")
                            Doc::SoftBreak
                            self.non_code(suffix)
                        }
                        ")"
                    };
                    group.push(call_doc);
                }

                Chain::Index { index, .. } => {
                    let index_doc = group! {
                        "["
                        Doc::SoftBreak
                        indent! { self.prefixed_expr(index) }
                        Doc::SoftBreak
                        "]"
                    };
                    group.push(index_doc);
                }
            }
        }

        group_base.push(indent! { Doc::Concat(group_next) });
        group! { Doc::Concat(group_base) }
    }

    pub fn seqs(&self, elements: &[Prefixed<Seq>], suffix: &[NonCode]) -> Doc<'a> {
        let mut result = Vec::new();
        for (i, elem) in elements.iter().enumerate() {
            let elem_doc = self.seq(&elem.inner);

            // We wrap the inner Seq in a group, so you can have a collection
            // that consists of multiple comprehensions, and each one fits on
            // a line, so they are all formatted wide, but the collection itself
            // is formatted tall.
            result.push(self.non_code(&elem.prefix));
            result.push(group! { elem_doc });

            let is_last = i + 1 == elements.len();
            let sep_doc = match i {
                // For collections that contain a single comprehension, do not
                // add a separator, even when they are multi-line. It makes
                // comprehensions look weird, which are regularly multi-line
                // but only rarely are there multiple seqs in the collection.
                // If there is suffix noncode, then we need the separator before
                // it, otherwise we would output a syntax error.
                _ if elements.len() == 1 && suffix.is_empty() => match elements[0].inner {
                    Seq::Elem { .. } | Seq::AssocExpr { .. } | Seq::AssocIdent { .. } => {
                        Doc::tall(",")
                    }
                    Seq::For { .. } | Seq::If { .. } | Seq::Stmt { .. } => Doc::Empty,
                },
                _ if is_last => Doc::tall(","),
                _ => Doc::str(","),
            };
            result.push(sep_doc);

            if !is_last {
                result.push(Doc::Sep)
            }
        }

        result.push(Doc::SoftBreak);

        // We could do it non-conditionally and push an empty doc, but seq is
        // a very common thing and suffixes are not, so efficiency matters here.
        if !suffix.is_empty() {
            result.push(self.non_code(suffix));
        }

        Doc::Concat(result)
    }

    /// Format a sequence.
    pub fn seq(&self, seq: &Seq) -> Doc<'a> {
        match seq {
            Seq::Elem { value, .. } => self.expr(value),

            Seq::AssocExpr { field, value, .. } => {
                // TODO: Special-case an inner string for markup?
                concat! { self.expr(field).with_markup(Markup::Field) ": " self.expr(value) }
            }

            Seq::AssocIdent { field, value, .. } => {
                concat! { self.span(*field).with_markup(Markup::Field) " = " self.expr(value) }
            }

            Seq::Stmt { stmt, body, .. } => {
                let body_doc = self.seq(&body.inner);
                concat! {
                    self.stmt(stmt)
                    Doc::Sep
                    self.non_code(&body.prefix)
                    body_doc
                }
            }

            Seq::For {
                idents,
                collection,
                body,
                ..
            } => {
                let body_doc = self.seq(&body.inner);
                concat! {
                    Doc::str("for").with_markup(Markup::Keyword)
                    " "
                    // TODO: This does not use a proper sep, which means we
                    // cannot break this over multiple lines. But maybe that's
                    // okay.
                    Doc::join(
                        idents.iter().map(|ident| self.span(*ident)),
                        ", ".into(),
                    )
                    " "
                    Doc::str("in").with_markup(Markup::Keyword)
                    " "
                    self.expr(collection)
                    ":"
                    Doc::Sep
                    self.non_code(&body.prefix)
                    body_doc
                }
            }

            Seq::If {
                condition, body, ..
            } => {
                let body_doc = self.seq(&body.inner);
                concat! {
                    Doc::str("if").with_markup(Markup::Keyword)
                    " "
                    self.expr(condition)
                    ":"
                    Doc::Sep
                    self.non_code(&body.prefix)
                    body_doc
                }
            }
        }
    }

    pub fn type_(&self, type_: &Type) -> Doc<'a> {
        match type_ {
            Type::Term(span) => self.span(*span),
            Type::Apply { name, args, .. } => concat! {
                self.span(*name)
                self.types("[", args, "]")
            },
            Type::Function { args, result, .. } => concat! {
                self.types("(", args, ")")
                " -> "
                self.type_(result)
            },
        }
    }

    /// A list of types enclosed by opening and closing delimiters.
    pub fn types(
        &self,
        open: &'static str,
        types: &[Prefixed<Type>],
        close: &'static str,
    ) -> Doc<'a> {
        group! {
            open
            Doc::SoftBreak
            indent! {
                Doc::join(
                    types.iter().map(|t| self.prefixed_type(t)),
                    concat!{ "," Doc::Sep },
                )
                Doc::tall(",")
            }
            Doc::SoftBreak
            close
        }
    }
}
