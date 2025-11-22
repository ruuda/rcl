// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An auto-formatter for pretty-printing CST nodes.
//!
//! The formatter converts the CST into a [`Doc`], which can subsequently be
//! pretty-printed for formatting.

use crate::ast::UnOp;
use crate::cst::{
    Chain, Expr, List, NonCode, Prefixed, Seq, SeqControl, Stmt, StringPart, Type, Yield,
};
use crate::lexer::{QuoteStyle, StringPrefix};
use crate::markup::Markup;
use crate::pprint::{concat, flush_indent, group, indent, Doc};
use crate::source::{Inputs, Span};
use crate::string;

/// Format a document.
pub fn format_expr<'a>(inputs: &'a Inputs<'a>, expr: &'a Expr) -> Doc<'a> {
    Formatter::new(inputs).expr(expr)
}

/// Helper so we can use methods for resolving spans against the input.
struct Formatter<'a> {
    // TODO: This could all be more efficient if we resolved on bytestrings, so
    // the code point slicing check can be omitted.
    inputs: &'a Inputs<'a>,
}

impl<'a> Formatter<'a> {
    pub fn new(inputs: &'a Inputs<'a>) -> Self {
        Self { inputs }
    }

    /// Format the span as-is. It should not contain newlines.
    pub fn span(&self, span: Span) -> Doc<'a> {
        span.resolve(self.inputs).into()
    }

    /// Format a span that may contain newlines using raw line breaks.
    pub fn raw_span(&self, span: Span) -> Doc<'a> {
        let mut inner = span.resolve(self.inputs);
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

    /// The separator to add after a collection's opening punctuation.
    ///
    /// This is used in collection literals. If there are elements, then we have
    /// a hard or soft break between the opening delimiter and content, and
    /// between the content and closing delimiter. But if we have no content
    /// (but possibly a suffix) then we need only one soft break.
    pub fn collection_opening_sep<T>(&self, list: &List<T>) -> Option<Doc<'a>> {
        // When there is a trailing comma, then regardless of whether the list
        // would fit in wide mode, we force it to be tall, to give the user some
        // control over wide/tall. This is inspired by Black's "magic trailing comma":
        // https://black.readthedocs.io/en/stable/the_black_code_style/current_style.html#the-magic-trailing-comma
        match list.elements.len() {
            0 => None,
            _ if list.trailing_comma => Some(Doc::HardBreak),
            _ => Some(Doc::SoftBreak),
        }
    }

    /// Special case of [`collection_opening_sep`] for `Seq`.
    ///
    /// For singleton comprehensions, we do not add a trailing comma. This means
    /// that we should not format wide/tall based on the presence of a trailing
    /// comma for singleton list comprehensions.
    pub fn seq_opening_sep(&self, list: &List<Seq>) -> Option<Doc<'a>> {
        match list.elements.len() {
            1 if list.elements[0].has_control() => Some(Doc::SoftBreak),
            _ => self.collection_opening_sep(list),
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

    /// The final trailing comma in a collection (also call args, type args, etc.).
    ///
    /// Also includes the soft break after the comma.
    pub fn trailing_comma<T>(&self, list: &List<T>) -> Doc<'a> {
        if !list.suffix.is_empty() {
            return concat! {
                ","
                Doc::SoftBreak
                self.non_code(&list.suffix)
            };
        }

        if list.elements.is_empty() {
            return Doc::SoftBreak;
        }

        concat! {
            Doc::tall(",")
            Doc::SoftBreak
        }
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
                    result.push(Doc::str("{").with_markup(Markup::Escape));
                    // We need soft breaks here in case the expression contains
                    // forced breaks, for example when it has non-code.
                    result.push(group! {
                        indent! {
                            Doc::SoftBreak
                            self.expr(expr)
                            Doc::SoftBreak
                        }
                    });
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
        let mut result = vec![Doc::str(open).with_markup(Markup::String)];

        // We assume that all parts are spans from the same document, and we
        // resolve it once at the start. Due to the `crate::patch` feature, we
        // can get CSTs that mix documents, but within syntax nodes (like a
        // string), there should not be any mixing.
        let (input, n_strip) = match parts.first() {
            Some(StringPart::String(p) | StringPart::Escape(p, ..) | StringPart::Hole(p, ..)) => {
                let input = self.inputs[p.doc()].data;
                let n_strip = string::count_common_leading_spaces(input, parts);
                (input, n_strip)
            }
            None => ("", 0),
        };

        for (i, part) in parts.iter().enumerate() {
            match part {
                StringPart::String(span) => {
                    let mut line = span.resolve(input);

                    // If we are at the start of a new line, strip the leading
                    // whitespace and emit the line break to the document.
                    if line.starts_with('\n') {
                        result.push(Doc::HardBreak);
                        line = span.trim_start(1 + n_strip).resolve(input);
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
                    result.push(Doc::str("{").with_markup(Markup::Escape));
                    // We need soft breaks here in case the expression contains
                    // forced breaks, for example when it has non-code.
                    result.push(group! {
                        indent! {
                            Doc::SoftBreak
                            self.expr(expr)
                            Doc::SoftBreak
                        }
                    });
                    result.push(Doc::str("}").with_markup(Markup::Escape));
                }
            }
        }

        result.push(Doc::str("\"\"\"").with_markup(Markup::String));

        flush_indent! { Doc::Concat(result) }
    }

    pub fn prefixed_type(&self, type_: &Prefixed<Type>) -> Doc<'a> {
        concat! {
            self.non_code(&type_.prefix)
            self.type_(&type_.inner)
        }
    }

    pub fn stmt(&self, stmt: &Stmt) -> Doc<'a> {
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
                group! {
                    Doc::str("assert").with_markup(Markup::Keyword)
                    " "
                    self.expr(condition)
                    ":"
                    Doc::Sep
                    indent! { self.expr(message) ";" }
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
            Expr::Statements { stmts, body, .. } => {
                let mut parts = Vec::new();
                // If we have two or more statements, then always format tall,
                // even if it would fit on a line. If you have that many
                // statements it's probably complex, a oneliner might make it
                // hard to understand.
                let sep = if stmts.len() >= 2 {
                    Doc::HardBreak
                } else {
                    Doc::Sep
                };
                for (_span, stmt) in stmts.iter() {
                    parts.push(self.non_code(&stmt.prefix));
                    parts.push(self.stmt(&stmt.inner));
                    parts.push(sep.clone());
                }
                parts.push(self.non_code(&body.prefix));
                parts.push(self.expr(&body.inner));
                group! { flush_indent! { Doc::Concat(parts) } }
            }

            Expr::Import { path, .. } => {
                group! {
                    concat! {
                        Doc::str("import").with_markup(Markup::Keyword)
                        indent! {
                            Doc::Sep
                            self.expr(path)
                        }
                    }
                }
            }

            Expr::BraceLit { elements, .. } => {
                if elements.elements.is_empty() && elements.suffix.is_empty() {
                    Doc::str("{}")
                } else {
                    let sep = match self.seq_opening_sep(elements) {
                        Some(Doc::HardBreak) => Some(Doc::HardBreak),
                        other => self.sep_key_value(&elements.elements).or(other),
                    };
                    group! {
                        "{"
                        sep
                        indent! { self.seqs(elements) }
                        "}"
                    }
                }
            }

            Expr::BracketLit { elements, .. } => {
                if elements.elements.is_empty() && elements.suffix.is_empty() {
                    Doc::str("[]")
                } else {
                    group! {
                        "["
                        self.seq_opening_sep(elements)
                        indent! { self.seqs(elements) }
                        "]"
                    }
                }
            }

            Expr::Parens { body, .. } => {
                group! {
                    "("
                    Doc::SoftBreak
                    indent! { self.expr(body) }
                    Doc::SoftBreak
                    ")"
                }
            }

            Expr::NullLit(span) => self.span(*span).with_markup(Markup::Keyword),

            Expr::BoolLit(span, ..) => self.span(*span).with_markup(Markup::Keyword),

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
                Doc::string(span.resolve(self.inputs).to_ascii_lowercase())
                    .with_markup(Markup::Number)
            }

            Expr::NumBinary(span) => self.span(*span).with_markup(Markup::Number),

            Expr::NumDecimal(span) => {
                // Normalize exponent E to e.
                Doc::string(span.resolve(self.inputs).to_ascii_lowercase())
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
                        indent! { self.expr(then_body) }
                        Doc::Sep
                        Doc::str("else").with_markup(Markup::Keyword) ":"
                        Doc::Sep
                        indent! { self.expr(else_body) }
                    }
                }
            }

            Expr::Function { args, body, .. } => {
                let args_doc: Doc = match args.elements.len() {
                    0 => Doc::str("()"),
                    // Don't put parens around the argument if there is a single
                    // argument that has no comments on it. If it has comments,
                    // then we need the parens, because otherwise we might
                    // produce a syntax error in the output.
                    1 if args.elements[0].prefix.is_empty() && args.suffix.is_empty() => {
                        self.span(args.elements[0].inner)
                    }
                    _ => group! {
                        "("
                        self.collection_opening_sep(args)
                        indent! {
                            Doc::join(
                                args.elements.iter().map(|arg| concat! {
                                    self.non_code(&arg.prefix)
                                    self.span(arg.inner)
                                }),
                                concat!{ "," Doc::Sep },
                            )
                            self.trailing_comma(args)
                        }
                        ")"
                    },
                };
                concat! {
                    args_doc " => " self.expr(body)
                }
            }

            Expr::UnOp { op, body, .. } => match op {
                UnOp::Neg => concat! {
                    "-" self.expr(body)
                },
                UnOp::Not => concat! {
                    Doc::from("not").with_markup(Markup::Keyword)
                    " "
                    self.expr(body)
                },
            },

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
                Chain::Call { args, .. } => {
                    let call_doc = group! {
                        "("
                        self.collection_opening_sep(args)
                        indent! {
                            Doc::join(
                                args.elements.iter().map(|(_span, arg)| self.expr(arg)),
                                concat!{ "," Doc::Sep },
                            )
                            self.trailing_comma(args)
                        }
                        ")"
                    };
                    group.push(call_doc);
                }

                Chain::Index { index, .. } => {
                    let index_doc = group! {
                        "["
                        Doc::SoftBreak
                        indent! { self.expr(index) }
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

    /// If the elements start or end with a key-value, return a separator, otherwise empty string.
    ///
    /// This is so that `{ a = 10 }` formats with spaces, but `{a, 10}` does not.
    fn sep_key_value(&self, elements: &[Seq]) -> Option<Doc<'a>> {
        match elements.first().map(|x| x.is_inner_elem()) {
            Some(false) => Some(Doc::Sep),
            Some(true) => None,
            None => None,
        }
    }

    pub fn seqs(&self, seqs: &List<Seq>) -> Doc<'a> {
        let mut result = Vec::new();
        for (i, elem) in seqs.elements.iter().enumerate() {
            result.push(self.seq(elem));

            let is_last = i + 1 == seqs.elements.len();
            let sep_doc = match i {
                // If there is suffix noncode, then we need the separator before
                // it, otherwise we would output a syntax error.
                _ if !seqs.suffix.is_empty() => Doc::str(","),
                // For collections that contain a single comprehension, do not
                // add a separator, even when they are multi-line. It makes
                // comprehensions look weird, which are regularly multi-line
                // but only rarely are there multiple seqs in the collection.
                _ if seqs.elements.len() == 1 => match seqs.elements[0].has_control() {
                    true => Doc::Empty,
                    false => Doc::tall(","),
                },
                _ if is_last => Doc::tall(","),
                _ => Doc::str(","),
            };
            result.push(sep_doc);

            if !is_last {
                result.push(Doc::Sep)
            }
        }

        // Depending on whether we have key-values, add a soft break or sep.
        result.push(self.sep_key_value(&seqs.elements).unwrap_or(Doc::SoftBreak));

        // We could do it non-conditionally and push an empty doc, but seq is
        // a very common thing and suffixes are not, so efficiency matters here.
        if !seqs.suffix.is_empty() {
            result.push(self.non_code(&seqs.suffix));
        }

        Doc::Concat(result)
    }

    /// Format a yield inside a sequence.
    pub fn yield_(&self, yield_: &Yield) -> Doc<'a> {
        match yield_ {
            Yield::Elem { value, .. } => self.expr(value),

            Yield::AssocExpr { field, value, .. } => {
                // TODO: Special-case an inner string for markup?
                concat! { self.expr(field).with_markup(Markup::Field) ": " self.expr(value) }
            }

            Yield::AssocIdent { field, value, .. } => {
                concat! { self.span(*field).with_markup(Markup::Field) " = " self.expr(value) }
            }

            Yield::UnpackElems { collection, .. } => concat! {
                ".." self.expr(collection)
            },

            Yield::UnpackAssocs { collection, .. } => concat! {
                "..." self.expr(collection)
            },
        }
    }

    /// Format a control item inside a sequence.
    pub fn seq_control(&self, control: &SeqControl) -> Doc<'a> {
        match control {
            SeqControl::Stmt { stmt } => self.stmt(stmt),
            SeqControl::For {
                idents, collection, ..
            } => concat! {
                Doc::str("for").with_markup(Markup::Keyword)
                // Note, we use regular spaces here not, Doc::Sep.
                // That means we don't break this over multiple lines,
                // which so far seems fine.
                " "
                Doc::join(
                    idents.iter().map(|ident| self.span(*ident)),
                    ", ".into(),
                )
                " "
                Doc::str("in").with_markup(Markup::Keyword)
                " "
                self.expr(collection)
                ":"
            },
            SeqControl::If { condition, .. } => concat! {
                Doc::str("if").with_markup(Markup::Keyword)
                " "
                self.expr(condition)
                ":"
            },
        }
    }

    /// Format an element in a sequence (which can be a comprehension).
    pub fn seq(&self, seq: &Seq) -> Doc<'a> {
        // If we have a seq with multiple control items (statements or control
        // flow), even though it *could* fit on one line, this is a complex
        // thing akin to a nested for loop, usually it gets more readable if we
        // spread it across multiple lines. So if the seq is deeper than 2,
        // force it to be tall.
        let sep = if seq.control.len() > 1 {
            Doc::HardBreak
        } else {
            Doc::Sep
        };

        let mut parts = Vec::with_capacity(2 + seq.control.len() * 3);

        for control in seq.control.iter() {
            parts.push(self.non_code(&control.prefix));
            parts.push(self.seq_control(&control.inner));
            parts.push(sep.clone());
        }

        parts.push(self.non_code(&seq.body.prefix));
        parts.push(self.yield_(&seq.body.inner));

        // The seq itself is a group: you can have a collection which is tall,
        // but the seq inside can be wide. However, for the initial non-code,
        // we don't want the presence of a comment to force tall mode, so we
        // take that one out of the group.
        let prefix = parts.remove(0);
        let group = group! { Doc::Concat (parts) };
        concat! { prefix group }
    }

    pub fn type_(&self, type_: &Type) -> Doc<'a> {
        match type_ {
            Type::Term(span) => self.span(*span).with_markup(Markup::Type),
            Type::Apply { name, args, .. } => concat! {
                self.span(*name).with_markup(Markup::Type)
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
        types: &List<Prefixed<Type>>,
        close: &'static str,
    ) -> Doc<'a> {
        group! {
            open
            self.collection_opening_sep(types)
            indent! {
                Doc::join(
                    types.elements.iter().map(|t| self.prefixed_type(t)),
                    concat!{ "," Doc::Sep },
                )
                self.trailing_comma(types)
            }
            close
        }
    }
}
