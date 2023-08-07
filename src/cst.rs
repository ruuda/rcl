// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The Concrete Syntax Tree.
//!
//! The Concrete Syntax Tree (CST) is one level lower than the Abstract Syntax
//! Tree (AST). It represents the input document more precisely, which makes it
//! less suitable to feed into a typechecker or evaluator, but more suitable for
//! to feed into an autoformatter. For instance, it preserves comments.
//!
//! The CST does not account for every byte in the input document. For example,
//! it does not record whitespace that it would not respect anyway when
//! formatting, and it does not record the spans for some keywords that are
//! implied by the structure of the tree. For example, for a let-binding, it
//! does not store the span of the `let` keyword nor of the `=` after the name.

use crate::source::Span;

/// Not code, but a piece of the document relevant to preserve for formatting.
#[derive(Debug)]
pub enum NonCode {
    /// One or more blank lines.
    Blank(Span),

    /// A comment that runs until the end of the line.
    LineComment(Span),
}

/// An inner node that might be preceded by non-code.
#[derive(Debug)]
pub struct Prefixed<T> {
    /// Non-code that precedes the tree node.
    pub prefix: Box<[NonCode]>,

    /// The tree node itself.
    pub inner: T,
}

#[derive(Debug)]
pub enum Expr {
    /// A let-binding that binds `value` to the name `ident` in `body`.
    Let {
        ident: Span,
        value: Box<Expr>,
        body: Box<Prefixed<Expr>>,
    },

    /// A `{}`-enclosed collection literal.
    BraceLit {
        open: Span,
        close: Span,
        elements: Box<[Prefixed<Seq>]>,
    },

    /// A `[]`-encosed collection literal.
    BracketLit {
        open: Span,
        close: Span,
        elements: Box<[Prefixed<Seq>]>,
    },

    /// An expression enclosed in `()`.
    Parens {
        open: Span,
        close: Span,
        body: Box<Expr>,
    },

    String(Span),
}

/// An inner element of a collection literal.
#[derive(Debug)]
pub enum Seq {
    /// A single element.
    Elem { value: Box<Expr> },

    /// A `key: value` mapping, where the key is an expression.
    AssocExpr { field: Box<Expr>, value: Box<Expr> },

    /// A `key = value` mapping, where the key is an identifier.
    AssocIdent { field: Span, value: Box<Expr> },

    /// Let in the middle of a sequence literal.
    ///
    /// This is syntactically different from a let before an expression, because
    /// associations are not first-class values.
    Let {
        ident: Span,
        value: Box<Expr>,
        body: Box<Prefixed<Seq>>,
    },

    /// Loop over the collection, binding the values to `idents`.
    For {
        idents: Box<[Span]>,
        collection: Box<Expr>,
        body: Box<Prefixed<Seq>>,
    },

    /// Enter the loop only if the condition is true.
    If {
        condition: Box<Expr>,
        body: Box<Prefixed<Seq>>,
    },
}
