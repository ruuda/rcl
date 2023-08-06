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

use crate::source::Span;

/// Not code, but a piece of the document relevant to preserve for formatting.
#[derive(Debug)]
pub enum NonCode {
    /// One or more blank lines.
    Blank(Span),

    /// A comment that runs until the end of the line.
    LineComment(Span),
}

#[derive(Debug)]
pub enum Expr {
    /// Non-code that precedes the body.
    NonCode { skip: NonCode, body: Box<Expr> },

    /// A let-binding that binds `value` to the name `ident` in `body`.
    Let {
        ident: Span,
        value: Box<Expr>,
        body: Box<Expr>,
    },

    /// A `{}`-enclosed collection literal.
    BraceLit {
        open: Span,
        close: Span,
        elements: Vec<Seq>,
    },

    /// A `[]`-encosed collection literal.
    BracketLit {
        open: Span,
        close: Span,
        elements: Vec<Seq>,
    },
}

/// An inner element of a collection literal.
#[derive(Debug)]
pub enum Seq {
    /// Non-code that precedes an element.
    NonCode { skip: NonCode },

    /// A single element.
    Elem { value: Box<Expr> },

    /// A `key: value` mapping, where the key is an expression.
    AssocExpr { field: Box<Expr>, value: Box<Expr> },

    /// A `key = value` mapping, where the key is an identifier.
    AssocIdent { field: Span, value: Box<Expr> },

    /// A comprehension that yields elements or mappings.
    Compr(Compr),
}

/// A for-comprehension.
#[derive(Debug)]
pub enum Compr {
    /// Non-code that precedes a clause of the comprehension.
    NonCode { skip: NonCode },

    /// Let in the middle of a comprehension.
    ///
    /// This is syntactically different from a let before an expression, because
    /// associations are not first-class values.
    Let {
        ident: Span,
        value: Box<Expr>,
        body: Box<Seq>,
    },

    /// Loop over the collection, binding the values to `idents`.
    For {
        idents: Vec<Span>,
        collection: Box<Expr>,
        body: Box<Seq>,
    },

    /// Enter the loop only if the condition is true.
    If {
        condition: Box<Expr>,
        body: Box<Seq>,
    },
}
