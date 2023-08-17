// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The Abstract Syntax Tree.

use std::fmt;
use std::rc::Rc;

pub use crate::cst::{BinOp, UnOp};

use crate::source::Span;

/// An identifier.
// TODO: Should we deduplicate idents, or even all strings, in a hash table?
// Should they be slices into the source document? For now the easy thing is to
// just make them strings, we can optimize later.
#[derive(Clone, Eq, PartialEq)]
pub struct Ident(pub Rc<str>);

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl From<&str> for Ident {
    fn from(s: &str) -> Ident {
        Ident(s.to_string().into())
    }
}

/// An expression.
#[derive(Debug)]
pub enum Expr {
    /// A let-binding. First is the bound value, then the result expression.
    Let {
        ident: Ident,
        value: Box<Expr>,
        body: Box<Expr>,
    },

    /// A dict or set literal (depending on the element types) enclosed in `{}`.
    BraceLit(Vec<Seq>),

    /// A list literal enclosed in `[]`.
    BracketLit(Vec<Seq>),

    /// A boolean literal.
    BoolLit(bool),

    /// A string literal.
    StringLit(Rc<str>),

    /// An integer literal.
    /// TODO: This should be a bigint.
    IntegerLit(i64),

    /// An conditional choice (if, then, else).
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),

    /// Access a variable.
    Var(Ident),

    /// Access a field on the inner expression.
    Field { inner: Box<Expr>, field: Ident },

    /// Define a function.
    Lam(Vec<Ident>, Box<Expr>),

    /// Call a function.
    Call {
        function: Box<Expr>,
        args: Vec<Expr>,
    },

    /// Apply a unary operator.
    UnOp(UnOp, Box<Expr>),

    /// Apply a binary operator.
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

/// One or more elements of a sequence.
#[derive(Debug)]
pub enum Seq {
    /// A single element.
    Elem { span: Span, value: Box<Expr> },

    /// A `key: value` mapping.
    Assoc {
        /// The span of the `=` or `:`.
        op_span: Span,
        key: Box<Expr>,
        value: Box<Expr>,
    },

    /// Let in the middle of a sequence literal.
    ///
    /// This is syntactically different from a let before an expression, because
    /// associations are not first-class values.
    Let {
        ident: Ident,
        value: Box<Expr>,
        body: Box<Seq>,
    },

    /// Loop over the collection, binding the values to `idents`.
    For {
        idents: Vec<Ident>,
        collection: Box<Expr>,
        body: Box<Seq>,
    },

    /// Enter the loop only if the condition is true.
    If {
        condition: Box<Expr>,
        body: Box<Seq>,
    },
}
