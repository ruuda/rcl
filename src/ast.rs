// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

pub type Ident = &'static str;

/// A unary operator.
#[derive(Debug, Copy, Clone)]
pub enum UnOp {
    /// Negate a boolean.
    Neg,
}

/// A binary operator.
#[derive(Debug, Copy, Clone)]
pub enum BinOp {
    /// `|`: Union two collections
    Union,

    /// `+`: Add two numbers.
    Add,
}

#[derive(Debug)]
pub enum Expr {
    /// A map or set literal, depending on the element types.
    MapLit(Vec<Seq>),

    /// A list literal.
    ListLit(Vec<Seq>),

    /// A string literal.
    StringLit(String),

    // TODO: Having those would require an explicit type for them.
    // It may be nice for some function calls, but for now we can just require
    // the user to wrap them in [] or {}.
    // /// A for-comprehension.
    // Compr(Box<Compr>),
    /// An conditional choice (if, then, else).
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),

    /// Access a variable.
    Var(Ident),

    /// Access a field or key.
    Field(Ident, Box<Expr>),

    /// A let-binding. First is the bound value, then the result expression.
    Let(Ident, Box<Expr>, Box<Expr>),

    /// Call a function.
    Call(Box<Expr>, Vec<Expr>),

    /// Define a function.
    Lam(Vec<Ident>, Box<Expr>),

    /// Apply a unary operator.
    UnOp(UnOp, Box<Expr>),

    /// Apply a binary operator.
    BinOp(BinOp, Box<Expr>, Box<Expr>),
}

/// One or more elements of a sequence.
#[derive(Debug)]
pub enum Seq {
    /// A single element.
    Elem(Box<Expr>),

    /// A `key: value` mapping.
    Assoc(Box<Expr>, Box<Expr>),

    /// A comprehension that yields elements or mappings.
    Compr(Compr),
}

/// A for-comprehension.
#[derive(Debug)]
pub enum Compr {
    /// Loop over the collection.
    For {
        collection: Box<Expr>,
        elements: Vec<Ident>,
        body: Box<Seq>,
    },

    /// Enter the loop only if the condition is true.
    If {
        condition: Box<Expr>,
        body: Box<Seq>,
    },

    /// Let in the middle of a comprehension.
    ///
    /// This is syntactically different from a let before an expression, because
    /// the `Seq::Assoc` is not a first-class value. Not sure if duplicating the
    /// let or making `Assoc` a value is the best way to go about it, but let's
    /// try this way for now.
    Let {
        name: Ident,
        value: Box<Expr>,
        body: Box<Seq>,
    },
}
