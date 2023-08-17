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

/// A unary operator.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UnOp {
    /// Negate a boolean.
    Neg,
}

/// A binary operator.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinOp {
    /// `+`: Add two numbers.
    Add,

    /// `*` Multiply two numbers.
    Mul,

    /// `and`: Boolean AND.
    And,

    /// `or`: Boolean OR.
    Or,

    /// `|`: Union two collections
    Union,

    /// `<`: Less than.
    Lt,

    /// `>`: Greater than.
    Gt,

    /// `<=`: Less than or equal.
    LtEq,

    /// `>=`: Greater than or equal.
    GtEq,
}

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
        body: Box<Prefixed<Expr>>,
    },

    /// A boolean literal.
    BoolLit(Span, bool),

    /// A string literal quoted in double quotes (`"`).
    StringLit(Span),

    /// A string literal quoted in triple double quotes (`"""`).
    StringLitTriple(Span),

    /// An integer in hexadecimal notation.
    NumHexadecimal(Span),

    /// An integer in binary notation.
    NumBinary(Span),

    /// A number in decimal notation.
    NumDecimal(Span),

    /// A conditional expression.
    IfThenElse {
        condition: Box<Expr>,
        before_then: Box<[NonCode]>,
        body_then: Box<Prefixed<Expr>>,
        before_else: Box<[NonCode]>,
        body_else: Box<Prefixed<Expr>>,
    },

    /// Access a variable.
    Var(Span),

    /// Access a field on the inner expression.
    Field { inner: Box<Expr>, field: Span },

    /// Call a function.
    Call {
        open: Span,
        close: Span,
        function: Box<Expr>,
        args: Box<[Prefixed<Expr>]>,
    },

    /// A unary operator.
    UnOp {
        op: UnOp,
        op_span: Span,
        body: Box<Expr>,
    },

    /// A binary operator.
    BinOp {
        // TODO: How to handle noncode in binops? It is somewhat reasonable to
        // expect people to write
        //     let x = foo +
        //       // Add trailing newline.
        //       "\n";
        // But also to write
        //     let x = foo
        //       // Add trailing newline.
        //       + "\n";
        // Personally I prefer the second form, but maybe we should support the
        // first form and reformat it to the second. We could store one NonCode
        // with the operator, but then we need to concatenate the noncode from
        // before and after, strip duplicate blanks, etc ... it would be messy.
        op: BinOp,
        op_span: Span,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

/// An inner element of a collection literal.
#[derive(Debug)]
pub enum Seq {
    /// A single element.
    Elem {
        /// The span covering this value.
        span: Span,
        value: Box<Expr>
    },

    /// A `key: value` mapping, where the key is an expression.
    AssocExpr {
        /// The `:` span.
        op_span: Span,
        field: Box<Expr>,
        value: Box<Expr>,
    },

    /// A `key = value` mapping, where the key is an identifier.
    AssocIdent {
        /// The `=` span.
        op_span: Span,
        field: Span,
        value: Box<Expr>,
    },

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
