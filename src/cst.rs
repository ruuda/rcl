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

use crate::lexer::QuoteStyle;
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

/// A hole in a format string.
#[derive(Debug)]
pub struct FormatHole {
    /// The span of the expression that fills the hole, excluding `}` and `{`.
    pub span: Span,

    /// The expression that fills the hole.
    pub body: Expr,

    /// The string literal following the hole, including `}`.
    pub suffix: Span,
}

#[derive(Debug)]
pub enum Expr {
    /// A let-binding that binds `value` to the name `ident` in `body`.
    Let {
        ident: Span,
        value_span: Span,
        value: Box<Expr>,
        body_span: Span,
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
        body_span: Span,
        body: Box<Prefixed<Expr>>,
    },

    /// A null literal.
    NullLit(Span),

    /// A boolean literal.
    BoolLit(Span, bool),

    /// A string literal quoted in double or triple quotes (`"`).
    StringLit(QuoteStyle, Span),

    /// A format string, also called f-string.
    FormatString {
        /// Whether the string is double (`f"`) or triple (`f"""`) quoted.
        style: QuoteStyle,
        /// The string literal up to and including the `{` of the first hole.
        begin: Span,
        /// Contents of a hole followed by the string literal after it.
        holes: Vec<FormatHole>,
    },

    /// An integer in hexadecimal notation.
    NumHexadecimal(Span),

    /// An integer in binary notation.
    NumBinary(Span),

    /// A number in decimal notation.
    NumDecimal(Span),

    /// A conditional expression.
    IfThenElse {
        condition_span: Span,
        condition: Box<Expr>,
        then_before: Box<[NonCode]>,
        then_span: Span,
        then_body: Box<Prefixed<Expr>>,
        else_before: Box<[NonCode]>,
        else_span: Span,
        else_body: Box<Prefixed<Expr>>,
    },

    /// Access a variable.
    Var(Span),

    /// Access a field on the inner expression.
    Field { inner: Box<Expr>, field: Span },

    /// Call a function.
    Call {
        /// The opening parenthesis.
        open: Span,
        /// The closing parenthesis.
        close: Span,
        function_span: Span,
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
        value: Box<Expr>,
    },

    /// A `key: value` mapping, where the key is an expression.
    AssocExpr {
        /// The `:` span.
        op_span: Span,
        field: Box<Expr>,
        value_span: Span,
        value: Box<Expr>,
    },

    /// A `key = value` mapping, where the key is an identifier.
    AssocIdent {
        /// The `=` span.
        op_span: Span,
        field: Span,
        value_span: Span,
        value: Box<Expr>,
    },

    /// Let in the middle of a sequence literal.
    ///
    /// This is syntactically different from a let before an expression, because
    /// associations are not first-class values.
    Let {
        ident: Span,
        value_span: Span,
        value: Box<Expr>,
        body: Box<Prefixed<Seq>>,
    },

    /// Loop over the collection, binding the values to `idents`.
    For {
        idents: Box<[Span]>,
        collection_span: Span,
        collection: Box<Expr>,
        body: Box<Prefixed<Seq>>,
    },

    /// Enter the loop only if the condition is true.
    If {
        condition_span: Span,
        condition: Box<Expr>,
        body: Box<Prefixed<Seq>>,
    },
}
