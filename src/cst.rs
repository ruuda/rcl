// RCL -- A reasonable configuration language.
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

use crate::lexer::{Escape, QuoteStyle, StringPrefix};
use crate::source::Span;

/// A unary operator.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UnOp {
    /// Negate a boolean.
    Not,

    /// Invert a number (additive inverse, i.e. negation).
    Neg,
}

/// A binary operator.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinOp {
    /// `+`: Add two numbers.
    Add,

    /// `*` Multiply two numbers.
    Mul,

    /// `/` Divide two numbers.
    Div,

    /// `-` Subtract two numbers.
    Sub,

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

    /// `==`: Equals.
    Eq,

    /// `!=`: Does not equal.
    Neq,
}

/// Not code, but a piece of the document relevant to preserve for formatting.
#[derive(Debug)]
pub enum NonCode {
    /// One or more blank lines.
    Blank(Span),

    /// A comment that runs until the end of the line (excluding the newline).
    LineComment(Span),

    /// A line that starts with `#!`, excluding the newline itself.
    Shebang(Span),
}

/// An inner node that might be preceded by non-code.
#[derive(Debug)]
pub struct Prefixed<T> {
    /// Non-code that precedes the tree node.
    pub prefix: Box<[NonCode]>,

    /// The tree node itself.
    pub inner: T,
}

/// A prefixed expression, and the span of the inner expression.
pub type SpanPrefixedExpr = (Span, Prefixed<Expr>);

/// A part of a string literal or format string.
#[derive(Debug)]
pub enum StringPart {
    /// A fragment of a line. The `\n` is leading, not trailing.
    String(Span),
    /// An escape sequence.
    Escape(Span, Escape),
    /// An interpolation hole (only inside format strings).
    Hole(Span, Expr),
}

/// A `let`-binding, `assert`, or `trace`.
///
/// RCL does not have statements that have side effects, but it does have
/// constructs that look like statements, which evaluate a left-hand side,
/// and then evaluate a body in a modified environment. For lack of a better
/// name, we call those _statements_.
#[derive(Debug)]
pub enum Stmt {
    /// A let-binding that binds `value` to the name `ident` in `body`.
    Let {
        ident: Span,
        type_: Option<Box<Type>>,
        value_span: Span,
        value: Box<Expr>,
    },

    /// An assertion with a failure message.
    Assert {
        condition_span: Span,
        condition: Box<Expr>,
        message_span: Span,
        message: Box<Expr>,
    },

    /// Trace prints the message (for debugging) and then evaluates to the body.
    Trace {
        message_span: Span,
        message: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum Expr {
    /// A statement-like expression.
    Stmt {
        stmt: Stmt,
        body_span: Span,
        body: Box<Prefixed<Expr>>,
    },

    /// Import an expression from a given file path.
    Import {
        /// The span for the path expression.
        path_span: Span,

        /// An expression that evaluates to the path to import.
        path: Box<Prefixed<Expr>>,
    },

    /// A `{}`-enclosed collection literal.
    BraceLit {
        open: Span,
        close: Span,
        elements: Box<[Prefixed<Seq>]>,
        /// Any content before the closing brace.
        suffix: Box<[NonCode]>,
    },

    /// A `[]`-enclosed collection literal.
    BracketLit {
        open: Span,
        close: Span,
        elements: Box<[Prefixed<Seq>]>,
        /// Any content before the closing bracket.
        suffix: Box<[NonCode]>,
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

    /// A string literal (both regular and format string).
    StringLit {
        /// Whether the string is a format string or not.
        prefix: StringPrefix,
        /// Whether the string is double (`"`) or triple (`"""`) quoted.
        style: QuoteStyle,
        /// The opening quote.
        open: Span,
        /// The closing quote.
        close: Span,
        /// Inner parts of the string literal, split by line, and holes.
        parts: Vec<StringPart>,
    },

    /// An integer in hexadecimal notation.
    NumHexadecimal(Span),

    /// An integer in binary notation.
    NumBinary(Span),

    /// A number in decimal notation.
    NumDecimal(Span),

    /// Access a variable.
    Var(Span),

    /// Access a field on the inner expression.
    Field {
        inner_span: Span,
        inner: Box<Expr>,
        field: Span,
    },

    /// A conditional expression.
    IfThenElse {
        condition_span: Span,
        condition: Box<Expr>,
        then_span: Span,
        then_body: Box<Prefixed<Expr>>,
        else_span: Span,
        else_body: Box<Prefixed<Expr>>,
    },

    /// Define a lambda function.
    Function {
        /// The source location of the `=>`.
        span: Span,
        args: Box<[Prefixed<Span>]>,
        /// Any non-code between the final arg and the closing paren.
        suffix: Box<[NonCode]>,
        body: Box<Expr>,
    },

    /// Call a function.
    Call {
        /// The opening parenthesis.
        open: Span,
        /// The closing parenthesis.
        close: Span,
        /// The span of the callee.
        function_span: Span,
        /// The callee expression.
        function: Box<Expr>,
        args: Box<[SpanPrefixedExpr]>,
        /// Any non-code between the final argument and the closing paren.
        suffix: Box<[NonCode]>,
    },

    /// Index into a collection with `[]`.
    Index {
        /// The opening bracket.
        open: Span,
        /// The closing bracket.
        close: Span,
        collection_span: Span,
        collection: Box<Expr>,
        index_span: Span,
        index: Box<Prefixed<Expr>>,
    },

    /// A unary operator.
    UnOp {
        op_span: Span,
        op: UnOp,
        body_span: Span,
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

    /// A "statement" in the middle of a sequence literal.
    ///
    /// This is syntactically different from a let, assert, and trace before an
    /// expression, because associations are not first-class values.
    Stmt {
        stmt: Stmt,
        body_span: Span,
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

#[derive(Debug)]
pub enum Type {
    /// A term is a named type, not necessarily primitive.
    ///
    /// For example, `Int` (primitive), or `Widget` (user-defined).
    Term(Span),

    /// Instantiate a generic type; apply a type constructor.
    ///
    /// For example, `Dict[k, v]`.
    Apply {
        name: Span,
        args: Box<[Prefixed<Type>]>,
    },

    /// A function type with zero or more arguments, and one result type.
    Function {
        args: Box<[Prefixed<Type>]>,
        result: Box<Type>,
    },
}
