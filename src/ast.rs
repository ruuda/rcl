// RCL -- A reasonable configuration language.
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
#[derive(Clone, Eq, Ord, PartialEq, PartialOrd)]
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

/// A part of a format string, either a hole or a string literal.
// TODO: Should not be cloneable, make it GC'able instead.
#[derive(Clone, Debug)]
pub struct FormatFragment {
    /// For a hole, the span of the expression that fills the hole.
    pub span: Span,
    pub body: Expr,
}

/// A statement-like expression.
///
/// RCL does not have statements that have side effects, but it does have
/// constructs that look like statements, which evaluate a left-hand side,
/// and then evaluate a body in a modified environment. For lack of a better
/// name, we do call those _statements_.
// TODO: Should not be cloneable, make it GC'able instead.
#[derive(Clone, Debug)]
pub enum Stmt {
    /// A let-binding.
    Let { ident: Ident, value: Box<Expr> },

    /// Evaluate to the body if true, fail with the message if false.
    Assert {
        /// The span of the condition. Here we report the error from.
        condition_span: Span,
        condition: Box<Expr>,
        message: Box<Expr>,
    },

    /// Print the message for debugging.
    Trace {
        message_span: Span,
        message: Box<Expr>,
    },
}

/// An expression.
// TODO: Should not be cloneable, make it GC'able instead.
#[derive(Clone, Debug)]
pub enum Expr {
    /// A statement-like expression.
    Stmt { stmt: Stmt, body: Box<Expr> },

    /// Import an expression from a given file path.
    Import {
        /// The span for the path expression.
        path_span: Span,

        /// An expression that evaluates to the path to import.
        path: Box<Expr>,
    },

    /// A dict or set literal (depending on the element types) enclosed in `{}`.
    BraceLit(Vec<Seq>),

    /// A list literal enclosed in `[]`.
    BracketLit(Vec<Seq>),

    /// A null literal.
    NullLit,

    /// A boolean literal.
    BoolLit(bool),

    /// A string literal.
    StringLit(Rc<str>),

    /// An integer literal.
    /// TODO: This should be a bigint.
    IntegerLit(i64),

    /// A format string, with string literals and hole contents interleaved.
    Format(Vec<FormatFragment>),

    /// An conditional choice (if, then, else).
    IfThenElse {
        condition_span: Span,
        condition: Box<Expr>,
        body_then: Box<Expr>,
        body_else: Box<Expr>,
    },

    /// Access a variable.
    Var { span: Span, ident: Ident },

    /// Access a field on the inner expression.
    Field {
        inner: Box<Expr>,
        inner_span: Span,
        field: Ident,
        field_span: Span,
    },

    /// Define a lambda function.
    Function {
        /// The span of the `=>`.
        span: Span,
        args: Vec<Ident>,
        body: Box<Expr>,
    },

    /// Call a function.
    Call {
        /// The opening parenthesis.
        open: Span,
        /// The closing parenthesis.
        close: Span,
        function_span: Span,
        function: Box<Expr>,
        args: Vec<(Span, Expr)>,
    },

    /// Index into a collection as `collection[index]`.
    Index {
        /// The opening bracket.
        open: Span,
        /// The closing bracket.
        close: Span,
        collection_span: Span,
        collection: Box<Expr>,
        index_span: Span,
        index: Box<Expr>,
    },

    /// Apply a unary operator.
    UnOp {
        op: UnOp,
        op_span: Span,
        body: Box<Expr>,
    },

    /// Apply a binary operator.
    BinOp {
        op: BinOp,
        op_span: Span,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

/// The innermost part of comprehension ([`Seq`]).
// TODO: Should not be cloneable, make it GC'able instead.
#[derive(Clone, Debug)]
pub enum Yield {
    /// A single element.
    Elem { span: Span, value: Box<Expr> },

    /// A `key: value` mapping.
    Assoc {
        /// The span of the `=` or `:`.
        op_span: Span,
        key: Box<Expr>,
        value: Box<Expr>,
    },
}

/// One or more elements of a sequence.
// TODO: Should not be cloneable, make it GC'able instead.
#[derive(Clone, Debug)]
pub enum Seq {
    /// Yield a value or key-value pair.
    Yield(Yield),

    /// A statement in the middle of a sequence literal.
    ///
    /// This is syntactically different from a statement before an expression,
    /// because associations are not first-class values.
    Stmt { stmt: Stmt, body: Box<Seq> },

    /// Loop over the collection, binding the values to `idents`.
    For {
        idents_span: Span,
        idents: Vec<Ident>,
        collection_span: Span,
        collection: Box<Expr>,
        body: Box<Seq>,
    },

    /// Enter the loop only if the condition is true.
    If {
        condition_span: Span,
        condition: Box<Expr>,
        body: Box<Seq>,
    },
}

impl Seq {
    /// Return the innermost seq, which is either an `Elem` or `Assoc`.
    pub fn innermost(&self) -> &Yield {
        match self {
            Seq::Yield(y) => y,
            Seq::Stmt { body, .. } => body.innermost(),
            Seq::For { body, .. } => body.innermost(),
            Seq::If { body, .. } => body.innermost(),
        }
    }
}
