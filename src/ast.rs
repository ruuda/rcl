// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The Abstract Syntax Tree.

use std::fmt;
use std::rc::Rc;

pub use crate::cst::{BinOp, UnOp};

use crate::decimal::Decimal;
use crate::source::Span;
use crate::types::{self, SourcedType};

/// An identifier.
// TODO: Should we deduplicate idents, or even all strings, in a hash table?
// Should they be slices into the source document? For now the easy thing is to
// just make them strings, we can optimize later.
#[derive(Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Ident(pub Rc<str>);

// coverage:off -- Debug is needed for asserts but not covered when there are no errors.
impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
// coverage:on

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
    Let {
        ident_span: Span,
        ident: Ident,
        type_: Option<Box<Type>>,
        value_span: Span,
        value: Box<Expr>,
    },

    /// Evaluate to the body if true, fail with the message if false.
    Assert {
        /// The span of the condition. Here we report the error from.
        condition_span: Span,
        condition: Box<Expr>,
        message_span: Span,
        message: Box<Expr>,
    },

    /// Print the message for debugging.
    Trace {
        message_span: Span,
        message: Box<Expr>,
    },
}

/// An argument provided to a function call.
#[derive(Clone, Debug)]
pub struct CallArg<T> {
    pub span: Span,
    pub value: T,
}

/// An expression.
// TODO: Should not be cloneable, make it GC'able instead.
#[derive(Clone, Debug)]
pub enum Expr {
    /// A statement-like expression.
    Stmt {
        stmt: Stmt,
        body_span: Span,
        body: Box<Expr>,
    },

    /// Import an expression from a given file path.
    Import {
        /// The span for the path expression.
        path_span: Span,

        /// An expression that evaluates to the path to import.
        path: Box<Expr>,
    },

    /// A dict or set literal (depending on the element types) enclosed in `{}`.
    ///
    /// The typechecker replaces this by either [`Expr::SetLit`] or [`Expr::DictLit`].
    BraceLit { open: Span, elements: Vec<Seq> },

    /// A list literal enclosed in `[]`.
    BracketLit { open: Span, elements: Vec<Seq> },

    /// A set literal enclosed in `{}`.
    SetLit { open: Span, elements: Vec<Seq> },

    /// A dict literal enclosed in `{}`.
    DictLit { open: Span, elements: Vec<Seq> },

    /// A null literal.
    NullLit,

    /// A boolean literal.
    BoolLit(bool),

    /// A string literal.
    StringLit(Rc<str>),

    /// A number literal.
    NumberLit(Decimal),

    /// A format string, with string literals and hole contents interleaved.
    Format(Vec<FormatFragment>),

    /// A conditional choice (if, then, else).
    IfThenElse {
        condition_span: Span,
        condition: Box<Expr>,
        span_then: Span,
        span_else: Span,
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
    ///
    /// This node only exists before typechecking. The typechecker converts all
    /// [`Expr::Function`] nodes to [`Expr::TypedFunction`].
    Function {
        args: Vec<(Span, Ident)>,
        body_span: Span,
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
        args: Vec<CallArg<Expr>>,
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
        op_span: Span,
        op: UnOp,
        body_span: Span,
        body: Box<Expr>,
    },

    /// Apply a binary operator.
    BinOp {
        op_span: Span,
        op: BinOp,
        lhs_span: Span,
        lhs: Box<Expr>,
        rhs_span: Span,
        rhs: Box<Expr>,
    },

    /// Apply a dynamic type check.
    ///
    /// This node is not representable by the concrete syntax tree. After
    /// parsing and abstracting, this node is not part of the AST. Only in the
    /// typecheck phase, the typechecker can decide to insert these nodes.
    CheckType {
        /// The span of the expression that we are checking.
        span: Span,
        /// The type requirement that the value has to satisfy.
        type_: SourcedType,
        body: Box<Expr>,
    },

    /// Define a lambda function.
    ///
    /// This node only exists after typechecking. The typechecker converts all
    /// [`Expr::Function`] nodes to [`Expr::TypedFunction`]. The arguments and
    /// their names are stored in the type. The argument names are always
    /// present.
    TypedFunction {
        /// Source location of the function, including args, `=>`, and body.
        span: Span,
        body_span: Span,
        body: Box<Expr>,
        type_: Rc<types::Function>,
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
        key_span: Span,
        value_span: Span,
        key: Box<Expr>,
        value: Box<Expr>,
    },

    /// Yield all scalar elements from a collection.
    UnpackElems {
        unpack_span: Span,
        collection_span: Span,
        collection: Box<Expr>,

        /// If set, we need to perform a runtime type check on every element.
        ///
        /// `UnpackAssocs` does not have the corresponding field, because only
        /// dicts can be unpacked, so that one we can handle by wrapping the
        /// collection expression in a `CheckType` node. While we could do that
        /// here too and use a union type, we can get clearer errors with a
        /// first-class check.
        check_elem_type: Option<SourcedType>,
    },

    /// Yield all key-value pairs from a dict.
    UnpackAssocs {
        unpack_span: Span,
        collection_span: Span,
        collection: Box<Expr>,
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

#[derive(Clone, Debug)]
pub enum Type {
    /// A term is a named type, not necessarily primitive.
    ///
    /// For example, `Bool` (primitive), or `Widget` (user-defined).
    Term { span: Span, name: Ident },

    /// Instantiate a generic type; apply a type constructor.
    ///
    /// For example, `Dict[k, v]`.
    Apply {
        /// The span of the name.
        span: Span,
        /// The name, e.g. `Dict`.
        name: Ident,
        /// All the type arguments between `[]`.
        args: Box<[Type]>,
    },

    /// A function type with zero or more arguments, and one result type.
    Function {
        span: Span,
        args: Box<[Type]>,
        result: Box<Type>,
    },
}
