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

/// A prefixed statement, and the span of the inner statement.
pub type SpanPrefixedStmt = (Span, Prefixed<Stmt>);

/// A collection of `T`s separated by commas, with an optional trailing comma and non-code suffix.
///
/// This is a list in the sense of a sequence of elements, it is not a list
/// literal in the source code. `List<T>` only describes elements, not the
/// surrounding delimiters like `[]`, `()`, or `{}`.
#[derive(Debug)]
pub struct List<T> {
    /// The collection elements.
    pub elements: Box<[T]>,

    /// Any non-code before the closing delimiter.
    pub suffix: Box<[NonCode]>,

    /// Whether a trailing comma is present.
    pub trailing_comma: bool,
}

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
    /// An expression preceded by a prefix and/or one or more statements.
    Statements {
        stmts: Vec<SpanPrefixedStmt>,
        body_span: Span,
        body: Box<Prefixed<Expr>>,
    },

    /// Import an expression from a given file path.
    Import {
        /// The span for the path expression.
        path_span: Span,

        /// An expression that evaluates to the path to import.
        path: Box<Expr>,
    },

    /// A `{}`-enclosed collection literal.
    BraceLit {
        open: Span,
        close: Span,
        elements: List<Seq>,
    },

    /// A `[]`-enclosed collection literal.
    BracketLit {
        open: Span,
        close: Span,
        elements: List<Seq>,
    },

    /// An expression enclosed in `()`.
    Parens {
        open: Span,
        close: Span,
        body_span: Span,
        body: Box<Expr>,
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

    /// A conditional expression.
    IfThenElse {
        condition_span: Span,
        condition: Box<Expr>,
        then_span: Span,
        then_body: Box<Expr>,
        else_span: Span,
        else_body: Box<Expr>,
    },

    /// Define a lambda function.
    Function {
        args: List<Prefixed<Span>>,
        body_span: Span,
        body: Box<Expr>,
    },

    /// A unary operator.
    UnOp {
        op_span: Span,
        op: UnOp,
        body_span: Span,
        body: Box<Expr>,
    },

    /// A binary operator.
    // TODO: We might also break up the binop into a true binary operator with
    // two sides, e.g. `<=` and `==`, and into n-ary operators that can be
    // repeated such as `+` and `*`. The latter would have a vec of args while
    // the former would have just the two sides.
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
        op_span: Span,
        op: BinOp,
        lhs_span: Span,
        lhs: Box<Expr>,
        rhs_span: Span,
        rhs: Box<Expr>,
    },

    /// A chained expression (field lookup, calls, indexes).
    ///
    /// We represent every chain as a node with a list of chained expressions
    /// rather than a degenerate CST tree that is a singly linked list, to make
    /// it easier to format such chains. It also matches the structure of the
    /// parser quite well.
    Chain {
        /// The very first expression in the chained expression.
        base_expr: Box<Expr>,

        /// Any chained operations to apply on top of the body (call, field lookup, etc.).
        ///
        /// The spans contain the span of the inner expression to which the
        /// chained expression is applied, e.g. if the base is `a` and the chain
        /// has `.b` and `.c`, then `chain[1]` has span `a.b`.
        chain: Vec<(Span, Chain)>,
    },
}

/// The innermost part of a comprehension, or just an element in a collection.
#[derive(Debug)]
pub enum Yield {
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
        field_span: Span,
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

    /// `..xs` is a shorthand for `for x in xs: x`.
    UnpackElems {
        unpack_span: Span,
        collection_span: Span,
        collection: Box<Expr>,
    },

    /// `...xs` is a shorthand for `for k, v in xs: k: v`.
    UnpackAssocs {
        unpack_span: Span,
        collection_span: Span,
        collection: Box<Expr>,
    },
}

/// Control flow or a statement inside a collection literal.
#[derive(Debug)]
pub enum SeqControl {
    /// A statement in the middle of a sequence literal.
    ///
    /// This is syntactically different from a let, assert, and trace before an
    /// expression, because associations are not first-class values.
    Stmt { stmt: Stmt },

    /// Loop over the collection, binding the values to `idents`.
    For {
        idents: Box<[Span]>,
        collection_span: Span,
        collection: Box<Expr>,
    },

    /// Enter the loop only if the condition is true.
    If {
        condition_span: Span,
        condition: Box<Expr>,
    },
}

/// An element of a collection literal.
///
/// An element always ends in a yield, and it can optionally be preceded by
/// "control items": statements, or loop control flow.
#[derive(Debug)]
pub struct Seq {
    pub control: Box<[Prefixed<SeqControl>]>,
    pub body: Prefixed<Yield>,
}

impl Seq {
    /// Whether the inner yield is for single elements, as opposed to assocs.
    pub fn is_inner_elem(&self) -> bool {
        matches!(
            self.body.inner,
            Yield::Elem { .. } | Yield::UnpackElems { .. }
        )
    }

    /// True when there are any control items, false if there is only the yield.
    pub fn has_control(&self) -> bool {
        !self.control.is_empty()
    }
}

/// A case in a chained non-operator expression (field lookup, call, index).
#[derive(Debug)]
pub enum Chain {
    /// Access a field on the preceding expression.
    Field { field: Span },

    /// Call a function.
    Call {
        /// The opening parenthesis.
        open: Span,
        /// The closing parenthesis.
        close: Span,
        /// The arguments passed to the call.
        args: List<(Span, Expr)>,
    },

    /// Index into a collection with `[]`.
    Index {
        /// The opening bracket.
        open: Span,
        /// The closing bracket.
        close: Span,
        /// The span of the index expression between the `[]`.
        index_span: Span,
        /// The index expression.
        index: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum Type {
    /// A term is a named type, not necessarily primitive.
    ///
    /// For example, `Bool` (primitive), or `Widget` (user-defined).
    Term(Span),

    /// Instantiate a generic type; apply a type constructor.
    ///
    /// For example, `Dict[k, v]`.
    Apply {
        span: Span,
        name: Span,
        args: List<Prefixed<Type>>,
    },

    /// A function type with zero or more arguments, and one result type.
    Function {
        span: Span,
        args: List<Prefixed<Type>>,
        result: Box<Type>,
    },
}
