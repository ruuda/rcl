// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Representations of types.

use std::rc::Rc;

use crate::fmt_type::format_type;
use crate::markup::Markup;
use crate::pprint::{concat, indent, Doc};

/// A type.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Type {
    /// Any value, the type is not statically known.
    ///
    /// This is the top of the type lattice, it is a supertype of all types.
    Dynamic,

    /// The type of unreachable code.
    ///
    /// This is the bottom of the type lattice, it is a subtype of any type.
    Void,

    /// The primitive type `Bool`.
    Bool,

    /// The primitive type `Int`.
    Int,

    /// The primitive type `Null`.
    Null,

    /// The primitive type `String`.
    String,

    /// A dict with the given key and value types.
    Dict(Rc<Dict>),

    /// A list with the given element type.
    List(Rc<Type>),

    /// A set with the given element type.
    Set(Rc<Type>),

    /// A function.
    Function(Rc<Function>),
}

/// The type parameters for the `Dict` type.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Dict {
    pub key: Type,
    pub value: Type,
}

/// A function type.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Function {
    pub args: Vec<Type>,
    pub result: Type,
}

impl Type {
    /// Return an upper bound of the two types.
    ///
    /// An upper bound is a type `T` such that `self` and `other` are both
    /// subtypes of `T`. Note, we don't promise to return the _least_ upper bound,
    /// just _an_ upper bound.
    pub fn meet(&self, other: &Type) -> Type {
        match (self, other) {
            // Anything involving dynamic becomes dynamic, anything involving
            // void becomes the other thing, these are the top and bottom of
            // the type lattice.
            (Type::Dynamic, _) => Type::Dynamic,
            (_, Type::Dynamic) => Type::Dynamic,
            (Type::Void, that) => that.clone(),
            (this, Type::Void) => this.clone(),

            // If we have matching primitive types, they are preserved.
            (Type::Bool, Type::Bool) => Type::Bool,
            (Type::Int, Type::Int) => Type::Int,
            (Type::Null, Type::Null) => Type::Null,
            (Type::String, Type::String) => Type::String,

            // For composite types, we meet on their elements.
            (Type::Dict(d1), Type::Dict(d2)) => {
                // TODO: Could reuse existing Rc instead of building a new one
                // if the types are equal. Also below.
                let dm = Dict {
                    key: d1.key.meet(&d2.key),
                    value: d1.value.meet(&d2.value),
                };
                Type::Dict(Rc::new(dm))
            }
            (Type::List(l1), Type::List(l2)) => Type::List(Rc::new(l1.meet(l2))),
            (Type::Set(l1), Type::Set(l2)) => Type::Set(Rc::new(l1.meet(l2))),

            // TODO: Support meeting functions.
            (Type::Function(_), Type::Function(_)) => Type::Dynamic,

            // Any two values that mismatch, we can't describe with a single
            // static type, but that doesn't mean it's a type error, the program
            // may still be valid at runtime. E.g, I have a list with a function
            // and an int. If the program only ever calls `list[0]` and performs
            // integer addition on `list[1]`, that is fine. We can type the list
            // as `List[Dynamic]`.
            _ => Type::Dynamic,
        }
    }

    /// Return whether the type is not composite, i.e. is not composed of other types.
    pub fn is_atom(&self) -> bool {
        matches!(
            self,
            Type::Bool | Type::Int | Type::Null | Type::String | Type::Void | Type::Dynamic,
        )
    }
}

/// Helper to enable using short names in type errors.
pub trait AsTypeName {
    fn format_type(&self) -> Doc<'static>;
    fn is_atom(&self) -> bool;
}

impl AsTypeName for &'static str {
    fn format_type(&self) -> Doc<'static> {
        Doc::from(*self).with_markup(Markup::Type)
    }
    fn is_atom(&self) -> bool {
        true
    }
}

impl AsTypeName for Type {
    fn format_type(&self) -> Doc<'static> {
        // If we are generating a type error, and it has Dynamic in there as
        // the top-level type that we format, then that's a bug in the
        // typechecker, because Dynamic is just a way to say "I don't know what
        // the type is yet", you can't violate a type expectation this way.
        debug_assert_ne!(
            self,
            &Type::Dynamic,
            "The Dynamic type should never be a direct cause of a type error.",
        );
        format_type(self).into_owned()
    }
    fn is_atom(&self) -> bool {
        self.is_atom()
    }
}

/// Format a static type error body.
///
/// This does not include the "Type mismatch." message, so that the body can be
/// used in various places.
pub fn report_type_mismatch<T1: AsTypeName, T2: AsTypeName>(
    expected: &T1,
    actual: &T2,
) -> Doc<'static> {
    // If types are atoms, they are short to format, so we can put the message
    // on one line. If they are composite, we put them in an indented block.
    match (expected.is_atom(), actual.is_atom()) {
        (true, true) => concat! {
            "Expected " expected.format_type()
            " but found " actual.format_type() "."
        },
        (true, false) => concat! {
            "Expected " expected.format_type() " but found this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { actual.format_type() }
        },
        (false, true) => concat! {
            "Expected this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { expected.format_type() }
            Doc::HardBreak Doc::HardBreak
            "But found " actual.format_type() "."
        },
        (false, false) => concat! {
            "Expected this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { expected.format_type() }
            Doc::HardBreak Doc::HardBreak
            "But found this type: "
            Doc::HardBreak Doc::HardBreak
            indent! { actual.format_type() }
        },
    }
}

/// Format the body of an arity mismatch error.
///
/// This prints the two function signatures and their argument count.
/// TODO: This is unused. But I've written something like this 5 times now.
/// So let's keep it around for a bit.
fn _report_arity_mismatch(expected: Rc<Function>, actual: Rc<Function>) -> Doc<'static> {
    let mut parts: Vec<Doc<'static>> = vec!["Expected a function that takes ".into()];
    match expected.args.len() {
        0 => parts.push("no arguments:".into()),
        1 => parts.push("one argument:".into()),
        n => {
            parts.push(n.to_string().into());
            parts.push(" arguments:".into());
        }
    }
    parts.push(Doc::HardBreak);
    parts.push(Doc::HardBreak);
    parts.push(indent! { format_type(&Type::Function(expected)).into_owned() });
    parts.push(Doc::HardBreak);
    parts.push(Doc::HardBreak);
    parts.push("But got a function that takes ".into());
    parts.push(actual.args.len().to_string().into());
    parts.push(":".into());
    parts.push(Doc::HardBreak);
    parts.push(Doc::HardBreak);
    parts.push(indent! { format_type(&Type::Function(actual)).into_owned() });

    Doc::Concat(parts)
}

/// Rust eDSL for writing RCL types.
///
/// The syntax is similar to RCL, except for the generic and function types, to
/// make them fit Rust grammar.
///
/// * `List[T]` is written `[T]`.
/// * `Set[T]` is written `{T}`.
/// * `Dict[K, V]` is written `{K: V}`.
/// * `(P, Q) -> R` is written `(fn (P, Q) -> R)`
macro_rules! make_type {
    (Int) => { Type::Int };
    (Bool) => { Type::Bool };
    (Dynamic) => { Type::Dynamic };
    (String) => { Type::String };
    ([$elem:tt]) => { Type::List(Rc::new(crate::types::make_type!($elem))) };
    ({$elem:tt}) => { Type::Set(Rc::new(crate::types::make_type!($elem))) };
    ({$k:tt: $v:tt}) => {
        Type::Dict(Rc::new(crate::types::Dict {
            key: crate::types::make_type!($k),
            value: crate::types::make_type!($v),
        }))
    };
    ((fn ($( $arg_name:ident: $arg_type:tt ),*) -> $result:tt)) => {
        Type::Function(Rc::new(
            crate::types::make_function!(($( $arg_name:$arg_type ),*) -> $result)
        ))
    };
}
pub(crate) use make_type;

/// Rust eDSL for writing RCL function types.
///
/// See also [`make_type!`] for the syntax. This does not include the enclosing
/// `(fn ...)`, parens and `fn`, only the `...` is input to this macro.
macro_rules! make_function {
    (($( $arg_name:ident: $arg_type:tt ),*) -> $result:tt) => {
        crate::types::Function {
            // TODO: Include the argument names in types? Or at least elsewhere?
            args: vec![ $( crate::types::make_type!($arg_type) ),* ],
            result: crate::types::make_type!($result),
        }
    };
}
pub(crate) use make_function;
