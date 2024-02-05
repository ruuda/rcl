// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Representations of types.

use std::rc::Rc;

use crate::error::{Error, IntoError, Result};
use crate::fmt_type::format_type;
use crate::markup::Markup;
use crate::pprint::{concat, indent, Doc};
use crate::source::Span;

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

    /// Check that `self` is a subtype of `expected`.
    ///
    /// When a type is known for a particular variable, but we then try to use
    /// that variable in a context where a particular type is expected, we have
    /// to verify that the known type fits the expected type. For example, a
    /// record that only has `Int` fields would fit the type `Dict[String, Int]`,
    /// but not the other way around.
    ///
    /// Type errors will be attributed to the span `at`.
    pub fn check_subtype_of(&self, at: Span, expected: &Type) -> Result<()> {
        match (expected, self) {
            // If we expect void -- there exist no values of type void, it's
            // dead code. Even `Dynamic` is no good, because expressions of type
            // `Dynamic` at least have *some* value at runtime.
            (Type::Void, Type::Void) => Ok(()),
            (Type::Void, _) => at
                .error(concat! {
                    "Expected a value of type "
                    format_type(&Type::Void).into_owned()
                    ", but such values do not exist."
                })
                .err(),

            // If we defer the typecheck to runtime, anything is allowed.
            (Type::Dynamic, _) => Ok(()),

            (Type::Function(expected_fn), Type::Function(actual_fn)) => {
                if expected_fn.args.len() != actual_fn.args.len() {
                    let mut msg: Vec<Doc> = vec!["Expected a function that takes ".into()];
                    match expected_fn.args.len() {
                        0 => msg.push("no arguments:".into()),
                        1 => msg.push("1 argument:".into()),
                        n => {
                            msg.push(n.to_string().into());
                            msg.push(" arguments:".into());
                        }
                    }
                    msg.push(concat! {
                        Doc::HardBreak Doc::HardBreak
                        indent! { format_type(expected).into_owned() }
                        Doc::HardBreak Doc::HardBreak
                        "But got a function that takes "
                    });
                    msg.push(actual_fn.args.len().to_string().into());
                    msg.push(concat! {
                        ":"
                        Doc::HardBreak Doc::HardBreak
                        indent! { format_type(self).into_owned() }
                    });
                    return at
                        .error("Arity mismatch.")
                        .with_body(Doc::Concat(msg))
                        .err();
                }

                actual_fn
                    .result
                    .check_subtype_of(at, &expected_fn.result)
                    .map_err(|err|
                        // TODO: Include full function type in message.
                        err.with_note(at, "While checking that function types match."))?;
                for (arg_expected, arg_actual) in expected_fn.args.iter().zip(actual_fn.args.iter())
                {
                    // Note, the roles of expected and actual are reversed for
                    // function arguments.
                    // TODO: Report error at the argument span?
                    arg_expected
                        .check_subtype_of(at, arg_actual)
                        .map_err(|err| {
                            err.with_note(at, "While checking that function types match.")
                        })?;
                }

                Ok(())
            }

            // Every type is a subtype of itself.
            _ if expected == self => Ok(()),

            // TODO: Check inside collections.
            _ => type_error(at, expected, self).err(),
        }
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

/// Report a static type error.
///
/// A static type error can be reported at typecheck time based on the AST, so
/// the culprit is a syntactic construct, not a runtime value.
///
/// The `actual` message should be in the form of “Found «actual» instead”.
pub fn type_error<T1: AsTypeName, T2: AsTypeName>(at: Span, expected: &T1, actual: &T2) -> Error {
    // If types are atoms, they are short to format, so we can put the message
    // on one line. If they are composite, we put them in an indented block.
    match (expected.is_atom(), actual.is_atom()) {
        (true, true) => at.error("Type mismatch.").with_body(concat! {
            "Expected " expected.format_type()
            " but found " actual.format_type() "."
        }),
        (true, false) => at.error("Type mismatch.").with_body(concat! {
            "Expected " expected.format_type() " but found this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { actual.format_type() }
        }),
        (false, true) => at.error("Type mismatch.").with_body(concat! {
            "Expected this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { expected.format_type() }
            Doc::HardBreak Doc::HardBreak
            "But found " actual.format_type() "."
        }),
        (false, false) => at.error("Type mismatch.").with_body(concat! {
            "Expected this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { expected.format_type() }
            Doc::HardBreak Doc::HardBreak
            "But found this type: "
            indent! { expected.format_type() }
        }),
    }
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
