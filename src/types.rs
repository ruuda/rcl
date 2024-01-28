// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Representations of types.

use std::rc::Rc;

use crate::error::{IntoError, Result};
use crate::fmt_type::format_type;
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
            _ => {
                // TODO: Generate a briefer error when the expected type is a primitive type,
                // dedup between the `typecheck::type_error` function.
                at.error("Type mismatch.")
                    .with_body(concat! {
                        "Expected this type:"
                        Doc::HardBreak Doc::HardBreak
                        indent! { format_type(expected).into_owned() }
                        Doc::HardBreak Doc::HardBreak
                        "But got this type:"
                        Doc::HardBreak Doc::HardBreak
                        indent! { format_type(self).into_owned() }
                    })
                    .err()
            }
        }
    }
}
