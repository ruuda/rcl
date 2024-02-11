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
    // TODO: Rename to `Any`? It is now very clearly the "any" value type in the
    // lattice and it's no longer an exception to mean "runtime-checked".
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
    List(Rc<SourcedType>),

    /// A set with the given element type.
    Set(Rc<SourcedType>),

    /// A function.
    Function(Rc<Function>),
}

/// The type parameters for the `Dict` type.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Dict {
    pub key: SourcedType,
    pub value: SourcedType,
}

/// A function type.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Function {
    pub args: Vec<SourcedType>,
    pub result: SourcedType,
}

/// The place where a type was constructed.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Source {
    /// There is no source for this type.
    ///
    /// This is the case for `Type::Dynamic` when it is introduced by the
    /// typechecker for expressions that are not otherwise constrained.
    None,

    /// There were multiple sources, we had to merge them and lost the details.
    Many,

    /// The type comes from a built-in function.
    /// TODO: Add more details.
    Builtin,

    /// The type was inferred from a literal.
    ///
    /// I have a suspicion that this case is not going to show up in type errors,
    /// so I didn't add more details -- let's test first, maybe in the fuzzer,
    /// if it is even possible to cause an error that can be traced back here.
    Literal,

    /// It was a type annotation in the source code.
    Annotation(Span),

    /// A boolean is required, because it's used as a condition (`if` or `assert`).
    Condition,

    /// The type is the required type of the operator at the given span.
    Operator(Span),

    /// An integer is required due to indexing into a list.
    IndexList,
}

/// A type and its source.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct SourcedType {
    pub type_: Type,
    pub source: Source,
}

/// The result of a subtype check `T ≤ U`.
///
/// The result is a tree, to be able to pinpoint where the check fails, enabling
/// more helpful errors.
#[derive(Debug)]
pub enum TypeDiff {
    /// Yes, `T ≤ U`, and here is `T`.
    Ok(SourcedType),

    /// No, `not (T ≤ U)`, and here are `T` and `U`.
    Error(SourcedType, SourcedType),

    /// The types `T` and `U` are not orderable, and here is `U`.
    Defer(SourcedType),

    /// Both sides are a list, but the element type has an issue.
    List(Box<TypeDiff>),

    /// Both sides are a set, but the element type has an issue.
    Set(Box<TypeDiff>),

    /// Both sides are a dict, but the key or value (or both) have issues.
    Dict(Box<TypeDiff>, Box<TypeDiff>),

    /// Both sides are functions of the same arity, but args or result have issues.
    Function(Vec<TypeDiff>, Box<TypeDiff>),
}

impl Type {
    /// Return the least possible supertype of the two types.
    ///
    /// The meet is a type `T` such that `self` and `other` are both subtypes
    /// of `T`.
    /// TODO: This should take self and other by value.
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
                // Premature optimization: If the two types are equal, we can
                // skip the meet. I should measure how often we hit this case.
                let dm = if Rc::ptr_eq(d1, d2) {
                    d1.clone()
                } else {
                    Rc::new(Dict {
                        key: d1.key.meet(&d2.key),
                        value: d1.value.meet(&d2.value),
                    })
                };
                Type::Dict(dm)
            }
            (Type::List(l1), Type::List(l2)) => Type::List({
                if Rc::ptr_eq(l1, l2) {
                    l1.clone()
                } else {
                    Rc::new(l1.meet(l2))
                }
            }),
            (Type::Set(s1), Type::Set(s2)) => Type::Set({
                if Rc::ptr_eq(s1, s2) {
                    s1.clone()
                } else {
                    Rc::new(s1.meet(s2))
                }
            }),

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

impl Source {
    /// Combine two sources. Sources only combine when they are equal.
    pub fn meet(&self, other: &Source) -> Source {
        match (self, other) {
            (Source::None, _) => *other,
            (_, Source::None) => *other,
            (_, _) if self == other => *self,
            _ => Source::Many,
        }
    }

    /// Add context to a type error about why a particular type was expected.
    pub fn clarify_error<T: AsTypeName>(&self, expected_type: &T, error: Error) -> Error {
        let expected_name = if expected_type.is_atom() {
            expected_type.format_type()
        } else {
            "type".into()
        };
        match self {
            Source::None => error,
            Source::Many => error,

            // TODO: Add information about the builtin (function and arg name).
            Source::Builtin => error,

            Source::Literal => panic!("Found a case where Literal occurs in a type error!"),

            Source::Annotation(at) => error.with_note(
                *at,
                concat! {
                    "Expected " expected_name " because of this annotation."
                },
            ),

            Source::Operator(at) => error.with_note(
                *at,
                concat! {
                    "Expected " expected_name " because of this operator."
                },
            ),

            Source::Condition => {
                error.with_help("There is no implicit conversion, conditions must be boolean.")
            }

            Source::IndexList => error.with_help("List indices must be integers."),
        }
    }
}

impl SourcedType {
    /// Construct [`Type::Void`] without source.
    pub fn void() -> SourcedType {
        SourcedType {
            type_: Type::Void,
            source: Source::None,
        }
    }

    /// Construct [`Type::Dynamic`] without source.
    pub const fn any() -> SourcedType {
        SourcedType {
            type_: Type::Dynamic,
            source: Source::None,
        }
    }

    /// See [`Type::meet`].
    pub fn meet(&self, other: &SourcedType) -> SourcedType {
        let type_ = self.type_.meet(&other.type_);
        let source = self.source.meet(&other.source);
        SourcedType { type_, source }
    }

    /// Return whether `T` (`self`) is a subtype of `U` (`other`).
    ///
    /// What it means to be a subtype: if we take an arbitrary instance `t` of
    /// type `T`, is it an instance of `U`? There are three possible outcomes:
    /// 1. Yes, irrespective of `t`. `T` is a subtype of `U`: `T < U`.
    /// 2. No, irrespective of `t`. `T` is not a subtype of `U`: `not (T < U)`.
    /// 3. It depends on `t`, the two types are not orderable.
    /// Note that case 2 (`not (T < U)`) does not imply the converse! It does
    /// *not* mean that `U < T` holds!
    pub fn is_subtype_of(&self, other: &SourcedType) -> TypeDiff {
        match (&self.type_, &other.type_) {
            // Void is a subtype of everything, Dynamic a supertype of everything,
            // they are the top and bottom of the lattice.
            (Type::Void, _) => TypeDiff::Ok(self.clone()),
            (_, Type::Dynamic) => TypeDiff::Ok(self.clone()),

            // If I take any value from not-Void, it is not a member of Void.
            (_, Type::Void) => TypeDiff::Error(self.clone(), other.clone()),

            // If I take any arbitrary value, is it a member of some type T,
            // when T is not `Dynamic` (that case is already covered above)?
            // We don't know, it depends on T.
            (Type::Dynamic, _) => TypeDiff::Defer(other.clone()),

            // Every type is a subtype of itself. We preserve the right-hand
            // side as the type because usually that has the more interesting
            // source (it has a requirement). TODO: Do I need to meet the sources,
            // or will it work fine like this?
            (Type::Bool, Type::Bool) => TypeDiff::Ok(other.clone()),
            (Type::Int, Type::Int) => TypeDiff::Ok(other.clone()),
            (Type::Null, Type::Null) => TypeDiff::Ok(other.clone()),
            (Type::String, Type::String) => TypeDiff::Ok(other.clone()),

            // The collection types are covariant in their argument.
            // E.g. `List[Int] < List[Dynamic]`.
            (Type::List(l1), Type::List(l2)) => match l1.is_subtype_of(l2) {
                TypeDiff::Ok(..) => TypeDiff::Ok(self.clone()),
                TypeDiff::Defer(..) => TypeDiff::Defer(other.clone()),
                error => TypeDiff::List(error.into()),
            },
            (Type::Set(l1), Type::Set(l2)) => match l1.is_subtype_of(l2) {
                TypeDiff::Ok(..) => TypeDiff::Ok(self.clone()),
                TypeDiff::Defer(..) => TypeDiff::Defer(other.clone()),
                error => TypeDiff::Set(error.into()),
            },
            (Type::Dict(d1), Type::Dict(d2)) => {
                let dk = d1.key.is_subtype_of(&d2.key);
                let dv = d1.value.is_subtype_of(&d2.value);
                match (dk, dv) {
                    (TypeDiff::Ok(..), TypeDiff::Ok(..)) => TypeDiff::Ok(self.clone()),
                    // If we are unsure about any, then we are unsure about the
                    // entire thing.
                    (
                        TypeDiff::Ok(tk) | TypeDiff::Defer(tk),
                        TypeDiff::Ok(tv) | TypeDiff::Defer(tv),
                    ) => {
                        let dict = Dict { key: tk, value: tv };
                        let styp = SourcedType {
                            type_: Type::Dict(dict.into()),
                            source: Source::None,
                        };
                        TypeDiff::Defer(styp)
                    }
                    // If either the key or value is not a subtype, then the
                    // entire thing is not.
                    (k_diff, v_diff) => TypeDiff::Dict(k_diff.into(), v_diff.into()),
                }
            }

            // TODO: Check function types.

            // If we have any other combination of types, they are incompatible.
            _ => TypeDiff::Error(self.clone(), other.clone()),
        }
    }
}

/// The result of a static typecheck.
pub enum Typed {
    /// The type is known statically, and this is the most specific type we infer.
    Type(SourcedType),

    /// We can't check this statically, a runtime check is needed.
    ///
    /// If the runtime check passes, then the value fits the returned type.
    Defer(SourcedType),
}

impl TypeDiff {
    /// Report the diff as a type error, or extract its result.
    pub fn check(self, at: Span) -> Result<Typed> {
        match self {
            TypeDiff::Ok(t) => Ok(Typed::Type(t)),
            TypeDiff::Defer(t) => Ok(Typed::Defer(t)),
            TypeDiff::Error(expected, actual) => {
                // A top-level type error, we can report with a simple message.
                let err = at
                    .error("Type mismatch.")
                    .with_body(report_type_mismatch(&expected, &actual));

                // If we have it, explain why the expected type is expected.
                expected.source.clarify_error(&expected, err).err()
            }
            diff => {
                // If the error is nested somewhere inside a type, then we
                // resort to a more complex format where we first print the
                // type itself, with the error part replaced with a placeholder,
                // and then we add a secondary error to explain the placeholder.
                at.error("Type mismatch in type.")
                    .with_body(format!("TODO: Pretty-print: {diff:?}"))
                    .err()
            }
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

impl AsTypeName for SourcedType {
    fn format_type(&self) -> Doc<'static> {
        // If we are generating a type error, and it has Dynamic in there as
        // the top-level type that we format, then that's a bug in the
        // typechecker, because Dynamic is just a way to say "I don't know what
        // the type is yet", you can't violate a type expectation this way.
        debug_assert_ne!(
            self.type_,
            Type::Dynamic,
            "The Dynamic type should never be a direct cause of a type error.",
        );
        format_type(&self.type_).into_owned()
    }

    fn is_atom(&self) -> bool {
        self.type_.is_atom()
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

/// Create a builtin type, used by the [`make_type`] macro.
///
/// Not intended to be public, but it has to be for the macro to work.
/// TODO: Is it worth it? Should we just use the type expr parser? But then we
/// can't handle type variables ...
#[doc(hidden)]
pub fn builtin(type_: Type) -> SourcedType {
    SourcedType {
        type_,
        source: Source::Builtin,
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
    (Int) => { builtin(Type::Int) };
    (Bool) => { builtin(Type::Bool) };
    (Dynamic) => { builtin(Type::Dynamic) };
    (String) => { builtin(Type::String) };
    ([$elem:tt]) => { builtin(Type::List(Rc::new(make_type!($elem)))) };
    ({$elem:tt}) => { builtin(Type::Set(Rc::new(make_type!($elem)))) };
    ({$k:tt: $v:tt}) => {
        builtin(Type::Dict(Rc::new(Dict {
            key: make_type!($k),
            value: make_type!($v),
        })))
    };
    ((fn ($( $arg_name:ident: $arg_type:tt ),*) -> $result:tt)) => {
        builtin(Type::Function(Rc::new(
            make_function!(($( $arg_name:$arg_type ),*) -> $result)
        )))
    };
}
pub(crate) use make_type;

/// Rust eDSL for writing RCL function types.
///
/// See also [`make_type!`] for the syntax. This does not include the enclosing
/// `(fn ...)`, parens and `fn`, only the `...` is input to this macro.
macro_rules! make_function {
    (($( $arg_name:ident: $arg_type:tt ),*) -> $result:tt) => {
        Function {
            // TODO: Include the argument names in types? Or at least elsewhere?
            args: vec![ $( make_type!($arg_type) ),* ],
            result: make_type!($result),
        }
    };
}
pub(crate) use make_function;
