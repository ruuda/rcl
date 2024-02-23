// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Representations of types.

use std::cmp::Ordering;
use std::rc::Rc;

use crate::ast::{CallArg, Ident};
use crate::error::{IntoError, Result};
use crate::fmt_type::format_type;
use crate::markup::Markup;
use crate::pprint::{concat, Doc};
use crate::source::Span;
use crate::type_diff::{Mismatch, TypeDiff};
use crate::type_source::Source;

/// A type.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Type {
    /// Any value, a more concrete type is not statically known.
    ///
    /// This is the top of the type lattice, it is a supertype of all types.
    Any,

    /// The type of unreachable code.
    ///
    /// This is the bottom of the type lattice, it is a subtype of any type.
    Void,

    /// The primitive type `Null`.
    Null,

    /// The primitive type `Bool`.
    Bool,

    /// The primitive type `Int`.
    Int,

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

impl Type {
    /// Return whether the type is not composite, i.e. is not composed of other types.
    pub fn is_atom(&self) -> bool {
        matches!(
            self,
            Type::Bool | Type::Int | Type::Null | Type::String | Type::Void | Type::Any,
        )
    }
}

/// The type parameters for the `Dict` type.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Dict {
    pub key: SourcedType,
    pub value: SourcedType,
}

/// An argument in a function type.
///
/// The names are ignored for equality and comparison purposes, but we track
/// them to enable more helpful error messages. The name and span can exist
/// separately. For example, builtin functions have no argument span, but
/// user-defined functions do.
#[derive(Clone, Debug)]
pub struct FunctionArg {
    /// The name of this argument.
    pub name: Option<Ident>,
    /// The span where the argument is defined in the function definition.
    pub span: Option<Span>,
    /// The type of the argument.
    pub type_: SourcedType,
}

impl PartialEq for FunctionArg {
    fn eq(&self, other: &Self) -> bool {
        self.type_.eq(&other.type_)
    }
}

impl PartialOrd for FunctionArg {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.type_.partial_cmp(&other.type_)
    }
}

impl Eq for FunctionArg {}

impl Ord for FunctionArg {
    fn cmp(&self, other: &Self) -> Ordering {
        self.type_.cmp(&other.type_)
    }
}

impl FunctionArg {
    /// Check whether a function argument is a subtype.
    ///
    /// Function arguments are contravariant: the subtype relationship
    /// goes the other way. For example, `(Any) -> Int` is a subtype of
    /// `(Int) -> Int`: in every case where we need to call the latter,
    /// we can call the former. So although `Int ≤ Any`, as function args
    /// we have the opposite: `Arg(Any) ≤ Arg(Int)`.
    pub fn is_subtype_of(&self, other: &FunctionArg) -> TypeDiff<FunctionArg> {
        match other.type_.is_subtype_of(&self.type_) {
            TypeDiff::Ok(t) => TypeDiff::Ok(FunctionArg {
                // Ok returns the most specific type, so we take the name and
                // span from there, though if it has no name, we take the other
                // name.
                name: other.name.as_ref().or(self.name.as_ref()).cloned(),
                span: other.span,
                type_: t,
            }),
            TypeDiff::Defer(t) => TypeDiff::Defer(FunctionArg {
                // Defer returns the most generic type, so we take the name and
                // span from there.
                name: self.name.as_ref().or(other.name.as_ref()).cloned(),
                span: self.span,
                type_: t,
            }),
            TypeDiff::Error(err) => TypeDiff::Error(err),
        }
    }
}

/// A function type.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Function {
    /// The function arguments, including optional names.
    ///
    /// The names are ignored for equality and comparison purposes, but we track
    /// them to enable more helpful error messages.
    pub args: Vec<FunctionArg>,

    /// The result type, also called return type.
    pub result: SourcedType,
}

impl Function {
    /// Confirm that there are as many provided arguments as expected arguments.
    ///
    /// If not, report that as an error on the proper spans, with as much
    /// information as we have.
    pub fn check_arity<T>(
        &self,
        function_name: Option<&str>,
        provided_args: &[CallArg<T>],
        call_close: Span,
    ) -> Result<()> {
        if provided_args.len() == self.args.len() {
            return Ok(());
        }

        let fn_name = match function_name {
            None => "The function".into(),
            Some(name) => concat! { "'" Doc::highlight(name) "'" },
        };

        let n_args = match self.args.len() {
            1 => "1 argument".to_string(),
            n => format!("{n} arguments"),
        };

        if provided_args.len() < self.args.len() {
            let missing_arg = &self.args[provided_args.len()];

            let missing_msg = match &missing_arg.name {
                None => "Missing argument. ".into(),
                Some(name) => concat! {
                  "Missing argument '" Doc::highlight(name.as_ref()) "'. "
                },
            };

            let msg = concat! {
                missing_msg fn_name " takes " n_args ", but got "
                provided_args.len().to_string()
                "."
            };

            let error = call_close.error(msg.into_owned());

            match missing_arg.span {
                None => error.err(),
                Some(arg_span) => error.with_note(arg_span, "Argument defined here.").err(),
            }
        } else {
            let excess_arg = &provided_args[self.args.len()];
            let msg = concat! {
                "Unexpected argument. " fn_name " takes " n_args ", but got "
                provided_args.len().to_string()
                "."
            };
            // TODO: Store a reference to the function span in the type,
            // so we can add a note with the function definition, just like with
            // the the missing argument.
            excess_arg.span.error(msg.into_owned()).err()
        }
    }

    pub fn is_subtype_of(self: &Rc<Self>, other: &Rc<Function>) -> TypeDiff<Rc<Function>> {
        // If there is an arity mismatch, report that as a normal diff.
        // Unfortunately at this point we don't have access to the type sources,
        // so this check only kicks in in places where we have a `Function` but
        // not the surrounding type. In `Type::is_subtype_of` we do the check
        // that preserves the sources.
        if self.args.len() != other.args.len() {
            let err = Mismatch::Atom {
                actual: SourcedType {
                    type_: Type::Function(self.clone()),
                    source: Source::None,
                },
                expected: SourcedType {
                    type_: Type::Function(other.clone()),
                    source: Source::None,
                },
            };
            return TypeDiff::Error(err);
        }

        let mut is_err = false;
        let mut is_defer = false;

        let mut args = Vec::with_capacity(self.args.len());
        let mut arg_diffs = Vec::new();

        for (a1, a2) in self.args.iter().zip(other.args.iter()) {
            // Note, the contravariance is built into the `FunctionArg` check,
            // so here we do check that a1 ≤ a2.
            match a1.is_subtype_of(a2) {
                TypeDiff::Ok(t) | TypeDiff::Defer(t) if is_err => {
                    arg_diffs.push(TypeDiff::Ok(t));
                }
                TypeDiff::Ok(t) => args.push(t),
                TypeDiff::Defer(t) => {
                    is_defer = true;
                    args.push(t);
                }
                err if is_err => arg_diffs.push(err),
                err => {
                    is_err = true;
                    for not_err in args.drain(..) {
                        arg_diffs.push(TypeDiff::Ok(not_err));
                    }
                    arg_diffs.push(err);
                }
            }
        }
        let result_type = match self.result.is_subtype_of(&other.result) {
            TypeDiff::Ok(t) | TypeDiff::Defer(t) if is_err => {
                let err = Mismatch::Function(arg_diffs, TypeDiff::Ok(t).into());
                return TypeDiff::Error(err);
            }
            TypeDiff::Ok(t) => t,
            TypeDiff::Defer(t) => {
                is_defer = true;
                t
            }
            err if is_err => {
                let err = Mismatch::Function(arg_diffs, err.into());
                return TypeDiff::Error(err);
            }
            err => {
                let err =
                    Mismatch::Function(args.into_iter().map(TypeDiff::Ok).collect(), err.into());
                return TypeDiff::Error(err);
            }
        };
        let fn_type = Function {
            args,
            result: result_type,
        };
        if is_defer {
            TypeDiff::Defer(fn_type.into())
        } else {
            // In this case, even though `self` should be equivalent to `fn_type`,
            // it is not identical. The sources of the types can differ, and we
            // may have collected names for unnamed arguments.
            TypeDiff::Ok(fn_type.into())
        }
    }
}

/// A type and its source.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct SourcedType {
    pub type_: Type,
    pub source: Source,
}

impl SourcedType {
    /// Construct [`Type::Void`] with empty collection source.
    pub fn void(at: Span) -> SourcedType {
        SourcedType {
            type_: Type::Void,
            source: Source::EmptyCollection(at),
        }
    }

    /// Construct [`Type::Any`] without source.
    pub const fn any() -> SourcedType {
        SourcedType {
            type_: Type::Any,
            source: Source::None,
        }
    }

    /// Return the least possible supertype of the two types.
    ///
    /// The meet is a type `T` such that `self` and `other` are both subtypes
    /// of `T`.
    /// TODO: This should take self and other by value.
    pub fn meet(&self, other: &SourcedType) -> SourcedType {
        let src_meet = self.source.meet(&other.source);
        let (type_, source) = match (&self.type_, &other.type_) {
            // Anything involving any becomes any, anything involving
            // void becomes the other thing, these are the top and bottom of
            // the type lattice.
            (Type::Any, _) => (Type::Any, self.source),
            (_, Type::Any) => (Type::Any, other.source),
            (Type::Void, that) => (that.clone(), other.source),
            (this, Type::Void) => (this.clone(), self.source),

            // If we have matching primitive types, they are preserved.
            (Type::Bool, Type::Bool) => (Type::Bool, src_meet),
            (Type::Int, Type::Int) => (Type::Int, src_meet),
            (Type::Null, Type::Null) => (Type::Null, src_meet),
            (Type::String, Type::String) => (Type::String, src_meet),

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
                // TODO: If the types are the same on both sides, we can meet the sources.
                (Type::Dict(dm), Source::None)
            }
            (Type::List(l1), Type::List(l2)) => {
                let type_ = Type::List({
                    if Rc::ptr_eq(l1, l2) {
                        l1.clone()
                    } else {
                        Rc::new(l1.meet(l2))
                    }
                });
                // TODO: If the types are the same on both sides, we can meet the sources.
                (type_, Source::None)
            }
            (Type::Set(s1), Type::Set(s2)) => {
                let type_ = Type::Set({
                    if Rc::ptr_eq(s1, s2) {
                        s1.clone()
                    } else {
                        Rc::new(s1.meet(s2))
                    }
                });
                // TODO: If the types are the same on both sides, we can meet the sources.
                (type_, Source::None)
            }

            // TODO: Support meeting functions.
            (Type::Function(_), Type::Function(_)) => (Type::Any, Source::None),

            // Any two values that mismatch, we can't describe with a single
            // static type, but that doesn't mean it's a type error, the program
            // may still be valid at runtime. E.g, I have a list with a function
            // and an int. If the program only ever calls `list[0]` and performs
            // integer addition on `list[1]`, that is fine. We can type the list
            // as `List[Any]`.
            _ => (Type::Any, Source::None),
        };
        SourcedType { type_, source }
    }

    /// Return whether `T` (`self`) is a subtype of `U` (`other`).
    ///
    /// What it means to be a subtype: if we take an arbitrary instance `t` of
    /// type `T`, is it an instance of `U`? There are three possible outcomes:
    /// 1. Yes, irrespective of `t`. `T` is a subtype of `U`: `T ≤ U`.
    /// 2. No, irrespective of `t`. `T` is not a subtype of `U`: `not (T ≤ U)`.
    /// 3. It depends on `t`, the two types are not orderable.
    /// Note that case 2 (`not (T ≤ U)`) does not imply the converse! It does
    /// *not* mean that `U ≤ T` holds!
    ///
    /// Also, we do make some exceptions to this, because it's more helpful to
    /// catch type errors than to be able to type any possible expression that
    /// can be evaluated. For example, `not (Int ≤ String)` is definitely true.
    /// We would like `List` to be covariant in its argument, so we could say
    /// `List[T] ≤ List[U] <=> T ≤ U`. We would get `not (List[Int] ≤ List[String])`.
    /// But that violates the above definition, because `[]` is an instance of
    /// both! But in this case, reporting an error if the element types mismatch
    /// is helpful, so we won't make `[]` an exception that causes a runtime
    /// check.
    pub fn is_subtype_of(&self, other: &SourcedType) -> TypeDiff<SourcedType> {
        match (&self.type_, &other.type_) {
            // Void is a subtype of everything, Any a supertype of everything,
            // they are the top and bottom of the lattice.
            (Type::Void, _) => TypeDiff::Ok(self.clone()),
            (_, Type::Any) => TypeDiff::Ok(self.clone()),

            // If I take any value from not-Void, it is not a member of Void.
            (_, Type::Void) => TypeDiff::Error(Mismatch::Atom {
                actual: self.clone(),
                expected: other.clone(),
            }),

            // If I take any arbitrary value, is it a member of some type T,
            // when T is not `Any` (that case is already covered above)?
            // We don't know, it depends on T.
            (Type::Any, _) => TypeDiff::Defer(other.clone()),

            // Every type is a subtype of itself. We preserve the right-hand
            // side as the type because usually that has the more interesting
            // source (it has a requirement). TODO: Do I need to meet the sources,
            // or will it work fine like this?
            (Type::Bool, Type::Bool) => TypeDiff::Ok(other.clone()),
            (Type::Int, Type::Int) => TypeDiff::Ok(other.clone()),
            (Type::Null, Type::Null) => TypeDiff::Ok(other.clone()),
            (Type::String, Type::String) => TypeDiff::Ok(other.clone()),

            // The collection types are covariant in their argument.
            // E.g. `List[Int] < List[Any]`.
            (Type::List(l1), Type::List(l2)) => match l1.is_subtype_of(l2) {
                TypeDiff::Ok(..) => TypeDiff::Ok(self.clone()),
                TypeDiff::Defer(..) => TypeDiff::Defer(other.clone()),
                error => TypeDiff::Error(Mismatch::List(error.into())),
            },
            (Type::Set(l1), Type::Set(l2)) => match l1.is_subtype_of(l2) {
                TypeDiff::Ok(..) => TypeDiff::Ok(self.clone()),
                TypeDiff::Defer(..) => TypeDiff::Defer(other.clone()),
                error => TypeDiff::Error(Mismatch::Set(error.into())),
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
                    (k_diff, v_diff) => {
                        TypeDiff::Error(Mismatch::Dict(k_diff.into(), v_diff.into()))
                    }
                }
            }
            (Type::Function(f1), Type::Function(f2)) => {
                if f1.args.len() != f2.args.len() {
                    // If we have an arity mismatch, report that directly, because
                    // then we can preserve the sources of the types.
                    TypeDiff::Error(Mismatch::Atom {
                        actual: self.clone(),
                        expected: other.clone(),
                    })
                } else {
                    match f1.is_subtype_of(f2) {
                        TypeDiff::Ok(..) => TypeDiff::Ok(self.clone()),
                        TypeDiff::Defer(f) => {
                            let styp = SourcedType {
                                type_: Type::Function(f),
                                source: Source::None,
                            };
                            TypeDiff::Defer(styp)
                        }
                        TypeDiff::Error(err) => TypeDiff::Error(err),
                    }
                }
            }

            // If we have any other combination of types, they are incompatible.
            _ => TypeDiff::Error(Mismatch::Atom {
                actual: self.clone(),
                expected: other.clone(),
            }),
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
        format_type(&self.type_).into_owned()
    }

    fn is_atom(&self) -> bool {
        self.type_.is_atom()
    }
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
    (Any) => { builtin(Type::Any) };
    (Int) => { builtin(Type::Int) };
    (Bool) => { builtin(Type::Bool) };
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
            args: vec![
                $( FunctionArg {
                    name: Some(stringify!($arg_name).into()),
                    span: None,
                    type_: make_type!($arg_type),
                }),*
            ],
            result: make_type!($result),
        }
    };
}
pub(crate) use make_function;

#[cfg(test)]
mod test {
    use super::{Function, FunctionArg, Source, SourcedType, Type};
    use crate::source::{DocId, Span};

    fn mk_type(type_: Type) -> SourcedType {
        SourcedType {
            type_,
            source: Source::None,
        }
    }

    #[test]
    fn function_ord_ignores_names() {
        let mut f1 = Function {
            args: vec![
                FunctionArg {
                    name: Some("a".into()),
                    span: None,
                    type_: mk_type(Type::Int),
                },
                FunctionArg {
                    name: Some("b".into()),
                    span: None,
                    type_: mk_type(Type::Bool),
                },
            ],
            result: mk_type(Type::String),
        };
        let mut f2 = f1.clone();
        assert_eq!(f1, f2);

        // Even when we delete the names entirely, the functions should still
        // be equal.
        f2.args[0].name = None;
        assert_eq!(f1, f2);

        // Or when we add spans, it shouldn't affect things.
        f2.args[0].span = Some(Span::new(DocId(0), 0, 0));
        assert_eq!(f1, f2);

        // Void orders before String.
        f1.result = mk_type(Type::Void);
        assert!(f1 < f2);

        // Void orders before Int.
        f1.result = mk_type(Type::String);
        f1.args[0].type_ = mk_type(Type::Void);
        assert!(f1 < f2);

        // Void orders before Bool.
        f1.args[0].type_ = mk_type(Type::Int);
        f1.args[1].type_ = mk_type(Type::Void);
        assert!(f1 < f2);
        // Also trigger the `cmp` method, because `<` doesn't.
        assert_eq!(f1.cmp(&f2), std::cmp::Ordering::Less);

        // Now we are back to the initial equality (but with names changed).
        f1.args[1].type_ = mk_type(Type::Bool);
        assert_eq!(f1, f2);

        // Having fewer args makes it order before.
        f1.args.pop();
        assert!(f1 < f2);
    }
}
