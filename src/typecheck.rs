// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! A checker for static annotations and runtime dynamic types.
//!
//! The same value in RCL can be described by multiple types, values do not have
//! unique types. For example, `[]` is a valid value for the type `List[Int]`
//! but also for the type `List[String]`. Therefore we check whether a value
//! _fits_ a particular type, and that same value may fit multiple types.

use std::rc::Rc;

use crate::env::Env;
use crate::error::{IntoError, Result};
use crate::fmt_rcl::format_rcl;
use crate::fmt_type::format_type;
use crate::pprint::{concat, indent, Doc};
use crate::runtime::{self, Value};
use crate::source::Span;
use crate::types::{self, Type};

/// Return the type prelude, all the types that are in scope by default.
pub fn prelude() -> Env<Rc<Type>> {
    let mut env = Env::new();

    // The primitive types are in scope by default.
    env.push("Bool".into(), Rc::new(Type::Bool));
    env.push("Int".into(), Rc::new(Type::Int));
    env.push("Null".into(), Rc::new(Type::Null));
    env.push("String".into(), Rc::new(Type::String));

    // TODO: What to do about Dict, List, and Set? They are technically type
    // constructors. Should those exist, at this level, if they can't be
    // user-defined? It's easier to implement if we just hard-code those few,
    // but then if you write `let xs: List = [1, 2, 3]`, it will lead to a
    // confusing error.

    env
}

/// Confirm that the value fits the given type.
pub fn check_value(at: Span, type_: &Type, value: &Value) -> Result<()> {
    match (type_, value) {
        // The primitive types.
        (Type::Bool, Value::Bool(..)) => Ok(()),
        (Type::Int, Value::Int(..)) => Ok(()),
        (Type::Null, Value::Null) => Ok(()),
        (Type::String, Value::String(..)) => Ok(()),

        // Collection types.
        (Type::List(element_type), Value::List(xs)) => {
            for v in xs.iter() {
                // TODO: Extend the error with the list index, possibly with
                // context, otherwise it might look like we blame a top-level
                // let.
                check_value(at, element_type, v)?;
            }
            Ok(())
        }
        (Type::Set(element_type), Value::Set(xs)) => {
            for v in xs.iter() {
                // TODO: Add typecheck context, otherwise it might look like we
                // blame a top-level let.
                check_value(at, element_type, v)?;
            }
            Ok(())
        }
        (Type::Dict(key_type, value_type), Value::Dict(xs)) => {
            for (k, v) in xs.iter() {
                // TODO: Add typecheck context, otherwise it might look like we
                // blame at top-level let.
                check_value(at, key_type, k)?;
                check_value(at, value_type, v)?;
            }
            Ok(())
        }

        // The function type describes the different callable values.
        (Type::Function(fn_type), Value::Function(fn_val)) => {
            check_function_value(at, fn_type, fn_val)
        }
        (Type::Function { .. }, Value::BuiltinFunction { .. }) => {
            #[cfg(fuzzing)]
            return Ok(());
            #[cfg(not(fuzzing))]
            unimplemented!("TODO: Typecheck function for BuiltinFunction.")
        }
        (Type::Function { .. }, Value::BuiltinMethod { .. }) => {
            #[cfg(fuzzing)]
            return Ok(());
            #[cfg(not(fuzzing))]
            unimplemented!("TODO: Typecheck function for BuiltinMethod.")
        }

        _ => at
            .error("Type mismatch.")
            .with_body(concat! {
                "Expected a value that fits this type:"
                Doc::HardBreak Doc::HardBreak
                indent! { format_type(type_).into_owned() }
                Doc::HardBreak Doc::HardBreak
                "But got this value:"
                Doc::HardBreak Doc::HardBreak
                indent! { format_rcl(value).into_owned() }
            })
            .err(),
    }
}

pub fn check_function_value(
    at: Span,
    fn_type: &types::Function,
    fn_value: &runtime::Function,
) -> Result<()> {
    if fn_type.args.len() != fn_value.args.len() {
        return at
            .error(concat! {
                "Expected a function that takes "
                fn_type.args.len().to_string()
                " arguments, but got a function that takes "
                fn_value.args.len().to_string()
                " arguments."
            })
            .with_note(fn_value.span, "Function defined here.")
            .err();
    }

    // TODO: Now that the arity is confirmed, we have to perform a static
    // typecheck on the function body, with the arguments bound to the provided
    // argument types, and then check that the resulting value fits the result
    // type. So we have to implement the static typechecker ...
    at.error("Typechecking function values is not yet supported.")
        .err()
}
