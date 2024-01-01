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

use crate::error::{IntoError, Result};
use crate::fmt_rcl::format_rcl;
use crate::pprint::{concat, indent, Doc};
use crate::runtime::Value;
use crate::source::Span;
use crate::types::Type;

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
            for v in xs {
                // TODO: Extend the error with the list index, possibly with
                // context, otherwise it might look like we blame a top-level
                // let.
                check_value(at, element_type, v)?;
            }
            Ok(())
        }
        (Type::Set(element_type), Value::Set(xs)) => {
            for v in xs {
                // TODO: Add typecheck context, otherwise it might look like we
                // blame a top-level let.
                check_value(at, element_type, v)?;
            }
            Ok(())
        }
        (Type::Dict(key_type, value_type), Value::Dict(xs)) => {
            for (k, v) in xs {
                // TODO: Add typecheck context, otherwise it might look like we
                // blame at top-level let.
                check_value(at, key_type, k)?;
                check_value(at, value_type, v)?;
            }
            Ok(())
        }

        // The function type describes the different callable values.
        (Type::Function { .. }, Value::Function { .. }) => {
            unimplemented!("TODO: Typecheck function for Function.")
        }
        (Type::Function { .. }, Value::BuiltinFunction { .. }) => {
            unimplemented!("TODO: Typecheck function for BuiltinFunction.")
        }
        (Type::Function { .. }, Value::BuiltinMethod { .. }) => {
            unimplemented!("TODO: Typecheck function for BuiltinMethod.")
        }

        _ => at
            .error("Type mismatch.")
            .with_body(concat! {
                "Expected a value that fits the type (TODO: pprint-type):"
                Doc::HardBreak Doc::HardBreak
                indent! {
                    format!("{type_:?}")
                }
                Doc::HardBreak Doc::HardBreak
                "But got value:"
                Doc::HardBreak Doc::HardBreak
                indent! {
                    format_rcl(value).into_owned()
                }
            })
            .err(),
    }
}
