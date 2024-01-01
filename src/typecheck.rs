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

use crate::ast::{Expr, Seq};
use crate::error::{IntoError, Result};
use crate::fmt_rcl::format_rcl;
use crate::fmt_type::format_type;
use crate::markup::Markup;
use crate::pprint::{concat, indent, Doc};
use crate::runtime::{self, Value};
use crate::source::Span;
use crate::types::{self, Type};

pub type Env = crate::env::Env<Type>;

// Temporarily add a way to mark things as unfinished, without blocking the
// fuzzer on it.
#[cfg(not(fuzzing))]
macro_rules! unfinished {
    ($($arg:tt)*) => { unimplemented!($($arg)*) }
}

#[cfg(fuzzing)]
macro_rules! unfinished {
    ($($arg:tt)*) => {
        Ok(())
    };
}

/// Return the type prelude, all the types that are in scope by default.
pub fn prelude() -> Env {
    let mut env = Env::new();

    // The primitive types are in scope by default.
    env.push("Bool".into(), Type::Bool);
    env.push("Int".into(), Type::Int);
    env.push("Null".into(), Type::Null);
    env.push("String".into(), Type::String);

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
        (Type::Dict(kv), Value::Dict(xs)) => {
            for (k, v) in xs.iter() {
                // TODO: Add typecheck context, otherwise it might look like we
                // blame at top-level let.
                check_value(at, &kv.key, k)?;
                check_value(at, &kv.value, v)?;
            }
            Ok(())
        }

        // The function type describes the different callable values.
        (Type::Function(fn_type), Value::Function(fn_val)) => {
            check_function_value(at, fn_type, fn_val)
        }
        (Type::Function { .. }, Value::BuiltinFunction { .. }) => {
            unfinished!("TODO: Typecheck function for BuiltinFunction.")
        }
        (Type::Function { .. }, Value::BuiltinMethod { .. }) => {
            unfinished!("TODO: Typecheck function for BuiltinMethod.")
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

/// Report a static type error.
///
/// A static type error can be reported at typecheck time based on the AST, so
/// the culprit is a syntactic construct, not a runtime value.
///
/// The `actual` message should be in the form of “Found «actual» instead”.
fn type_error<T>(at: Span, expected: &Type, actual: &'static str) -> Result<T> {
    // TODO: Generate a briefer error when the expected type is a primitive type.
    at.error("Type mismatch.")
        .with_body(concat! {
            "Expected a value that fits this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { format_type(expected).into_owned() }
            Doc::HardBreak Doc::HardBreak
            "Found " Doc::from(actual).with_markup(Markup::Type) " instead."
        })
        .err()
}

pub struct TypeChecker {}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {}
    }

    /// Check that an expression fits the expected type.
    ///
    /// This also updates the AST to replace statically known constructs.
    /// For example, methods might be resolved at runtime, so `Expr::Field` can
    /// be specialized. Typechecks that are statically removed can also be
    /// removed from the evaluation phase.
    pub fn check_expr(
        &mut self,
        env: &mut Env,
        expected: &Type,
        expr_span: Span,
        expr: &mut Expr,
    ) -> Result<()> {
        match expr {
            Expr::Stmt { stmt: _, body: _ } => {
                unfinished!("TODO: Typecheck statement.")
            }

            Expr::Import { path_span, path: _ } => {
                // TODO: Confirm that the path is a string literal,
                // we can do that here!

                // The type of an import is always `Dynamic`. Though if that is
                // the case, that removes one justification for having it be a
                // keyword instead of a builtin method `std.import`. Because we
                // can just type it: `std.import: (fname: String) -> Dynamic`.
                // TODO: Re-evaluate that.
                // TODO 2: Although ... I could feed the expected type all the
                // way through the typecheck of the imported document. Then we
                // can get way better type errors for e.g. importing json
                // documents, because we can pinpoint violating *spans* instead
                // of runtime values! We could even have both, a function in std,
                // and the import construct. Then the function can import any
                // dynamic path. But a function is maybe not needed.
                // TODO 3: Also, import statements are good for multithreading,
                // see also `ideas/multithreading.md` that I just wrote.
                // ---
                // If the expected type is also dynamic, that is fine; if the
                // expected type is different, then we need to insert a runtime
                // type check here.
                if expected == &Type::Dynamic {
                    Ok(())
                } else {
                    // TODO: The path span is the wrong span to blame the
                    // type error on in case of a runtime type error; we
                    // should get the obligation site (the annotation on the
                    // let) from the caller.
                    let blame_span = *path_span;
                    // Wrap the existing expr in a `CheckType`. We have to
                    // sacrifice a temporary NullLit to the borrow checker.
                    let mut tmp = Expr::NullLit;
                    std::mem::swap(&mut tmp, expr);
                    *expr = Expr::CheckType {
                        span: blame_span,
                        type_: expected.clone(),
                        body: Box::new(tmp),
                    };
                    Ok(())
                }
            }

            Expr::BraceLit(_seq) => {
                unfinished!("TODO: Typecheck brace literal.")
            }

            Expr::BracketLit(seqs) => {
                let element_type = match expected {
                    // If we don't have any requirements and we got a list,
                    // then we don't have any requirements on the element type.
                    Type::Dynamic => expected,
                    // If we expected a list and got one, so far so good, but
                    // now the elements have to match as well.
                    Type::List(t) => t,
                    _ => return type_error(expr_span, expected, "List"),
                };
                for seq in seqs {
                    self.check_seq_scalar(env, element_type, seq)?;
                }
                Ok(())
            }

            Expr::NullLit => match expected {
                Type::Dynamic => Ok(()),
                Type::Null => Ok(()),
                _ => type_error(expr_span, expected, "Null"),
            },

            Expr::BoolLit(..) => match expected {
                Type::Dynamic => Ok(()),
                Type::Bool => Ok(()),
                _ => type_error(expr_span, expected, "Bool"),
            },

            Expr::StringLit(..) => match expected {
                Type::Dynamic => Ok(()),
                Type::String => Ok(()),
                _ => type_error(expr_span, expected, "String"),
            },

            Expr::IntegerLit(..) => match expected {
                Type::Dynamic => Ok(()),
                Type::Int => Ok(()),
                _ => type_error(expr_span, expected, "Int"),
            },

            // Format strings evaluate to string values, so they fit string types.
            Expr::Format(..) => match expected {
                Type::Dynamic => Ok(()),
                Type::String => Ok(()),
                _ => type_error(expr_span, expected, "String"),
            },

            Expr::IfThenElse {
                condition_span,
                condition,
                body_then,
                body_else,
                ..
            } => {
                // The condition always has to be a boolean.
                // TODO: Delete the runtime type check in the evaluator, this is
                // now a static typecheck. See if we can make the error friendly.
                self.check_expr(env, &Rc::new(Type::Bool), *condition_span, condition)?;
                // TODO: Record the spans on then and else. For now I'll just
                // put in the condition span as a temporary hack because I don't
                // want to change everything all over the place.
                self.check_expr(env, expected, *condition_span, body_then)?;
                self.check_expr(env, expected, *condition_span, body_else)?;
                Ok(())
            }

            Expr::Var { span: _, ident } => {
                match env.lookup(ident) {
                    // TODO: We can remove this check from the evaluator and
                    // turn it into an assert.
                    None => unfinished!("TODO: Report unknown variable."),
                    Some(t) => self.check_subtype(expected, t),
                }
            }

            Expr::Field { .. } => unfinished!("TODO: Implement typechecking fields."),

            Expr::Function { .. } => unfinished!("TODO: Implement typechecking functions."),

            Expr::Call { .. } => unfinished!("TODO: Implement typechecking calls."),

            Expr::Index { .. } => unfinished!("TODO: Implement typechecking indexing."),

            other => unfinished!("TODO: Implement typechecking {other:?}."),
        }
    }

    pub fn check_seq_scalar(
        &mut self,
        _env: &mut Env,
        _expected: &Type,
        _seq: &mut Seq,
    ) -> Result<()> {
        unfinished!("TODO: Typecheck seq.")
    }

    /// Check that `actual` is a subtype of `expected`.
    ///
    /// When a type is known for a particular variable, but we then try to use
    /// that variable in a context where a particular type is expected, we have
    /// to verify that the known type fits the expected type. For example, a
    /// record that only has `Int` fields would fit the type `Dict[String, Int]`,
    /// but not the other way around.
    pub fn check_subtype(&mut self, _expected: &Type, _actual: &Type) -> Result<()> {
        unfinished!("TODO: Check fits.")
    }
}
