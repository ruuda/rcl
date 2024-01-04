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

use crate::ast::{Expr, Seq, Stmt, Type as AType, UnOp};
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

/// Parse a type expression.
fn eval_type_expr(expr: &AType) -> Result<Type> {
    match expr {
        AType::Term { span, name } => match name.as_ref() {
            "Bool" => Ok(Type::Bool),
            "Int" => Ok(Type::Int),
            "Null" => Ok(Type::Null),
            "String" => Ok(Type::String),
            "Dict" => span
                .error("Expected a concrete type, but found uninstantiated generic type.")
                .with_help(concat! {
                    "'" Doc::highlight("Dict") "' without type parameters cannot be used directly."
                    Doc::SoftBreak
                    "Specify a key and value type, e.g. '" Doc::highlight("Dict[String, Int]") "'."
                })
                .err(),
            "List" => span
                .error("Expected a concrete type, but found uninstantiated generic type.")
                .with_help(concat! {
                    "'" Doc::highlight("List") "' without type parameters cannot be used directly."
                    Doc::SoftBreak
                    "Specify an element type, e.g. '" Doc::highlight("List[String]") "'."
                })
                .err(),
            "Set" => span
                .error("Expected a concrete type, but found uninstantiated generic type.")
                .with_help(concat! {
                    "'" Doc::highlight("Set") "' without type parameters cannot be used directly."
                    Doc::SoftBreak
                    "Specify an element type, e.g. '" Doc::highlight("Set[String]") "'."
                })
                .err(),
            _ => span.error("Unknown type.").err(),
        },
        AType::Function { args, result } => {
            let args_types = args
                .iter()
                .map(|t| eval_type_expr(t))
                .collect::<Result<Vec<_>>>()?;
            let result_type = eval_type_expr(result)?;
            let fn_type = types::Function {
                args: args_types,
                result: result_type,
            };
            Ok(Type::Function(Rc::new(fn_type)))
        }
        AType::Apply { span, name, args } => {
            let args_types = args
                .iter()
                .map(|t| eval_type_expr(t))
                .collect::<Result<Vec<_>>>()?;
            eval_type_apply(*span, name.as_ref(), &args_types)
        }
    }
}

/// Evaluate type constructor application (generic instantiation).
fn eval_type_apply(name_span: Span, name: &str, args: &[Type]) -> Result<Type> {
    match name {
        "Dict" => match args {
            [tk, tv] => {
                let dict = types::Dict {
                    key: tk.clone(),
                    value: tv.clone(),
                };
                Ok(Type::Dict(Rc::new(dict)))
            }
            // TODO: We can point at the excess or missing arg for a
            // friendlier error, but better to do that in a general way
            // when we add arbitrary type contructors to `types::Type`.
            _ => name_span
                .error(concat! {
                    "Type 'Dict' takes two type parameters (key and value), but got "
                    args.len().to_string() "."
                })
                .err(),
        },
        "List" => match args {
            [te] => Ok(Type::List(Rc::new(te.clone()))),
            // TODO: As above for dict, we can do a better job of the error.
            _ => name_span
                .error(concat! {
                    "Type 'List' takes one type parameter (the element type), but got "
                    args.len().to_string() "."
                })
                .err(),
        },
        "Set" => match args {
            [te] => Ok(Type::Set(Rc::new(te.clone()))),
            // TODO: As above for dict, we can do a better job of the error.
            _ => name_span
                .error(concat! {
                    "Type 'Set' takes one type parameter (the element type), but got "
                    args.len().to_string() "."
                })
                .err(),
        },
        _ => name_span.error("Unknown generic type.").err(),
    }
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
            "Expected this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { format_type(expected).into_owned() }
            Doc::HardBreak Doc::HardBreak
            "Found " Doc::from(actual).with_markup(Markup::Type) " instead."
        })
        .err()
}

/// Wrap the AST node in an `Expr::CheckType`.
fn wrap_in_check_type(expr: &mut Expr, span: Span, expected: Type) {
    // Wrap the existing expr in a `CheckType`. We have to
    // sacrifice a temporary NullLit to the borrow checker.
    let mut tmp = Expr::NullLit;
    std::mem::swap(&mut tmp, expr);
    *expr = Expr::CheckType {
        span,
        type_: expected,
        body: Box::new(tmp),
    };
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
    ///
    /// Returns the inferred type of the expression, which is a subtype of the
    /// expected type.
    pub fn check_expr(
        &mut self,
        env: &mut Env,
        expected: &Type,
        expr_span: Span,
        expr: &mut Expr,
    ) -> Result<Type> {
        match expr {
            Expr::Stmt {
                stmt,
                body_span,
                body,
            } => {
                let ck = env.checkpoint();
                self.check_stmt(env, stmt)?;
                let t = self.check_expr(env, expected, *body_span, body)?;
                env.pop(ck);
                Ok(t)
            }

            Expr::Import { path_span, path: _ } => {
                // TODO: Confirm that the path is a string literal,
                // we can do that here!

                // The type of an import is always `Dynamic`. Though if that is
                // the case, that removes one justification for having it be a
                // keyword instead of a builtin method `std.import`. Because we
                // can just type it: `std.import: (fname: String) -> Dynamic`.
                // If the expected type is also dynamic, that is fine; if the
                // expected type is different, then we need to insert a runtime
                // type check here.
                if expected == &Type::Dynamic {
                    Ok(Type::Dynamic)
                } else {
                    // TODO: The path span is the wrong span to blame the
                    // type error on in case of a runtime type error; we
                    // should get the obligation site (the annotation on the
                    // let) from the caller.
                    let blame_span = *path_span;
                    wrap_in_check_type(expr, blame_span, expected.clone());
                    Ok(expected.clone())
                }
            }

            Expr::BraceLit(_seq) => {
                unfinished!("TODO: Typecheck brace literal.")
            }

            Expr::BracketLit(seqs) => {
                let element_type = match expected {
                    // If we don't have any requirements and we got a list,
                    // then we don't have any requirements on the element type.
                    Type::Dynamic => &Type::Dynamic,
                    // If we expected a list and got one, so far so good, but
                    // now the elements have to match as well.
                    Type::List(t) => t,
                    _ => return type_error(expr_span, expected, "List"),
                };
                let mut et = Type::Void;
                for seq in seqs {
                    let t = self.check_seq_scalar(env, element_type, seq)?;
                    et = et.meet(&t);
                    // A reasonable expectation is that because each of the
                    // elements have an inferred type that is a subtype of the
                    // expected element type, then their meet is also a subtype.
                    // But that is only true if we return the _least_ upper
                    // bound, and that is tricky to get right, so verify that here.
                    debug_assert!(
                        self.check_subtype(expr_span, element_type, &et).is_ok(),
                        "Meet of all elements should be a subtype of the expected element type.",
                    );
                }
                Ok(Type::List(Rc::new(et)))
            }

            Expr::NullLit => match expected {
                Type::Dynamic | Type::Null => Ok(Type::Null),
                _ => type_error(expr_span, expected, "Null"),
            },

            Expr::BoolLit(..) => match expected {
                Type::Dynamic | Type::Bool => Ok(Type::Bool),
                _ => type_error(expr_span, expected, "Bool"),
            },

            Expr::StringLit(..) => match expected {
                Type::Dynamic | Type::String => Ok(Type::String),
                _ => type_error(expr_span, expected, "String"),
            },

            Expr::IntegerLit(..) => match expected {
                Type::Dynamic | Type::Int => Ok(Type::Int),
                _ => type_error(expr_span, expected, "Int"),
            },

            // Format strings evaluate to string values, so they fit string types.
            Expr::Format(..) => match expected {
                Type::Dynamic | Type::String => Ok(Type::String),
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
                self.check_expr(env, &Type::Bool, *condition_span, condition)?;
                // TODO: Record the spans on then and else. For now I'll just
                // put in the condition span as a temporary hack because I don't
                // want to change everything all over the place.
                let tt = self.check_expr(env, expected, *condition_span, body_then)?;
                let te = self.check_expr(env, expected, *condition_span, body_else)?;
                let t = tt.meet(&te);
                debug_assert!(
                    self.check_subtype(expr_span, expected, &t).is_ok(),
                    "Meet of the branches should be a subtype of the expected type.",
                );
                Ok(t)
            }

            Expr::Var { span, ident } => match (env.lookup(ident), expected) {
                (None, _) => span.error("Unknown variable.").err(),

                // If we don't expect a type statically, then anything is fine.
                (Some(t), Type::Dynamic) => Ok(t.clone()),

                // If we do expect a type statically, but the type is not known
                // statically, then we have to insert a runtime type check.
                (Some(Type::Dynamic), _not_dynamic) => {
                    let blame_span = *span;
                    wrap_in_check_type(expr, blame_span, expected.clone());
                    Ok(expected.clone())
                }

                // If both types are known statically, we can confirm right now
                // that the type fits.
                (Some(t), _) => {
                    self.check_subtype(*span, expected, t)?;
                    Ok(t.clone())
                }
            },

            Expr::Field { .. } => unfinished!("TODO: Implement typechecking fields."),

            Expr::Function { .. } => unfinished!("TODO: Implement typechecking functions."),

            Expr::Call { .. } => unfinished!("TODO: Implement typechecking calls."),

            Expr::Index { .. } => unfinished!("TODO: Implement typechecking indexing."),

            Expr::UnOp { op, body_span, body, .. } => self.check_unop(env, expected, expr_span, *op, *body_span, body),

            Expr::BinOp { .. } => unfinished!("TODO: Implement typechecking BinOp."),

            Expr::CheckType { .. } => panic!(
                "CheckType is inserted by the typechecker, it should not be present before checking."
            ),
        }
    }

    fn check_unop(
        &mut self,
        env: &mut Env,
        expected: &Type,
        expr_span: Span,
        op: UnOp,
        body_span: Span,
        body: &mut Expr,
    ) -> Result<Type> {
        // For the operators, they determine the type, so we could immediately
        // return an error top-down. But as a user, bottom-up is more natural,
        // so we check the body first. For example, in
        //
        //     let x: Int = not 42;
        //
        // Already at the `not`, we know the result is Bool but we need Int, so
        // that's an error. But there *another* error, which is applying `not`
        // to an int, and if we report only one type error, that seems like it
        // should come first, as it comes first in the evaluation order too.
        match op {
            UnOp::Neg => {
                self.check_expr(env, &Type::Int, body_span, body)?;
                match expected {
                    Type::Dynamic | Type::Int => Ok(Type::Int),
                    _ => type_error(expr_span, expected, "Int"),
                }
            }
            UnOp::Not => {
                self.check_expr(env, &Type::Bool, body_span, body)?;
                match expected {
                    Type::Dynamic | Type::Bool => Ok(Type::Bool),
                    _ => type_error(expr_span, expected, "Bool"),
                }
            }
        }
    }

    fn check_seq_scalar(
        &mut self,
        _env: &mut Env,
        _expected: &Type,
        _seq: &mut Seq,
    ) -> Result<Type> {
        unfinished!("TODO: Typecheck seq.")
    }

    fn check_stmt(&mut self, env: &mut Env, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Let {
                ident_span,
                ident,
                type_,
                value,
            } => {
                let expected = match type_ {
                    None => Type::Dynamic,
                    Some(type_expr) => eval_type_expr(type_expr)?,
                };
                // TODO: Should we gather the span for the expression, and blame
                // the type error on the expression instead? Or is it nicer to
                // blame it on the binding? Hmm...
                // TODO 2: I think it is nicer to use the body than the ident.
                let inferred = self.check_expr(env, &expected, *ident_span, value)?;

                // The inferred type is at least as precise as the expected type,
                // as it is a subtype. But when a user specifies a type for a
                // variable, we should bind exactly that type, even if it means
                // losing information.
                let bound_type = match type_ {
                    None => inferred,
                    Some(_) => expected,
                };
                env.push(ident.clone(), bound_type);

                Ok(())
            }
            Stmt::Assert {
                condition_span,
                condition,
                message_span,
                message,
            } => {
                // The condition has to be a boolean, the message can be any value.
                self.check_expr(env, &Type::Bool, *condition_span, condition)?;
                self.check_expr(env, &Type::Dynamic, *message_span, message)?;
                Ok(())
            }
            Stmt::Trace {
                message_span,
                message,
            } => {
                self.check_expr(env, &Type::Dynamic, *message_span, message)?;
                Ok(())
            }
        }
    }

    /// Check that `actual` is a subtype of `expected`.
    ///
    /// When a type is known for a particular variable, but we then try to use
    /// that variable in a context where a particular type is expected, we have
    /// to verify that the known type fits the expected type. For example, a
    /// record that only has `Int` fields would fit the type `Dict[String, Int]`,
    /// but not the other way around.
    ///
    /// Type errors will be attributed to the span `at`.
    pub fn check_subtype(&mut self, at: Span, expected: &Type, actual: &Type) -> Result<()> {
        match (expected, actual) {
            // If we defer the typecheck to runtime, anything is allowed.
            (Type::Dynamic, _) => Ok(()),

            // Every type is a subtype of itself.
            _ if expected == actual => Ok(()),

            _ => {
                // TODO: Generate a briefer error when the expected type is a primitive type,
                // dedup between the `type_error` function.
                at.error("Type mismatch.")
                    .with_body(concat! {
                        "Expected this type:"
                        Doc::HardBreak Doc::HardBreak
                        indent! { format_type(expected).into_owned() }
                        Doc::HardBreak Doc::HardBreak
                        "But got this type:"
                        Doc::HardBreak Doc::HardBreak
                        indent! { format_type(actual).into_owned() }
                    })
                    .err()
            }
        }
    }
}
