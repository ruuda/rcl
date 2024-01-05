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

use crate::ast::{BinOp, Expr, Seq, Stmt, Type as AType, UnOp, Yield};
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
    if expected.is_atom() {
        // If the expected type is an atom, it is short, so we can format
        // everything in a message that fits on one line.
        at.error("Type mismatch.")
            .with_body(concat! {
                "Expected "
                format_type(expected).into_owned()
                " but found " Doc::from(actual).with_markup(Markup::Type) "."
            })
            .err()
    } else {
        // If one of the types is composite, then it may format as something big,
        // so then we put the types on their own lines, indented.
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

            Expr::BraceLit(seqs) => {
                let mut seq_type = match expected {
                    Type::Dynamic => SeqType::SetOrDict,
                    Type::Set(t) => SeqType::TypedSet((**t).clone()),
                    Type::Dict(kv) => SeqType::TypedDict(kv.key.clone(), kv.value.clone()),
                    // TODO: This error is misleading, we might find a dict or set,
                    // not only a dict. We need to typecheck the seqs first, then
                    // report the error later.
                    _ => return type_error(expr_span, expected, "Dict"),
                };
                for seq in seqs {
                    seq_type = self.check_seq(env, seq, seq_type)?;
                }
                Ok(seq_type.into_type())
            }

            Expr::BracketLit(seqs) => {
                let mut seq_type = match expected {
                    Type::Dynamic => SeqType::UntypedList(Type::Void),
                    Type::List(t) => SeqType::TypedList((**t).clone()),
                    _ => return type_error(expr_span, expected, "List"),
                };
                for seq in seqs {
                    seq_type = self.check_seq(env, seq, seq_type)?;
                }
                Ok(seq_type.into_type())
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

            Expr::Field { inner, inner_span, .. } => {
                self.check_expr(env, &Type::Dynamic, *inner_span, inner)?;
                // At this point, we defer all field lookups to runtime checks.
                // a few methods we could resolve statically already, but we need
                // record types to really make this useful.
                if expected != &Type::Dynamic {
                    wrap_in_check_type(expr, expr_span, expected.clone());
                }
                Ok(Type::Dynamic)
            }

            Expr::Function { .. } => unfinished!("TODO: Implement typechecking functions."),

            Expr::Call { function_span, function, args, .. } => {
                match self.check_expr(env, &Type::Dynamic, *function_span, function)? {
                    Type::Dynamic => {
                        for (arg_span, arg) in args {
                            self.check_expr(env, &Type::Dynamic, *arg_span, arg)?;
                        }
                        if expected != &Type::Dynamic {
                            wrap_in_check_type(expr, expr_span, expected.clone());
                        }
                        Ok(Type::Dynamic)
                    }
                    Type::Function(..) => {
                        unfinished!("TODO: Statically check function call.");
                    }
                    not_callable => {
                        // Even though we already know the call is a type error,
                        // still typecheck the arguments and report any errors
                        // there first, so errors match evaluation order.
                        for (arg_span, arg) in args {
                            self.check_expr(env, &Type::Dynamic, *arg_span, arg)?;
                        }
                        function_span
                            .error("This cannot be called.")
                            .with_body(concat!{
                                "Expected a function, but got:"
                                Doc::HardBreak Doc::HardBreak
                                indent! { format_type(&not_callable).into_owned() }
                            })
                            .err()
                    }
                }
            }

            Expr::Index { open, collection_span, collection, index_span, index, .. } => {
                let collection_type = self.check_expr(env, &Type::Dynamic, *collection_span, collection)?;
                match collection_type {
                    Type::Dynamic => {
                        self.check_expr(env, &Type::Dynamic, *index_span, index)?;
                        if expected != &Type::Dynamic {
                            wrap_in_check_type(expr, expr_span, expected.clone());
                        }
                        Ok(expected.clone())
                    }
                    Type::List(element_type) => {
                        self.check_expr(env, &Type::Int, *index_span, index)?;
                        self.check_subtype(expr_span, expected, element_type.as_ref())?;
                        Ok((*element_type).clone())
                    }
                    Type::Dict(dict) => {
                        self.check_expr(env, &dict.key, *index_span, index)?;
                        self.check_subtype(expr_span, expected, &dict.value)?;
                        Ok(dict.value.clone())
                    }
                    not_indexable => {
                        open
                            .error("Indexing is not supported here.")
                            .with_body(concat!{
                                "Expected a dict or list, but got:"
                                Doc::HardBreak Doc::HardBreak
                                indent! { format_type(&not_indexable).into_owned() }
                            })
                            .err()
                    }
                }
            }

            Expr::UnOp { op, body_span, body, .. } => self.check_unop(
                env, expected, expr_span, *op, *body_span, body
            ),

            Expr::BinOp { op, lhs_span, lhs, rhs_span, rhs, .. } => self.check_binop(
                env, expected, expr_span, *op, *lhs_span, lhs, *rhs_span, rhs
            ),

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

    fn check_binop(
        &mut self,
        env: &mut Env,
        expected: &Type,
        expr_span: Span,
        op: BinOp,
        lhs_span: Span,
        lhs: &mut Expr,
        rhs_span: Span,
        rhs: &mut Expr,
    ) -> Result<Type> {
        // As with unop, we typecheck the sides even when we already know that
        // the result cannot be valid, to get more natural bottom-up errors in
        // case the bodies contain errors.
        let (sides_expected, result_type, result_name) = match op {
            BinOp::Add | BinOp::Mul | BinOp::Div | BinOp::Sub => (Type::Int, Type::Int, "Int"),
            BinOp::And | BinOp::Or => (Type::Bool, Type::Bool, "Bool"),
            BinOp::Union => panic!("TODO: Deal with overloaded operators."),
            BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => (Type::Int, Type::Bool, "Bool"),
            BinOp::Eq | BinOp::Neq => (Type::Dynamic, Type::Bool, "Bool"),
        };
        self.check_expr(env, &sides_expected, lhs_span, lhs)?;
        self.check_expr(env, &sides_expected, rhs_span, rhs)?;
        match expected {
            Type::Dynamic => Ok(result_type),
            t if t == &result_type => Ok(result_type),
            _ => type_error(expr_span, expected, result_name),
        }
    }

    fn check_seq(&mut self, env: &mut Env, seq: &mut Seq, seq_type: SeqType) -> Result<SeqType> {
        match seq {
            Seq::Yield(yield_) => self.check_yield(env, yield_, seq_type),
            Seq::Stmt { stmt, body } => {
                let ck = env.checkpoint();
                self.check_stmt(env, stmt)?;
                let t = self.check_seq(env, body, seq_type)?;
                env.pop(ck);
                Ok(t)
            }
            Seq::For {
                idents_span,
                idents,
                collection_span,
                collection,
                body,
                ..
            } => {
                let collection_type =
                    self.check_expr(env, &Type::Dynamic, *collection_span, collection)?;
                let ck = env.checkpoint();

                match collection_type {
                    // If we don't know the type, we can't verify the number of
                    // loop variables, and we don't know their types.
                    Type::Dynamic => {
                        for ident in idents {
                            env.push(ident.clone(), Type::Dynamic);
                        }
                    }
                    Type::Dict(dict) => {
                        if idents.len() != 2 {
                            // TODO: Deduplicate runtime error.
                            return idents_span
                                .error("Expected two variables in dict iteration.")
                                .with_note(
                                    *collection_span,
                                    "This is a dict, it yields a key and value per iteration.",
                                )
                                .err();
                        }
                        env.push(idents[0].clone(), dict.key.clone());
                        env.push(idents[1].clone(), dict.value.clone());
                    }
                    Type::List(element_type) => {
                        if idents.len() != 1 {
                            return idents_span
                                .error("Expected a single variable.")
                                .with_note(
                                    *collection_span,
                                    "This is a list, it yields one element per iteration.",
                                )
                                .err();
                        }
                        env.push(idents[0].clone(), (*element_type).clone());
                    }
                    Type::Set(element_type) => {
                        if idents.len() != 1 {
                            return idents_span
                                .error("Expected a single variable.")
                                .with_note(
                                    *collection_span,
                                    "This is a set, it yields one element per iteration.",
                                )
                                .err();
                        }
                        env.push(idents[0].clone(), (*element_type).clone());
                    }
                    not_collection => {
                        return collection_span
                            .error("This is not iterable.")
                            .with_body(concat! {
                                "Expected a collection, but got:"
                                Doc::HardBreak Doc::HardBreak
                                indent! { format_type(&not_collection).into_owned() }
                            })
                            .err()
                    }
                }

                let t = self.check_seq(env, body, seq_type)?;
                env.pop(ck);
                Ok(t)
            }
            Seq::If {
                condition_span,
                condition,
                body,
            } => {
                self.check_expr(env, &Type::Bool, *condition_span, condition)?;
                self.check_seq(env, body, seq_type)
            }
        }
    }

    /// Visit a yield inside a sequence literal.
    fn check_yield(
        &mut self,
        env: &mut Env,
        yield_: &mut Yield,
        mut seq_type: SeqType,
    ) -> Result<SeqType> {
        match yield_ {
            Yield::Elem { span, value } => match &mut seq_type {
                SeqType::SetOrDict => {
                    let t = self.check_expr(env, &Type::Dynamic, *span, value)?;
                    Ok(SeqType::UntypedSet(*span, t))
                }
                SeqType::TypedList(expected) | SeqType::TypedSet(expected) => {
                    self.check_expr(env, expected, *span, value)?;
                    Ok(seq_type)
                }
                SeqType::TypedDict(..) => {
                    // TODO: We could make a nicer error here, but for now this will do.
                    // See also the calls to `type_error` in the assoc case below.
                    type_error(*span, &seq_type.into_type(), "Dict")
                }
                SeqType::UntypedList(et) | SeqType::UntypedSet(.., et) => {
                    let t = self.check_expr(env, &Type::Dynamic, *span, value)?;
                    *et = et.meet(&t);
                    Ok(seq_type)
                }
                SeqType::UntypedDict(first, _k, _v) => {
                    span
                        .error("Expected key-value, not a scalar element.")
                        .with_note(
                            *first,
                            "The collection is a dict and not a set, because it starts with a key-value.",
                        )
                        .err()
                }
            }
            Yield::Assoc { op_span, key, value } => match &mut seq_type {
                // TODO: We need the key and value spans. For now I'm using op_span.
                SeqType::SetOrDict => {
                    let k = self.check_expr(env, &Type::Dynamic, *op_span, key)?;
                    let v = self.check_expr(env, &Type::Dynamic, *op_span, value)?;
                    Ok(SeqType::UntypedDict(*op_span, k, v))
                }
                SeqType::TypedList(..) => {
                    // TODO: We could make a nicer error here, but for now this will do.
                    type_error(*op_span, &seq_type.into_type(), "List")
                }
                SeqType::TypedSet(..) => {
                    // TODO: We could make a nicer error here, but for now this will do.
                    type_error(*op_span, &seq_type.into_type(), "Set")
                }
                SeqType::TypedDict(key_type, value_type) => {
                    // TODO: Again, spans.
                    self.check_expr(env, key_type, *op_span, key)?;
                    self.check_expr(env, value_type, *op_span, value)?;
                    Ok(seq_type)
                }
                SeqType::UntypedList(..) => op_span
                    .error("Expected scalar element, not key-value.")
                    .with_help(
                        "Key-value pairs are allowed in dicts, which are enclosed in '{}', not '[]'.",
                    ).err(),
                SeqType::UntypedSet(first, _elem) => op_span
                    .error("Expected scalar element, not key-value.")
                    .with_note(
                        *first,
                        "The collection is a set and not a dict, because it starts with a scalar value.",
                    )
                    .err(),
                SeqType::UntypedDict(_first, key_type, value_type) => {
                    // TODO: Spans again.
                    let k = self.check_expr(env, &Type::Dynamic, *op_span, key)?;
                    let v = self.check_expr(env, &Type::Dynamic, *op_span, value)?;
                    *key_type = key_type.meet(&k);
                    *value_type = value_type.meet(&v);
                    Ok(seq_type)
                }
            }
        }
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

            // TODO: Check inside collections, functions.
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

/// Helper to typecheck collection literals.
///
/// There are a few things that determine what types of yield are allowed, and
/// how to typecheck.
///
/// * Whether the collection is enclosed in `[]` (it's a list) or `{}` (a dict
///   or set).
/// * Whether we have an expected type due to a type annotation.
/// * Whether any previous yields where scalars or key-values.
///
/// This enum tracks what we know about the collection so far. We either track
/// an expected element type, or an inferred one. `TypeChecker::check_yield`
/// takes this and a `Yield` and incorporates the information.
///
/// TODO: The equivalent SeqOut may be removed from the evaluator. Maybe we can
/// substitute more specialized AST nodes for the different collections.
enum SeqType {
    /// It's still unclear whether this is a set or a dict.
    SetOrDict,

    /// Type annotations demand a list here with the following element type.
    TypedList(Type),

    /// Type annotations demand a set here with the following element type.
    TypedSet(Type),

    /// Type annotations demand a dict here with the following key-value types.
    TypedDict(Type, Type),

    /// We found a list, and the meet of the elements is as follows.
    UntypedList(Type),

    /// We found a set, as evidenced by the span of the first scalar.
    UntypedSet(Span, Type),

    /// We found a dict, as evidenced by the span of the first key-value.
    UntypedDict(Span, Type, Type),
}

impl SeqType {
    /// Return the inferred type for this sequence.
    fn into_type(self) -> Type {
        match self {
            // An empty literal `{}` is a dict, not a set, because it is a dict in json.
            SeqType::SetOrDict => Type::Dict(Rc::new(types::Dict {
                key: Type::Void,
                value: Type::Void,
            })),
            SeqType::UntypedList(t) | SeqType::TypedList(t) => Type::List(Rc::new(t)),
            SeqType::UntypedSet(.., t) | SeqType::TypedSet(t) => Type::Set(Rc::new(t)),
            SeqType::UntypedDict(.., k, v) | SeqType::TypedDict(k, v) => {
                Type::Dict(Rc::new(types::Dict { key: k, value: v }))
            }
        }
    }
}
