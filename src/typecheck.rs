// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! A checker for static annotations and runtime dynamic types.
//!
//! The same value in RCL can be described by multiple types, values do not have
//! unique types. For example, `[]` is a valid value for the type `List[Number]`
//! but also for the type `List[String]`. Therefore we check whether a value
//! _fits_ a particular type, and that same value may fit multiple types.

use std::rc::Rc;

use crate::ast::{BinOp, Expr, Ident, Seq, Stmt, Type as AType, UnOp, Yield};
use crate::error::{Error, IntoError, Result};
use crate::fmt_type::format_type;
use crate::markup::Markup;
use crate::pprint::{concat, indent, Doc};
use crate::source::Span;
use crate::type_diff::{report_type_mismatch, Typed};
use crate::type_source::Source;
use crate::types::{Dict, ElementType, Function, FunctionArg, Side, SourcedType, Type, Union};

pub type Env = crate::env::Env<SourcedType>;

/// Return the default environment with prelude in scope.
pub fn prelude() -> Env {
    let mut env = Env::new();
    // TODO: Type std correctly once we have record types.
    env.push("std".into(), SourcedType::any());
    env
}

/// Convert a type name into the corresponding primitive type.
fn get_primitive_type(name: &str) -> Option<Type> {
    match name {
        "Any" => Some(Type::Any),
        "Bool" => Some(Type::Bool),
        "Number" => Some(Type::Number),
        "Null" => Some(Type::Null),
        "String" => Some(Type::String),
        "Void" => Some(Type::Void),
        _ => None,
    }
}

/// Report an error for an unknown type, with a hint if the name looks familiar.
fn report_unknown_type<T>(span: Span, name: &str, error_msg: &'static str) -> Result<T> {
    match name {
        // Detect a few cases that users may try to use, so we can point
        // them in the right direction.
        "Int" | "Integer" | "Float" | "Num" | "int" | "float" | "number" => span
            .error(error_msg)
            .with_help(concat! { "The number type is called '" Doc::highlight("Number") "'." })
            .err(),
        "Dictionary" | "Map" | "Object" | "dict" => span
            .error(error_msg)
            .with_help(concat! { "The dictionary type is called '" Doc::highlight("Dict") "'." })
            .err(),
        "Array" | "array" | "list" => span
            .error(error_msg)
            .with_help(concat! { "The list type is called '" Doc::highlight("List") "'." })
            .err(),
        "Boolean" | "boolean" | "bool" => span
            .error(error_msg)
            .with_help(concat! { "The boolean type is called '" Doc::highlight("Bool") "'." })
            .err(),
        _ => span.error(error_msg).err(),
    }
}

/// Parse a type expression.
fn eval_type_expr(expr: &AType) -> Result<SourcedType> {
    match expr {
        AType::Term { span, name } => {
            if let Some(prim) = get_primitive_type(name.as_ref()) {
                let styp = SourcedType {
                    type_: prim,
                    source: Source::Annotation(*span),
                };
                return Ok(styp);
            }
            match name.as_ref() {
                "Dict" => {
                    span
                        .error("Expected a concrete type, but found uninstantiated generic type.")
                        .with_help(concat! {
                            "'" Doc::highlight("Dict") "' without type parameters cannot be used directly."
                            Doc::SoftBreak
                            "Specify a key and value type, e.g. '" Doc::highlight("Dict[String, Number]") "'."
                        })
                        .err()
                },
                "List" => {
                    span
                        .error("Expected a concrete type, but found uninstantiated generic type.")
                        .with_help(concat! {
                            "'" Doc::highlight("List") "' without type parameters cannot be used directly."
                            Doc::SoftBreak
                            "Specify an element type, e.g. '" Doc::highlight("List[String]") "'."
                        })
                        .err()
                },
                "Set" => {
                    span
                        .error("Expected a concrete type, but found uninstantiated generic type.")
                        .with_help(concat! {
                            "'" Doc::highlight("Set") "' without type parameters cannot be used directly."
                            Doc::SoftBreak
                            "Specify an element type, e.g. '" Doc::highlight("Set[String]") "'."
                        })
                        .err()
                },
                "Union" => {
                    span
                        .error("Expected a concrete type, but found uninstantiated union type.")
                        .with_help(concat! {
                            "'" Doc::highlight("Union") "' without type parameters cannot be used directly."
                            Doc::SoftBreak
                            "Specify types to union, e.g. '" Doc::highlight("Union[Number, Null]") "'."
                        })
                        .err()
                }
                unknown => report_unknown_type(*span, unknown, "Unknown type.")?,
            }
        }
        AType::Function { span, args, result } => {
            let args_types = args
                .iter()
                .map(|type_expr| {
                    Ok(FunctionArg {
                        // For user-defined function types, right now we don't
                        // allow argument names. If we do allow them at some
                        // point, this is where we would parse them.
                        name: None,
                        span: None,
                        type_: eval_type_expr(type_expr)?,
                    })
                })
                .collect::<Result<Vec<FunctionArg>>>()?;
            let result_type = eval_type_expr(result)?;
            let fn_type = Rc::new(Function {
                args: args_types,
                result: result_type,
            });
            let styp = SourcedType {
                type_: Type::Function(fn_type),
                source: Source::Annotation(*span),
            };
            Ok(styp)
        }
        AType::Apply { span, name, args } => {
            let args_types = args
                .iter()
                .map(eval_type_expr)
                .collect::<Result<Vec<_>>>()?;
            let styp = SourcedType {
                type_: eval_type_apply(*span, name.as_ref(), &args_types)?,
                source: Source::Annotation(*span),
            };
            Ok(styp)
        }
    }
}

/// Evaluate type constructor application (generic instantiation).
fn eval_type_apply(name_span: Span, name: &str, args: &[SourcedType]) -> Result<Type> {
    match name {
        "Dict" => match args {
            [tk, tv] => {
                let dict = Dict {
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
        "Union" => {
            let members = match args.len() {
                0 => {
                    return name_span
                        .error("A union type cannot be empty.")
                        .with_help("Use 'Void' for a type with no values.")
                        .err()
                }
                1 => {
                    return name_span
                        .error("A union type must have more than one member.")
                        .with_help(
                            "A union of a single type is equivalent to just that type itself.",
                        )
                        .err()
                }
                _ => args.to_vec(),
            };
            let union = Union { members };
            Ok(Type::Union(Rc::new(union)))
        }
        unknown => report_unknown_type(name_span, unknown, "Unknown generic type.")?,
    }
}

/// Shorthand for writing [`SourcedType::any`].
pub fn type_any() -> &'static SourcedType {
    &SourcedType {
        type_: Type::Any,
        source: Source::None,
    }
}

/// Construct a `SourcedType` for a literal.
fn type_literal(at: Span, type_: Type) -> SourcedType {
    SourcedType {
        type_,
        source: Source::Literal(at),
    }
}

/// Construct a `SourcedType` for a `Bool` for a condition.
fn type_bool_condition() -> &'static SourcedType {
    &SourcedType {
        type_: Type::Bool,
        source: Source::Condition,
    }
}

/// Construct a `SourcedType` for a `Number` for list indexing.
fn type_number_index() -> &'static SourcedType {
    &SourcedType {
        type_: Type::Number,
        source: Source::IndexList,
    }
}

/// Construct a `SourcedType` for an operator.
fn type_operator(at: Span, type_: Type) -> SourcedType {
    SourcedType {
        type_,
        source: Source::Operator(at),
    }
}

pub struct TypeChecker<'a> {
    // TODO: Do I really need to borrow it?
    // Could also move it into and out of the checker.
    env: &'a mut Env,
}

impl<'a> TypeChecker<'a> {
    pub fn new(env: &'a mut Env) -> TypeChecker<'a> {
        TypeChecker { env }
    }

    /// Check that an expression fits the type requirements.
    ///
    /// This also updates the AST to insert runtime type checks where necessary.
    /// Returns the inferred type of the expression, which is a subtype of the
    /// required type.
    pub fn check_expr(
        &mut self,
        expected: &SourcedType,
        expr_span: Span,
        expr: &mut Expr,
    ) -> Result<SourcedType> {
        let expr_type = match expr {
            Expr::Stmt {
                stmt,
                body_span,
                body,
            } => {
                let ck = self.env.checkpoint();
                self.check_stmt(stmt)?;
                let t = self.check_expr(expected, *body_span, body)?;
                self.env.pop(ck);
                Typed::Type(t)
            }

            Expr::Import { .. } => {
                // TODO: Confirm that the path is a string literal,
                // we can do that here!

                // The type of an import is always `Any`. Though if that is
                // the case, that removes one justification for having it be a
                // keyword instead of a builtin method `std.import`. Because we
                // can just type it: `std.import: (fname: String) -> Any`.
                type_any().is_subtype_of(expected).check(expr_span)?
            }

            Expr::BraceLit { open, elements: seqs } => {
                let mut is_error = false;
                // If we have a requirement on the element type, extract it.
                let mut seq_type = match &expected.type_ {
                    Type::Set(t) => SeqType::TypedSet {
                        set_source: expected.clone(),
                        elem_super: t.as_ref().clone(),
                        elem_infer: SourcedType::void(expr_span),
                    },
                    Type::Dict(kv) => {
                        SeqType::TypedDict {
                            dict_source: expected.clone(),
                            key_super: kv.key.clone(),
                            key_infer: SourcedType::void(expr_span),
                            value_super: kv.value.clone(),
                            value_infer: SourcedType::void(expr_span),
                        }
                    }
                    // If we are expecting something other than a dict or list,
                    // then this is definitely a type error. But to be able to
                    // report it in full detail, we first infer the type of the
                    // sequence.
                    not_collection => {
                        is_error = not_collection != &Type::Any;
                        SeqType::SetOrDict
                    }
                };

                // Typecheck all the elements, and enforce the element
                // requirement if we have one. This at the same time infers the
                // element type.
                for seq in seqs.iter_mut() {
                    seq_type = self.check_seq(seq, seq_type)?;
                }

                let seq_type = seq_type.into_type(expr_span);

                // Replace the BraceLit node where we don't know if it's a dict
                // or set with a node where we do know the type. This simplifies
                // the evaluator.
                let mut seqs_moved = Vec::new();
                std::mem::swap(seqs, &mut seqs_moved);
                match seq_type.type_ {
                    Type::Dict(..) => *expr = Expr::DictLit { open: *open, elements: seqs_moved },
                    Type::Set(..) => *expr = Expr::SetLit { open: *open, elements: seqs_moved },
                    _ => unreachable!("A `BraceLit` cannot produce a list `SeqType`."),
                }

                if is_error {
                    seq_type.is_subtype_of(expected).check(expr_span)?
                } else {
                    Typed::Type(seq_type)
                }
            }

            Expr::BracketLit { elements: seqs, .. } => {
                // This follows the same structure as `BraceLit`, see comments above.
                let mut is_error = false;
                let mut seq_type = match &expected.type_ {
                    Type::List(t) => SeqType::TypedList {
                        elem_super: t.as_ref().clone(),
                        elem_infer: SourcedType::void(expr_span),
                    },
                    not_list => {
                        is_error = not_list != &Type::Any;
                        SeqType::UntypedList(SourcedType::void(expr_span))
                    }
                };
                for seq in seqs {
                    seq_type = self.check_seq(seq, seq_type)?;
                }

                let seq_type = seq_type.into_type(expr_span);

                if is_error {
                    seq_type.is_subtype_of(expected).check(expr_span)?
                } else {
                    Typed::Type(seq_type)
                }
            }

            Expr::NullLit => type_literal(expr_span, Type::Null).is_subtype_of(expected).check(expr_span)?,
            Expr::BoolLit(..) => type_literal(expr_span, Type::Bool).is_subtype_of(expected).check(expr_span)?,
            Expr::NumberLit(..) => type_literal(expr_span, Type::Number).is_subtype_of(expected).check(expr_span)?,
            Expr::StringLit(..) => type_literal(expr_span, Type::String).is_subtype_of(expected).check(expr_span)?,

            Expr::Format(fragments) => {
                // Typecheck the fragments. For now we don't demand statically
                // that they can be formatted, but we do descend into them to
                // catch other type errors. TODO: check formatability statically.
                for fragment in fragments {
                    self.check_expr(type_any(), fragment.span, &mut fragment.body)?;
                }
                // Format strings evaluate to string values.
                type_literal(expr_span, Type::String).is_subtype_of(expected).check(expr_span)?
            },

            Expr::IfThenElse {
                condition_span,
                condition,
                body_then,
                body_else,
                span_then,
                span_else,
                ..
            } => {
                self.check_expr(type_bool_condition(), *condition_span, condition)?;

                let type_then = self.check_expr(expected, *span_then, body_then)?;
                let type_else = self.check_expr(expected, *span_else, body_else)?;

                // The inferred type is the meet of the two sides, which may be
                // more specific than the requirement (which they satisfy).
                Typed::Type(type_then.meet(&type_else))
            }

            Expr::Var { span, ident } => match self.env.lookup(ident) {
                None => return span.error("Unknown variable.").err(),
                Some(t) => t.is_subtype_of(expected).check(*span)?,
            },

            Expr::Field { inner, inner_span, .. } => {
                self.check_expr(type_any(), *inner_span, inner)?;
                // At this point, we defer all field lookups to runtime checks.
                // a few methods we could resolve statically already, but we need
                // record types to really make this useful.
                type_any().is_subtype_of(expected).check(expr_span)?
            }

            Expr::Function { args, body_span, body } => {
                let fn_type = self.check_function(expected, expr_span, args, *body_span, body)?;

                // Now that we know the type of the function, preserve it in the
                // AST, because we need it in the runtime value. We need to
                // juggle some temporaries to move values out of the old node
                // into the new one.
                let mut body_tmp = Box::new(Expr::NullLit);
                std::mem::swap(&mut body_tmp, body);
                *expr = Expr::TypedFunction {
                    span: expr_span,
                    body_span: *body_span,
                    body: body_tmp,
                    type_: fn_type.clone(),
                };

                Typed::Type(type_literal(expr_span, Type::Function(fn_type)))
            }

            Expr::Call { function_span, function, args, close, .. } => {
                // The direction of the requirements for a call could go two ways.
                // Take for example: `(x => x + 1)("42")`. We could say, it's a
                // call with String as first argument, so we push that into the
                // function body, and there is a type error at the `+` because
                // we expect a String but `+` creates a Number. But we could also
                // say, we typecheck the function first, infer `Any -> Number`
                // (with a runtime check inserted at left-hand side of `+`), then
                // we call that with "42", which passes, but the runtime check
                // fails. We go with the latter: we assume function definitions
                // are always correct, and the error is at the call site.
                let fn_type = self.check_expr(type_any(), *function_span, function)?;

                let result_type = match &fn_type.type_ {
                    Type::Function(f) => {
                        let function_name = None;
                        f.check_arity(function_name, args, *close)?;

                        // If we know the function type, then we can typecheck
                        // all the arguments precisely.
                        for (call_arg, fn_arg) in args.iter_mut().zip(f.args.iter()) {
                            self.check_expr(&fn_arg.type_, call_arg.span, &mut call_arg.value)?;
                        }

                        &f.result
                    },
                    Type::Any => {
                        // If we don't know the function type, then we don't have
                        // any expectations on the arguments, but we still need
                        // to typecheck them.
                        for call_arg in args {
                            self.check_expr(type_any(), call_arg.span, &mut call_arg.value)?;
                        }

                        type_any()
                    },
                    _not_function => {
                        return function_span
                            .error("This cannot be called.")
                            .with_body(report_type_mismatch(&"function", &fn_type))
                            .err()
                    },
                };

                result_type.is_subtype_of(expected).check(expr_span)?
            }

            Expr::Index { open, collection_span, collection, index_span, index, .. } => {
                let collection_type = self.check_expr(type_any(), *collection_span, collection)?;
                let (index_type, result_type) = match &collection_type.type_ {
                    Type::List(t) => (type_number_index(), (**t).clone()),
                    Type::Dict(kv) => (&kv.key, kv.value.clone()),
                    Type::Any => (type_any(), type_any().clone()),
                    Type::String => {
                        return open
                            .error("Indexing into a string is not yet supported.")
                            .with_note(*collection_span, "This is a string.")
                            .err()
                    }
                    not_indexable => {
                        let mut error = open
                            .error("Indexing is not supported here.")
                            .with_body(concat!{
                                "Expected a dict or list, but got:"
                                Doc::HardBreak Doc::HardBreak
                                indent! { format_type(not_indexable).into_owned() }
                            });
                        collection_type.explain_error(Side::Actual, &mut error);
                        return error.err();
                    }
                };
                self.check_expr(index_type, *index_span, index)?;
                result_type.is_subtype_of(expected).check(expr_span)?
            }

            Expr::UnOp { op_span, op, body_span, body, .. } => {
                self.check_unop(*op_span, *op, *body_span, body)?
                    .is_subtype_of(expected)
                    .check(expr_span)?
            },

            Expr::BinOp { op_span, op, lhs_span, lhs, rhs_span, rhs, .. } => {
                self.check_binop(*op_span, *op, *lhs_span, *rhs_span, lhs, rhs)?
                    .is_subtype_of(expected)
                    .check(expr_span)?
            }

            // coverage:off -- Arm should be unreachable.
            Expr::CheckType { .. } | Expr::TypedFunction { .. } | Expr::SetLit { .. } | Expr::DictLit { .. } => unreachable!(
                "Node {expr:?} is inserted by the typechecker, it should not be present before checking."
            ),
            // coverage:on
        };
        match expr_type {
            // If the type check passed, great, we now know the inferred type.
            Typed::Type(t) => Ok(t),

            // If we couldn't check statically, then we have to insert a runtime
            // type check around this node. We have to sacrifice a temporary
            // NullLit to the borrow checker to swap the node into place.
            Typed::Defer(t) => {
                let mut tmp = Expr::NullLit;
                std::mem::swap(&mut tmp, expr);
                *expr = Expr::CheckType {
                    span: expr_span,
                    type_: expected.clone(),
                    body: Box::new(tmp),
                };
                Ok(t)
            }
        }
    }

    /// Typecheck a function definition.
    fn check_function(
        &mut self,
        expected: &SourcedType,
        expr_span: Span,
        args: &[(Span, Ident)],
        body_span: Span,
        body: &mut Expr,
    ) -> Result<Rc<Function>> {
        let mut arg_types = Vec::with_capacity(args.len());

        let checkpoint = self.env.checkpoint();
        let mut is_error = false;

        let body_req = match &expected.type_ {
            // If the arities mismatch, that's an error, and we handle that
            // in the same arm as a non-function below. We typecheck the body
            // either way, but we only put the types from the requirement in the
            // environment if there is a match, because otherwise the body would
            // likely contain nonsense errors anyway.
            Type::Function(fn_req) if fn_req.args.len() == args.len() => {
                for ((arg_span, arg_name), arg_type) in args.iter().zip(fn_req.args.iter()) {
                    let fn_arg = FunctionArg {
                        // If the type includes an argument name, discard it,
                        // and take the name from the function definition instead.
                        name: Some(arg_name.clone()),
                        span: Some(*arg_span),
                        type_: arg_type.type_.clone(),
                    };
                    arg_types.push(fn_arg);
                    self.env.push(arg_name.clone(), arg_type.type_.clone());
                }
                &fn_req.result
            }
            not_fn => {
                // If there is no type requirement at all on this function, then
                // all the args have an unknown type and we have no requirement
                // on the result. If there is a requirement but not for a
                // function, then this is a type error, but we'll still
                // typecheck the function first and report the error later.
                is_error = not_fn != &Type::Any;
                for (arg_span, arg_name) in args.iter() {
                    let fn_arg = FunctionArg {
                        name: Some(arg_name.clone()),
                        span: Some(*arg_span),
                        type_: type_any().clone(),
                    };
                    arg_types.push(fn_arg);
                    self.env.push(arg_name.clone(), type_any().clone());
                }
                type_any()
            }
        };

        let result_type = self.check_expr(body_req, body_span, body)?;
        self.env.pop(checkpoint);

        let fn_type_inner = Rc::new(Function {
            args: arg_types,
            result: result_type,
        });

        if is_error {
            // This check will fail, this is just an easy way to construct the
            // right error.
            let fn_type = SourcedType {
                type_: Type::Function(fn_type_inner.clone()),
                source: Source::Literal(expr_span),
            };
            fn_type.is_subtype_of(expected).check(expr_span)?;
        };

        Ok(fn_type_inner)
    }

    fn check_unop(
        &mut self,
        op_span: Span,
        op: UnOp,
        body_span: Span,
        body: &mut Expr,
    ) -> Result<SourcedType> {
        // For the operators, they determine the type, so we could immediately
        // return an error top-down. But as a user, bottom-up is more natural,
        // so we check the body first. For example, in
        //
        //     let x: Number = not 42;
        //
        // Already at the `not`, we know the result is Bool, but we need Number,
        // so that's an error. But there's *another* error, which is applying `not`
        // to a Number, and if we report only one type error, that seems like it
        // should come first, as it comes first in the evaluation order too.
        let (body_type, result_type) = match op {
            UnOp::Neg => (Type::Number, Type::Number),
            UnOp::Not => (Type::Bool, Type::Bool),
        };
        self.check_expr(&type_operator(op_span, body_type), body_span, body)?;
        Ok(type_operator(op_span, result_type))
    }

    fn check_binop(
        &mut self,
        op_span: Span,
        op: BinOp,
        lhs_span: Span,
        rhs_span: Span,
        lhs: &mut Expr,
        rhs: &mut Expr,
    ) -> Result<SourcedType> {
        let (arg_type, result_type) = match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => (Type::Number, Type::Number),
            BinOp::And | BinOp::Or => (Type::Bool, Type::Bool),
            // Comparison operators make sense on many types (Number, String), even
            // composite types (e.g. List[Number] would have lexicographic order).
            // On some types like dict types, it is more questionable whether
            // that makes sense, but currently we have no good machinery to
            // define what should and should not be allowed, and all values _do_
            // have an order, so we allow comparing any value for now.
            BinOp::Lt | BinOp::LtEq | BinOp::Gt | BinOp::GtEq => (Type::Any, Type::Bool),
            BinOp::Eq | BinOp::Neq => (Type::Any, Type::Bool),
            BinOp::Union => return self.check_binop_union(op_span, lhs_span, rhs_span, lhs, rhs),
        };
        let arg_type = type_operator(op_span, arg_type);
        self.check_expr(&arg_type, lhs_span, lhs)?;
        self.check_expr(&arg_type, rhs_span, rhs)?;

        Ok(type_operator(op_span, result_type))
    }

    fn check_binop_union(
        &mut self,
        op_span: Span,
        lhs_span: Span,
        rhs_span: Span,
        lhs: &mut Expr,
        rhs: &mut Expr,
    ) -> Result<SourcedType> {
        let lhs_type = self.check_expr(type_any(), lhs_span, lhs)?;
        let rhs_type = self.check_expr(type_any(), rhs_span, rhs)?;
        let result_type = match (&lhs_type.type_, &rhs_type.type_) {
            // TODO: There rules are a bit ad-hoc. Maybe don't allow | with
            // list? Or do allow, but allow it on the left-hand side too?
            (Type::Dict(..), Type::Dict(..)) => lhs_type.meet(&rhs_type),
            (Type::Set(..), Type::Set(..)) => lhs_type.meet(&rhs_type),
            (Type::Set(tl), Type::List(tr)) => SourcedType {
                type_: Type::Set(Rc::new(tl.meet(tr.as_ref()))),
                source: Source::None,
            },
            // TODO: Because of this case, we still have to handle the case at
            // runtime. But we would need a way to express as type requirement
            // "Set or Dict". That gets messy, I think I prefer to delete the
            // union operator and add interpolation instead.
            (Type::Any | Type::Dict(..) | Type::Set(..), _) => type_any().clone(),
            (not_collection, _) => {
                let mut error = op_span.error(concat! {
                    "Expected Dict or Set as the left-hand side of "
                    Doc::highlight("|")
                    " operator, but found this:"
                    Doc::HardBreak Doc::HardBreak
                    indent! { format_type(not_collection).into_owned() }
                });
                lhs_type.explain_error(Side::Actual, &mut error);
                return error.err();
            }
        };

        Ok(result_type)
    }

    fn error_not_iterable(&self, collection_span: Span, collection_type: SourcedType) -> Error {
        collection_span
            .error("This is not iterable.")
            .with_body(concat! {
                "Expected a collection, but got:"
                Doc::HardBreak Doc::HardBreak
                indent! { format_type(&collection_type.type_).into_owned() }
            })
    }

    fn check_seq(&mut self, seq: &mut Seq, seq_type: SeqType) -> Result<SeqType> {
        match seq {
            Seq::Yield(yield_) => self.check_yield(yield_, seq_type),
            Seq::Stmt { stmt, body } => {
                let ck = self.env.checkpoint();
                self.check_stmt(stmt)?;
                let t = self.check_seq(body, seq_type)?;
                self.env.pop(ck);
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
                let collection_type = self.check_expr(type_any(), *collection_span, collection)?;
                let ck = self.env.checkpoint();

                match collection_type.element_type() {
                    // If we don't know the type, we can't verify the number of
                    // loop variables, and we don't know their types.
                    ElementType::Any => {
                        for ident in idents {
                            self.env.push(ident.clone(), type_any().clone());
                        }
                    }
                    ElementType::Dict(dict) => {
                        if idents.len() != 2 {
                            // TODO: Deduplicate runtime error. Make it a method
                            // on the type? Same for functions?
                            return idents_span
                                .error("Expected two variables in dict iteration.")
                                .with_note(
                                    *collection_span,
                                    "This is a dict, it yields a key and value per iteration.",
                                )
                                .err();
                        }
                        self.env.push(idents[0].clone(), dict.key.clone());
                        self.env.push(idents[1].clone(), dict.value.clone());
                    }
                    ElementType::Scalar(element_type) => {
                        if idents.len() != 1 {
                            return idents_span
                                .error("Expected a single variable.")
                                .with_note(
                                    *collection_span,
                                    concat! {
                                        "This is a "
                                        collection_type.type_.short_name().to_ascii_lowercase()
                                        ", it yields one element per iteration."
                                    },
                                )
                                .err();
                        }
                        self.env.push(idents[0].clone(), (*element_type).clone());
                    }
                    _not_collection => {
                        return self
                            .error_not_iterable(*collection_span, collection_type)
                            .err()
                    }
                }

                let t = self.check_seq(body, seq_type)?;
                self.env.pop(ck);
                Ok(t)
            }
            Seq::If {
                condition_span,
                condition,
                body,
            } => {
                self.check_expr(type_bool_condition(), *condition_span, condition)?;
                self.check_seq(body, seq_type)
            }
        }
    }

    /// Visit a yield inside a sequence literal.
    fn check_yield(&mut self, yield_: &mut Yield, mut seq_type: SeqType) -> Result<SeqType> {
        match yield_ {
            Yield::Elem { span, value } => match &mut seq_type {
                SeqType::SetOrDict => {
                    let t = self.check_expr(type_any(), *span, value)?;
                    Ok(SeqType::UntypedSet(SeqSourceSet::Scalar(*span), t))
                }
                SeqType::TypedList { elem_super, elem_infer } | SeqType::TypedSet { elem_super, elem_infer, .. } => {
                    // First we check that the element satisfies the requirement.
                    // That gives us an inferred type that can be more precise.
                    // Meet it with what we have so far.
                    let elem_type = self.check_expr(elem_super, *span, value)?;
                    *elem_infer = elem_infer.meet(&elem_type);
                    Ok(seq_type)
                }
                SeqType::TypedDict { dict_source, .. } => {
                    let mut error = span.error(
                        "Expected key-value, not a single element, because the collection is a dict."
                    );
                    dict_source.explain_error(Side::Expected, &mut error);
                    error.err()
                }
                SeqType::UntypedList(elem_type_meet) | SeqType::UntypedSet(.., elem_type_meet) => {
                    let elem_type = self.check_expr(type_any(), *span, value)?;
                    *elem_type_meet = elem_type_meet.meet(&elem_type);
                    Ok(seq_type)
                }
                SeqType::UntypedDict(src, _k, _v) => {
                    src.add_note(span.error("Expected key-value, not a single element.")).err()
                }
            }
            Yield::Assoc { op_span, key_span, key, value_span, value } => match &mut seq_type {
                SeqType::SetOrDict => {
                    let k = match self.check_expr(type_any(), *key_span, key) {
                        Err(err) if matches!(key.as_ref(), Expr::Var { .. }) => {
                            err.with_note(*op_span, concat! {
                                "To use unquoted keys, replace '"
                                Doc::highlight(":")
                                "' with '"
                                Doc::highlight("=")
                                "'."
                            }).err()
                        }
                        other => other,
                    }?;
                    let v = self.check_expr(type_any(), *value_span, value)?;
                    Ok(SeqType::UntypedDict(SeqSourceDict::Assoc(*op_span), k, v))
                }
                SeqType::TypedDict { key_super, key_infer, value_super, value_infer, .. } => {
                    let k = self.check_expr(key_super, *key_span, key)?;
                    let v = self.check_expr(value_super, *value_span, value)?;
                    *key_infer = key_infer.meet(&k);
                    *value_infer = value_infer.meet(&v);
                    Ok(seq_type)
                }
                SeqType::TypedList { .. } | SeqType::UntypedList(..) => op_span
                    .error("Expected single element, not key-value.")
                    .with_help(
                        "Key-value pairs are allowed in dicts, which are enclosed in '{}', not '[]'.",
                    ).err(),
                SeqType::TypedSet { set_source, .. } => {
                    let mut error = op_span.error(
                        "Expected single element, not key-value, because the collection is a set."
                    );
                    set_source.explain_error(Side::Expected, &mut error);
                    error.err()
                }
                SeqType::UntypedSet(src, _elem) => src.add_note(
                    op_span.error("Expected single element, not key-value.")
                ).err(),
                SeqType::UntypedDict(_first, key_meet, value_meet) => {
                    let k = self.check_expr(type_any(), *key_span, key)?;
                    let v = self.check_expr(type_any(), *value_span, value)?;
                    *key_meet = key_meet.meet(&k);
                    *value_meet = value_meet.meet(&v);
                    Ok(seq_type)
                }
            }
            Yield::UnpackElems { unpack_span, collection_span, collection, check_elem_type } => {
                self.check_yield_unpack_elems(seq_type, *unpack_span, collection, *collection_span, check_elem_type)
            },
            Yield::UnpackAssocs { unpack_span, collection_span, collection } => {
                self.check_yield_unpack_assocs(seq_type, *unpack_span, collection, *collection_span)
            }
        }
    }

    /// Visit a `..xs` (double dot) unpack inside a sequence literal.
    fn check_yield_unpack_elems(
        &mut self,
        mut seq_type: SeqType,
        unpack_span: Span,
        collection: &mut Expr,
        collection_span: Span,
        check_elem_type: &mut Option<SourcedType>,
    ) -> Result<SeqType> {
        let collection_type = self.check_expr(type_any(), collection_span, collection)?;
        let full_span = unpack_span.union(collection_span);
        let help_unpack_type = || {
            concat! {
                // We could use a note and point at the `..`, but it makes
                // the error verbose. I think just a hint is enough.
                "'" Doc::highlight("..") "' unpacks lists and sets, use '"
                Doc::highlight("...")
                "' to unpack dicts."
            }
        };
        match (&mut seq_type, collection_type.element_type()) {
            // If we weren't sure whether it's a dict or set, and then there is
            // a scalar unpack, then now we know it's a set.
            (SeqType::SetOrDict, ElementType::Any) => Ok(SeqType::UntypedSet(
                SeqSourceSet::Unpack(full_span),
                type_any().clone(),
            )),
            (SeqType::SetOrDict, ElementType::Scalar(inner)) => Ok(SeqType::UntypedSet(
                SeqSourceSet::Unpack(full_span),
                (*inner).clone(),
            )),
            (_, ElementType::None) => self
                .error_not_iterable(collection_span, collection_type)
                .err(),
            (_, ElementType::Dict(..)) => collection_span
                .error("Type mismatch in unpack.")
                .with_body(concat! {
                    "Expected " Doc::str("List").with_markup(Markup::Type)
                    " or " Doc::str("Set").with_markup(Markup::Type)
                    ", but got:"
                    Doc::HardBreak Doc::HardBreak
                    indent! { format_type(&collection_type.type_).into_owned() }
                })
                .with_help(help_unpack_type())
                .err(),
            (
                SeqType::UntypedList(elem_type_meet) | SeqType::UntypedSet(.., elem_type_meet),
                elem_type,
            ) => {
                let inner = match &elem_type {
                    ElementType::Any => type_any(),
                    ElementType::Scalar(inner) => inner.as_ref(),
                    _ => unreachable!("We handle all other cases in the outer match."),
                };
                *elem_type_meet = elem_type_meet.meet(inner);
                Ok(seq_type)
            }
            (
                SeqType::TypedList {
                    elem_super,
                    elem_infer,
                }
                | SeqType::TypedSet {
                    elem_super,
                    elem_infer,
                    ..
                },
                elem_type,
            ) => {
                let inner = match &elem_type {
                    ElementType::Any => type_any(),
                    ElementType::Scalar(inner) => inner.as_ref(),
                    _ => unreachable!("We handle all other cases in the outer match."),
                };
                // Typecheck the inferred type against the expected one. If we
                // can't confirm this statically, then we need to set the
                // expected element type for the runtime type check.
                match inner
                    .is_subtype_of(elem_super)
                    .check_unpack_scalar(full_span)?
                {
                    Typed::Type(_) => { /* Statically ok. */ }
                    Typed::Defer(_) => *check_elem_type = Some(elem_super.clone()),
                }
                *elem_infer = elem_infer.meet(inner);
                Ok(seq_type)
            }
            (SeqType::UntypedDict(..) | SeqType::TypedDict { .. }, _) => full_span
                .error("Invalid unpack in dict.")
                .with_help(help_unpack_type())
                .err(),
        }
    }

    /// Visit a `...xs` (triple dot) unpack inside a sequence literal.
    fn check_yield_unpack_assocs(
        &mut self,
        mut seq_type: SeqType,
        unpack_span: Span,
        collection: &mut Expr,
        collection_span: Span,
    ) -> Result<SeqType> {
        // If we already know the result must be a dict of the given type, then
        // we can propagate the expectation inwards when checking the yield,
        // because if the outer collection is expected to be Dict[K, V], then
        // the inner collection must also be Dict[K, V]. In other cases, we
        // typecheck the inner collection first with a less strict requirement,
        // and then we verify compatibility below in the big element type match.
        let collection_type = match &seq_type {
            SeqType::TypedDict { dict_source, .. } => {
                self.check_expr(dict_source, collection_span, collection)?
            }
            _ => self.check_expr(type_any(), collection_span, collection)?,
        };

        let full_span = unpack_span.union(collection_span);
        let help_unpack_type = || {
            concat! {
                // We could use a note and point at the `..`, but it makes
                // the error verbose. I think just a hint is enough.
                "'" Doc::highlight("...") "' unpacks dicts, use '"
                Doc::highlight("..")
                "' to unpack lists and sets."
            }
        };
        match (&mut seq_type, collection_type.element_type()) {
            (SeqType::TypedList { .. } | SeqType::UntypedList(..), _) => full_span
                .error("Invalid dict unpack in list.")
                .with_help(help_unpack_type())
                .err(),
            // If we weren't sure whether it's a dict or set, and then there is
            // a dict unpack, then now we know it's a dict.
            (SeqType::SetOrDict, ElementType::Any) => Ok(SeqType::UntypedDict(
                SeqSourceDict::Unpack(full_span),
                type_any().clone(),
                type_any().clone(),
            )),
            (SeqType::SetOrDict, ElementType::Dict(dict)) => Ok(SeqType::UntypedDict(
                SeqSourceDict::Unpack(full_span),
                dict.key.clone(),
                dict.value.clone(),
            )),
            (_, ElementType::None) => self
                .error_not_iterable(collection_span, collection_type)
                .err(),
            (_, ElementType::Scalar(..)) => collection_span
                .error("Type mismatch in unpack.")
                .with_body(concat! {
                    "Expected " Doc::str("Dict").with_markup(Markup::Type) ", but got:"
                    Doc::HardBreak Doc::HardBreak
                    indent! { format_type(&collection_type.type_).into_owned() }
                })
                .with_help(help_unpack_type())
                .err(),
            (SeqType::UntypedDict(_src, key_meet, value_meet), elem_type) => {
                let (key, value) = match &elem_type {
                    ElementType::Any => (type_any(), type_any()),
                    ElementType::Dict(kv) => (&kv.key, &kv.value),
                    _ => unreachable!("We handle all other cases in the outer match."),
                };
                *key_meet = key_meet.meet(key);
                *value_meet = value_meet.meet(value);
                Ok(seq_type)
            }
            (
                SeqType::TypedDict {
                    key_infer,
                    value_infer,
                    ..
                },
                ElementType::Dict(kv),
            ) => {
                *key_infer = key_infer.meet(&kv.key);
                *value_infer = value_infer.meet(&kv.value);
                Ok(seq_type)
            }
            (SeqType::TypedDict { .. }, ElementType::Any) => {
                unreachable!("TypedDict checks that the collection is a dict.")
            }

            // For list, it's clear why a list is a list (the square brackets),
            // so we can get away with one error. For sets we can't tell from
            // the brackets, so we include in the error *why* it's a set and not
            // a dict.
            (SeqType::TypedSet { set_source, .. }, _) => {
                let mut err = full_span.error("Invalid dict unpack in set.");
                set_source.explain_error(Side::Actual, &mut err);
                err.with_help(help_unpack_type()).err()
            }
            (SeqType::UntypedSet(set_source, ..), _) => set_source
                .add_note(full_span.error("Invalid dict unpack in set."))
                .with_help(help_unpack_type())
                .err(),
        }
    }

    fn check_stmt(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Let {
                ident_span: _,
                ident,
                type_,
                value_span,
                value,
            } => {
                let required_type = match type_ {
                    None => type_any().clone(),
                    Some(type_expr) => eval_type_expr(type_expr)?,
                };
                let inferred = self.check_expr(&required_type, *value_span, value)?;

                // The inferred type is at least as precise as the expected type,
                // as it is a subtype. But when a user specifies a type for a
                // variable, we should bind exactly that type, even if it means
                // losing information.
                let bound_type = match type_ {
                    None => inferred,
                    Some(_) => required_type,
                };
                self.env.push(ident.clone(), bound_type);

                Ok(())
            }
            Stmt::Assert {
                condition_span,
                condition,
                message_span,
                message,
            } => {
                // The condition has to be a boolean, the message can be any value.
                self.check_expr(type_bool_condition(), *condition_span, condition)?;
                self.check_expr(type_any(), *message_span, message)?;
                Ok(())
            }
            Stmt::Trace {
                message_span,
                message,
            } => {
                self.check_expr(type_any(), *message_span, message)?;
                Ok(())
            }
        }
    }
}

/// We know it's a set due to this reason.
enum SeqSourceSet {
    Scalar(Span),
    Unpack(Span),
}

/// We know it's a dict due to this reason.
enum SeqSourceDict {
    Assoc(Span),
    Unpack(Span),
}

impl SeqSourceSet {
    fn add_note(&self, err: Error) -> Error {
        match *self {
            SeqSourceSet::Scalar(first) => err.with_note(
                first,
                "The collection is a set and not a dict, because it starts with a single value.",
            ),
            SeqSourceSet::Unpack(unpack) => err.with_note(
                unpack,
                "The collection is a set and not a dict, because of this unpack.",
            ),
        }
    }
}

impl SeqSourceDict {
    fn add_note(&self, err: Error) -> Error {
        match *self {
            SeqSourceDict::Assoc(first) => err.with_note(
                first,
                "The collection is a dict and not a set, because it starts with a key-value.",
            ),
            SeqSourceDict::Unpack(unpack) => err.with_note(
                unpack,
                "The collection is a dict and not a set, because of the key-value unpack here.",
            ),
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
enum SeqType {
    /// It's still unclear whether this is a set or a dict.
    SetOrDict,

    /// We expect a list here with the following element type.
    TypedList {
        /// The required element type. Supertype of the inferred type.
        elem_super: SourcedType,
        /// The inferred element type, the `meet` of all elements.
        elem_infer: SourcedType,
    },

    /// We expect a set here with the following element type.
    TypedSet {
        /// The reason we are expecting a set.
        set_source: SourcedType,
        /// The required element type. Supertype of the inferred type.
        elem_super: SourcedType,
        /// The inferred element type, the `meet` of all elements.
        elem_infer: SourcedType,
    },

    /// We expect a dict here with the following key-value types.
    TypedDict {
        /// The reason we are expecting a dict.
        dict_source: SourcedType,
        /// The required key type. Supertype of the inferred type.
        key_super: SourcedType,
        /// The inferred key type, the `meet` of all keys.
        key_infer: SourcedType,
        /// The required value type. Supertype of the inferred type.
        value_super: SourcedType,
        /// The inferred value type.
        value_infer: SourcedType,
    },

    /// We found a list, and the meet of the elements is as follows.
    UntypedList(SourcedType),

    /// We found a set, as evidenced by the span of the first scalar.
    ///
    /// We also track the `meet` of all the elements.
    UntypedSet(SeqSourceSet, SourcedType),

    /// We found a dict, as evidenced by the span of the first key-value.
    ///
    /// We also track the `meet` of the key and value types.
    UntypedDict(SeqSourceDict, SourcedType, SourcedType),
}

impl SeqType {
    /// Return the inferred type for this sequence.
    ///
    /// Takes the span of the full collection literal.
    fn into_type(self, span: Span) -> SourcedType {
        let type_ = match self {
            // An empty literal `{}` is a dict, not a set, because it is a dict in json.
            SeqType::SetOrDict => Type::Dict(Rc::new(Dict {
                key: SourcedType::void(span),
                value: SourcedType::void(span),
            })),
            SeqType::UntypedList(t) | SeqType::TypedList { elem_infer: t, .. } => {
                Type::List(Rc::new(t))
            }
            SeqType::UntypedSet(.., t) | SeqType::TypedSet { elem_infer: t, .. } => {
                Type::Set(Rc::new(t))
            }
            SeqType::UntypedDict(.., k, v)
            | SeqType::TypedDict {
                key_infer: k,
                value_infer: v,
                ..
            } => Type::Dict(Rc::new(Dict { key: k, value: v })),
        };
        SourcedType {
            type_,
            source: Source::Literal(span),
        }
    }
}
