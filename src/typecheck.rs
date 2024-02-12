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

use crate::ast::{BinOp, Expr, Ident, Seq, Stmt, Type as AType, UnOp, Yield};
use crate::error::{IntoError, Result};
use crate::fmt_type::format_type;
use crate::pprint::{concat, indent, Doc};
use crate::source::Span;
use crate::types::{report_type_mismatch, Dict, Function, Source, SourcedType, Type, Typed};

pub type Env = crate::env::Env<SourcedType>;

/// Return the default environment with prelude in scope.
pub fn prelude() -> Env {
    let mut env = Env::new();
    // TODO: Type std correctly once we have record types.
    env.push("std".into(), SourcedType::any());
    env
}

/// Convert a type name into the corresponding primitive type requirement.
fn get_primitive_type(name: &str) -> Option<Type> {
    match name {
        "Bool" => Some(Type::Bool),
        "Dynamic" => Some(Type::Dynamic),
        "Int" => Some(Type::Int),
        "Null" => Some(Type::Null),
        "String" => Some(Type::String),
        "Void" => Some(Type::Void),
        // TODO: Should we allow `_` as type too?
        _ => None,
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
                            "Specify a key and value type, e.g. '" Doc::highlight("Dict[String, Int]") "'."
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
                _ => span.error("Unknown type.").err(),
            }
        }
        AType::Function { span, args, result } => {
            let args_types = args
                .iter()
                .map(eval_type_expr)
                .collect::<Result<Vec<_>>>()?;
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
        _ => name_span.error("Unknown generic type.").err(),
    }
}

/// Shorthand for writing [`SourcedType::any`].
pub fn type_any() -> &'static SourcedType {
    &SourcedType {
        type_: Type::Dynamic,
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

/// Construct a `SourcedType` for a `Int` for list indexing.
fn type_int_index() -> &'static SourcedType {
    &SourcedType {
        type_: Type::Int,
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

                // The type of an import is always `Dynamic`. Though if that is
                // the case, that removes one justification for having it be a
                // keyword instead of a builtin method `std.import`. Because we
                // can just type it: `std.import: (fname: String) -> Dynamic`.
                type_any().is_subtype_of(expected).check(expr_span)?
            }

            Expr::BraceLit { elements: seqs, .. } => {
                let mut is_error = false;
                // If we have a requirement on the element type, extract it.
                let mut seq_type = match &expected.type_ {
                    Type::Set(t) => SeqType::TypedSet {
                        set_source: expected.source,
                        elem_super: t.as_ref().clone(),
                        elem_infer: SourcedType::void(expr_span),
                    },
                    Type::Dict(kv) => {
                        SeqType::TypedDict {
                            dict_source: expected.source,
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
                        is_error = not_collection != &Type::Dynamic;
                        SeqType::SetOrDict
                    }
                };

                // Typecheck all the elements, and enforce the element
                // requirement if we have one. This at the same time infers the
                // element type.
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

            Expr::BracketLit { elements: seqs, .. } => {
                // This follows the same structure as `BraceLit`, see comments above.
                let mut is_error = false;
                let mut seq_type = match &expected.type_ {
                    Type::List(t) => SeqType::TypedList {
                        elem_super: t.as_ref().clone(),
                        elem_infer: SourcedType::void(expr_span),
                    },
                    not_list => {
                        is_error = not_list != &Type::Dynamic;
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
            Expr::IntegerLit(..) => type_literal(expr_span, Type::Int).is_subtype_of(expected).check(expr_span)?,
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

                // TODO: Delete the runtime type check in the evaluator, this is
                // now a static typecheck.

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

            Expr::Function { arrow_span, args, body_span, body } => {
                let fn_type = self.check_function(expected, expr_span, args, *body_span, body)?;

                // Now that we know the type of the function, preserve it in the
                // AST, because we need it in the runtime value. We need to
                // juggle some temporaries to move values out of the old node
                // into the new one.
                let mut args_tmp = Vec::new();
                let mut body_tmp = Box::new(Expr::NullLit);
                std::mem::swap(&mut args_tmp, args);
                std::mem::swap(&mut body_tmp, body);
                *expr = Expr::TypedFunction {
                    arrow_span: *arrow_span,
                    args: args_tmp,
                    body_span: *body_span,
                    body: body_tmp,
                    type_: fn_type.clone(),
                };

                Typed::Type(type_literal(expr_span, Type::Function(fn_type)))
            }

            Expr::Call { function_span, function, args, .. } => {
                // The direction of the requirements for a call could go two ways.
                // Take for example: `(x => x + 1)("42")`. We could say, it's a
                // call with String as first argument, so we push that into the
                // function body, and there is a type error at the `+` because
                // we expect a String but `+` creates an Int. But we could also
                // say, we typecheck the function first, infer `Dynamic -> Int`
                // (with a runtime check inserted at left-hand side of `+`), then
                // we call that with "42", which passes, but the runtime check
                // fails. We go with the latter: we assume function definitions
                // are always correct, and the error is at the call site.
                let fn_type = self.check_expr(type_any(), *function_span, function)?;

                let result_type = match &fn_type.type_ {
                    // TODO: Typecheck call args.
                    Type::Function(f) => &f.result,
                    Type::Dynamic => type_any(),
                    _not_function => {
                        return function_span
                            .error("This cannot be called.")
                            .with_body(report_type_mismatch(&"function", &fn_type))
                            .err()
                    },
                };

                // TODO: When the function type is statically known, possibly
                // check the args statically. But we can't know the function
                // type statically in all cases, so since the runtime check will
                // have to exist anyway, we rely on that for now. We still
                // descend into the args to typecheck them, but without any
                // requirement.
                for (arg_span, arg) in args {
                    self.check_expr(type_any(), *arg_span, arg)?;
                }

                result_type.is_subtype_of(expected).check(expr_span)?
            }

            Expr::Index { open, collection_span, collection, index_span, index, .. } => {
                let collection_type = self.check_expr(type_any(), *collection_span, collection)?;
                let (index_type, result_type) = match &collection_type.type_ {
                    Type::List(t) => (type_int_index(), (**t).clone()),
                    Type::Dict(kv) => (&kv.key, kv.value.clone()),
                    Type::Dynamic => (type_any(), type_any().clone()),
                    not_indexable => {
                        return open
                            .error("Indexing is not supported here.")
                            .with_body(concat!{
                                "Expected a dict or list, but got:"
                                Doc::HardBreak Doc::HardBreak
                                indent! { format_type(not_indexable).into_owned() }
                            })
                            .err()
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

            Expr::CheckType { .. } | Expr::TypedFunction { .. } => panic!(
                "Node {expr:?} is inserted by the typechecker, it should not be present before checking."
            ),
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
        args: &[Ident],
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
                for (arg, arg_type) in args.iter().zip(fn_req.args.iter()) {
                    arg_types.push(arg_type.clone());
                    self.env.push(arg.clone(), arg_type.clone());
                }
                &fn_req.result
            }
            not_fn => {
                // If there is no type requirement at all on this function, then
                // all the args have an unknown type and we have no requirement
                // on the result. If there is a requirement but not for a
                // function, then this is a type error, but we'll still
                // typecheck the function first and report the error later.
                is_error = not_fn != &Type::Dynamic;
                for arg in args.iter() {
                    arg_types.push(type_any().clone());
                    self.env.push(arg.clone(), type_any().clone());
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
        //     let x: Int = not 42;
        //
        // Already at the `not`, we know the result is Bool but we need Int, so
        // that's an error. But there *another* error, which is applying `not`
        // to an int, and if we report only one type error, that seems like it
        // should come first, as it comes first in the evaluation order too.
        let (body_type, result_type) = match op {
            UnOp::Neg => (Type::Int, Type::Int),
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
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => (Type::Int, Type::Int),
            BinOp::And | BinOp::Or => (Type::Bool, Type::Bool),
            BinOp::Lt | BinOp::LtEq | BinOp::Gt | BinOp::GtEq | BinOp::Eq | BinOp::Neq => {
                (Type::Dynamic, Type::Bool)
            }
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
            (Type::Dynamic, _) | (_, Type::Dynamic) => type_any().clone(),
            (not_collection, _) => {
                let err = op_span.error(concat! {
                    "Expected Dict or Set as the left-hand side of "
                    Doc::highlight("|")
                    " operator, but found this:"
                    Doc::HardBreak Doc::HardBreak
                    indent! { format_type(not_collection).into_owned() }
                });
                return err.err();
            }
        };

        Ok(result_type)
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

                match &collection_type.type_ {
                    // If we don't know the type, we can't verify the number of
                    // loop variables, and we don't know their types.
                    Type::Dynamic => {
                        for ident in idents {
                            self.env.push(ident.clone(), type_any().clone());
                        }
                    }
                    Type::Dict(dict) => {
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
                        self.env.push(idents[0].clone(), (**element_type).clone());
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
                        self.env.push(idents[0].clone(), (**element_type).clone());
                    }
                    not_collection => {
                        return collection_span
                            .error("This is not iterable.")
                            .with_body(concat! {
                                "Expected a collection, but got:"
                                Doc::HardBreak Doc::HardBreak
                                indent! { format_type(not_collection).into_owned() }
                            })
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
                    Ok(SeqType::UntypedSet(*span, t))
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
                    let err = span.error(
                        "Expected key-value, not a scalar element, because the collection is a dict."
                    );
                    dict_source.clarify_error(&"Dict", err).err()
                }
                SeqType::UntypedList(elem_type_meet) | SeqType::UntypedSet(.., elem_type_meet) => {
                    let elem_type = self.check_expr(type_any(), *span, value)?;
                    *elem_type_meet = elem_type_meet.meet(&elem_type);
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
            Yield::Assoc { op_span, key_span, key, value_span, value } => match &mut seq_type {
                SeqType::SetOrDict => {
                    let k = self.check_expr(type_any(), *key_span, key)?;
                    let v = self.check_expr(type_any(), *value_span, value)?;
                    Ok(SeqType::UntypedDict(*op_span, k, v))
                }
                SeqType::TypedDict { key_super, key_infer, value_super, value_infer, .. } => {
                    let k = self.check_expr(key_super, *key_span, key)?;
                    let v = self.check_expr(value_super, *value_span, value)?;
                    *key_infer = key_infer.meet(&k);
                    *value_infer = value_infer.meet(&v);
                    Ok(seq_type)
                }
                SeqType::TypedList { .. } | SeqType::UntypedList(..) => op_span
                    .error("Expected scalar element, not key-value.")
                    .with_help(
                        "Key-value pairs are allowed in dicts, which are enclosed in '{}', not '[]'.",
                    ).err(),
                SeqType::TypedSet { set_source, .. } => {
                    let err = op_span.error(
                        "Expected scalar element, not key-value, because the collection is a set."
                    );
                    set_source.clarify_error(&"Set", err).err()
                }
                SeqType::UntypedSet(first, _elem) => op_span
                    .error("Expected scalar element, not key-value.")
                    .with_note(
                        *first,
                        "The collection is a set and not a dict, because it starts with a scalar value.",
                    )
                    .err(),
                SeqType::UntypedDict(_first, key_meet, value_meet) => {
                    let k = self.check_expr(type_any(), *key_span, key)?;
                    let v = self.check_expr(type_any(), *value_span, value)?;
                    *key_meet = key_meet.meet(&k);
                    *value_meet = value_meet.meet(&v);
                    Ok(seq_type)
                }
            }
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
        set_source: Source,
        /// The required element type. Supertype of the inferred type.
        elem_super: SourcedType,
        /// The inferred element type, the `meet` of all elements.
        elem_infer: SourcedType,
    },

    /// We expect a dict here with the following key-value types.
    TypedDict {
        /// The reason we are expecting a dict.
        dict_source: Source,
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
    UntypedSet(Span, SourcedType),

    /// We found a dict, as evidenced by the span of the first key-value.
    ///
    /// We also track the `meet` of the key and value types.
    UntypedDict(Span, SourcedType, SourcedType),
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
