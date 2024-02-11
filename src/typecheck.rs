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
use crate::type_req::{DictReq, FunctionReq, ReqType, TypeReq, Typed};
use crate::types::{self, Type};

pub type Env = crate::env::Env<Type>;

/// Return the default environment with prelude in scope.
pub fn prelude() -> Env {
    let mut env = Env::new();
    // TODO: Type std correctly once we have record types.
    env.push("std".into(), Type::Dynamic);
    env
}

/// Convert a type name into the corresponding primitive type requirement.
fn get_primitive_type(name: &str) -> Option<ReqType> {
    match name {
        "Null" => Some(ReqType::Null),
        "Bool" => Some(ReqType::Bool),
        "Int" => Some(ReqType::Int),
        "String" => Some(ReqType::String),
        _ => None,
    }
}

/// Parse a type expression.
fn eval_type_expr(expr: &AType) -> Result<TypeReq> {
    match expr {
        AType::Term { span, name } => {
            if let Some(prim) = get_primitive_type(name.as_ref()) {
                return Ok(TypeReq::Annotation(*span, prim));
            }
            match name.as_ref() {
                // TODO: Should we allow `_` as type too?
                "Dynamic" => Ok(TypeReq::None),
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
            let fn_type = Rc::new(FunctionReq {
                args: args_types,
                result: result_type,
            });
            Ok(TypeReq::Annotation(*span, ReqType::Function(fn_type)))
        }
        AType::Apply { span, name, args } => {
            let args_types = args
                .iter()
                .map(eval_type_expr)
                .collect::<Result<Vec<_>>>()?;
            let req_type = eval_type_apply(*span, name.as_ref(), &args_types)?;
            Ok(TypeReq::Annotation(*span, req_type))
        }
    }
}

/// Evaluate type constructor application (generic instantiation).
fn eval_type_apply(name_span: Span, name: &str, args: &[TypeReq]) -> Result<ReqType> {
    match name {
        "Dict" => match args {
            [tk, tv] => {
                let dict = DictReq {
                    key: tk.clone(),
                    value: tv.clone(),
                };
                Ok(ReqType::Dict(Rc::new(dict)))
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
            [te] => Ok(ReqType::List(Rc::new(te.clone()))),
            // TODO: As above for dict, we can do a better job of the error.
            _ => name_span
                .error(concat! {
                    "Type 'List' takes one type parameter (the element type), but got "
                    args.len().to_string() "."
                })
                .err(),
        },
        "Set" => match args {
            [te] => Ok(ReqType::Set(Rc::new(te.clone()))),
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
    pub fn check_expr(&mut self, req: &TypeReq, expr_span: Span, expr: &mut Expr) -> Result<Type> {
        let expr_type = match expr {
            Expr::Stmt {
                stmt,
                body_span,
                body,
            } => {
                let ck = self.env.checkpoint();
                self.check_stmt(stmt)?;
                let t = self.check_expr(req, *body_span, body)?;
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
                req.check_type(expr_span, &Type::Dynamic)?
            }

            Expr::BraceLit { elements: seqs, .. } => {
                let mut is_error = false;
                // If we have a requirement on the element type, extract it.
                let mut seq_type = match req.req_type() {
                    Some(ReqType::Set(t)) => SeqType::TypedSet {
                        set_req: req.clone(),
                        elem_req: t.as_ref().clone(),
                        elem_type: Type::Void,
                    },
                    Some(ReqType::Dict(kv)) => {
                        SeqType::TypedDict {
                            dict_req: req.clone(),
                            key_req: kv.key.clone(),
                            value_req: kv.value.clone(),
                            key_type: Type::Void,
                            value_type: Type::Void,
                        }
                    }
                    None => SeqType::SetOrDict,
                    // If we are expecting something other than a dict or list,
                    // then this is definitely a type error. But to be able to
                    // report it in full detail, we first infer the type of the
                    // sequence.
                    _ => {
                        is_error = true;
                        SeqType::SetOrDict
                    }
                };

                // Typecheck all the elements, and enforce the element
                // requirement if we have one. This at the same time infers the
                // element type.
                for seq in seqs {
                    seq_type = self.check_seq(seq, seq_type)?;
                }

                if is_error {
                    req.check_type(expr_span, &seq_type.into_type())?
                } else {
                    Typed::Type(seq_type.into_type())
                }
            }

            Expr::BracketLit { elements: seqs, .. } => {
                // This follows the same structure as `BraceLit`, see comments above.
                let mut is_error = false;
                let mut seq_type = match req.req_type() {
                    Some(ReqType::List(t)) => SeqType::TypedList(t.as_ref().clone(), Type::Void),
                    None => SeqType::UntypedList(Type::Void),
                    _ => {
                        is_error = true;
                        SeqType::UntypedList(Type::Void)
                    }
                };
                for seq in seqs {
                    seq_type = self.check_seq(seq, seq_type)?;
                }

                if is_error {
                    req.check_type(expr_span, &seq_type.into_type())?
                } else {
                    Typed::Type(seq_type.into_type())
                }
            }

            Expr::NullLit => req.check_type(expr_span, &Type::Null)?,
            Expr::BoolLit(..) => req.check_type(expr_span, &Type::Bool)?,
            Expr::IntegerLit(..) => req.check_type(expr_span, &Type::Int)?,
            Expr::StringLit(..) => req.check_type(expr_span, &Type::String)?,

            Expr::Format(fragments) => {
                // Typecheck the fragments. For now we don't demand statically
                // that they can be formatted, but we do descend into them to
                // catch other type errors. TODO: check formatability statically.
                for fragment in fragments {
                    self.check_expr(&TypeReq::None, fragment.span, &mut fragment.body)?;
                }
                // Format strings evaluate to string values.
                req.check_type(expr_span, &Type::String)?
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
                self.check_expr(&TypeReq::Condition, *condition_span, condition)?;

                // TODO: Delete the runtime type check in the evaluator, this is
                // now a static typecheck.

                let type_then = self.check_expr(req, *span_then, body_then)?;
                let type_else = self.check_expr(req, *span_else, body_else)?;

                // The inferred type is the meet of the two sides, which may be
                // more specific than the requirement (which they satisfy).
                Typed::Type(type_then.meet(&type_else))
            }

            Expr::Var { span, ident } => match self.env.lookup(ident) {
                None => return span.error("Unknown variable.").err(),
                Some(t) => req.check_type(expr_span, t)?,
            },

            Expr::Field { inner, inner_span, .. } => {
                self.check_expr(&TypeReq::None, *inner_span, inner)?;
                // At this point, we defer all field lookups to runtime checks.
                // a few methods we could resolve statically already, but we need
                // record types to really make this useful.
                req.check_type(expr_span, &Type::Dynamic)?
            }

            Expr::Function { arrow_span, args, body_span, body } => {
                let fn_type = self.check_function(req, expr_span, args, *body_span, body)?;

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

                Typed::Type(Type::Function(fn_type))
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
                let fn_type = self.check_expr(&TypeReq::None, *function_span, function)?;

                let result_type = match &fn_type {
                    // TODO: Typecheck call args.
                    Type::Function(f) => &f.result,
                    Type::Dynamic => &Type::Dynamic,
                    not_function => return function_span.error(concat! {
                        "This cannot be called. Expected function but found:"
                        Doc::HardBreak
                        Doc::HardBreak
                        indent! { format_type(not_function).into_owned() }
                    }).err()
                };

                // TODO: When the function type is statically known, possibly
                // check the args statically. But we can't know the function
                // type statically in all cases, so since the runtime check will
                // have to exist anyway, we rely on that for now. We still
                // descend into the args to typecheck them, but without any
                // requirement.
                for (arg_span, arg) in args {
                    self.check_expr(&TypeReq::None, *arg_span, arg)?;
                }

                req.check_type(expr_span, result_type)?
            }

            Expr::Index { open, collection_span, collection, index_span, index, .. } => {
                let collection_type = self.check_expr(&TypeReq::None, *collection_span, collection)?;
                let index_req = match collection_type {
                    Type::List(..) => TypeReq::IndexList,
                    // TODO: Store the TypeReq in the dict keys, so we can propagate it.
                    Type::Dict(..) => TypeReq::None,
                    Type::Dynamic => TypeReq::None,
                    not_indexable => {
                        return open
                            .error("Indexing is not supported here.")
                            .with_body(concat!{
                                "Expected a dict or list, but got:"
                                Doc::HardBreak Doc::HardBreak
                                indent! { format_type(&not_indexable).into_owned() }
                            })
                            .err()
                    }
                };
                let result_type = self.check_expr(&index_req, *index_span, index)?;
                req.check_type(expr_span, &result_type)?
            }

            Expr::UnOp { op_span, op, body_span, body, .. } => {
                let result_type = self.check_unop(*op_span, *op, *body_span, body)?;
                req.check_type(expr_span, &result_type)?
            },

            Expr::BinOp { op_span, op, lhs_span, lhs, rhs_span, rhs, .. } => {
                let result_type = self.check_binop(*op_span, *op, *lhs_span, *rhs_span, lhs, rhs)?;
                req.check_type(expr_span, &result_type)?
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
                    type_: req.clone(),
                    body: Box::new(tmp),
                };
                Ok(t)
            }
        }
    }

    /// Typecheck a function definition.
    fn check_function(
        &mut self,
        req: &TypeReq,
        expr_span: Span,
        args: &[Ident],
        body_span: Span,
        body: &mut Expr,
    ) -> Result<Rc<types::Function>> {
        let mut arg_types = Vec::with_capacity(args.len());

        let checkpoint = self.env.checkpoint();
        let mut is_error = false;

        let body_req = match req.req_type() {
            Some(ReqType::Function(fn_req)) => {
                // TODO: Push the args with the right types into the
                // environment, check arity, etc.
                for arg in args.iter() {
                    let arg_type = Type::Dynamic;
                    arg_types.push(arg_type.clone());
                    self.env.push(arg.clone(), arg_type);
                }
                &fn_req.result
            }
            not_fn_req => {
                // If there is no type requirement at all on this function, then
                // all the args have an unknown type and we have no requirement
                // on the result. If there is a requirement but not for a
                // function, then this is a type error, but we'll still
                // typecheck the function first and report the error later.
                is_error = not_fn_req.is_some();
                for arg in args.iter() {
                    let arg_type = Type::Dynamic;
                    arg_types.push(arg_type.clone());
                    self.env.push(arg.clone(), arg_type);
                }
                &TypeReq::None
            }
        };

        let result_type = self.check_expr(body_req, body_span, body)?;
        self.env.pop(checkpoint);

        let fn_type_inner = Rc::new(types::Function {
            args: arg_types,
            result: result_type,
        });

        if is_error {
            // This check will fail, this is just an easy way to construct the
            // right error.
            req.check_type(expr_span, &Type::Function(fn_type_inner.clone()))?;
        };

        Ok(fn_type_inner)
    }

    fn check_unop(
        &mut self,
        op_span: Span,
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
        let (req, result_type) = match op {
            UnOp::Neg => (TypeReq::Operator(op_span, ReqType::Int), Type::Int),
            UnOp::Not => (TypeReq::Operator(op_span, ReqType::Bool), Type::Bool),
        };
        self.check_expr(&req, body_span, body)?;
        Ok(result_type)
    }

    fn check_binop(
        &mut self,
        op_span: Span,
        op: BinOp,
        lhs_span: Span,
        rhs_span: Span,
        lhs: &mut Expr,
        rhs: &mut Expr,
    ) -> Result<Type> {
        let (req, result_type) = match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                (TypeReq::Operator(op_span, ReqType::Int), Type::Int)
            }
            BinOp::And | BinOp::Or => (TypeReq::Operator(op_span, ReqType::Bool), Type::Bool),
            BinOp::Lt | BinOp::LtEq | BinOp::Gt | BinOp::GtEq | BinOp::Eq | BinOp::Neq => {
                (TypeReq::None, Type::Bool)
            }
            BinOp::Union => return self.check_binop_union(op_span, lhs_span, rhs_span, lhs, rhs),
        };
        self.check_expr(&req, lhs_span, lhs)?;
        self.check_expr(&req, rhs_span, rhs)?;
        Ok(result_type)
    }

    fn check_binop_union(
        &mut self,
        op_span: Span,
        lhs_span: Span,
        rhs_span: Span,
        lhs: &mut Expr,
        rhs: &mut Expr,
    ) -> Result<Type> {
        let lhs_type = self.check_expr(&TypeReq::None, lhs_span, lhs)?;
        let rhs_type = self.check_expr(&TypeReq::None, rhs_span, rhs)?;
        let result_type = match (&lhs_type, &rhs_type) {
            // TODO: There rules are a bit ad-hoc. Maybe don't allow | with
            // list? Or do allow, but allow it on the left-hand side too?
            (Type::Dict(..), Type::Dict(..)) => lhs_type.meet(&rhs_type),
            (Type::Set(..), Type::Set(..)) => lhs_type.meet(&rhs_type),
            (Type::Set(tl), Type::List(tr)) => Type::Set(Rc::new(tl.meet(tr.as_ref()))),
            (Type::Dynamic, _) | (_, Type::Dynamic) => Type::Dynamic,
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
                let collection_type =
                    self.check_expr(&TypeReq::None, *collection_span, collection)?;
                let ck = self.env.checkpoint();

                match collection_type {
                    // If we don't know the type, we can't verify the number of
                    // loop variables, and we don't know their types.
                    Type::Dynamic => {
                        for ident in idents {
                            self.env.push(ident.clone(), Type::Dynamic);
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
                        self.env.push(idents[0].clone(), (*element_type).clone());
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
                        self.env.push(idents[0].clone(), (*element_type).clone());
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

                let t = self.check_seq(body, seq_type)?;
                self.env.pop(ck);
                Ok(t)
            }
            Seq::If {
                condition_span,
                condition,
                body,
            } => {
                self.check_expr(&TypeReq::Condition, *condition_span, condition)?;
                self.check_seq(body, seq_type)
            }
        }
    }

    /// Visit a yield inside a sequence literal.
    fn check_yield(&mut self, yield_: &mut Yield, mut seq_type: SeqType) -> Result<SeqType> {
        match yield_ {
            Yield::Elem { span, value } => match &mut seq_type {
                SeqType::SetOrDict => {
                    let t = self.check_expr(&TypeReq::None, *span, value)?;
                    Ok(SeqType::UntypedSet(*span, t))
                }
                SeqType::TypedList(r, elem_type_meet) | SeqType::TypedSet { elem_req: r, elem_type: elem_type_meet, .. } => {
                    // First we check that the element satisfies the requirement.
                    // That gives us an inferred type that can be more precise.
                    // Meet it with what we have so far.
                    let elem_type = self.check_expr(r, *span, value)?;
                    *elem_type_meet = elem_type_meet.meet(&elem_type);
                    Ok(seq_type)
                }
                SeqType::TypedDict { dict_req, .. } => {
                    let err = span.error(
                        "Expected key-value, not a scalar element, because the collection is a dict."
                    );
                    dict_req.add_context(err).err()
                }
                SeqType::UntypedList(elem_type_meet) | SeqType::UntypedSet(.., elem_type_meet) => {
                    let elem_type = self.check_expr(&TypeReq::None, *span, value)?;
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
                    let k = self.check_expr(&TypeReq::None, *key_span, key)?;
                    let v = self.check_expr(&TypeReq::None, *value_span, value)?;
                    Ok(SeqType::UntypedDict(*op_span, k, v))
                }
                SeqType::TypedDict { key_req, value_req, key_type, value_type, .. } => {
                    let k = self.check_expr(key_req, *key_span, key)?;
                    let v = self.check_expr(value_req, *value_span, value)?;
                    *key_type = key_type.meet(&k);
                    *value_type = value_type.meet(&v);
                    Ok(seq_type)
                }
                SeqType::TypedList(..) | SeqType::UntypedList(..) => op_span
                    .error("Expected scalar element, not key-value.")
                    .with_help(
                        "Key-value pairs are allowed in dicts, which are enclosed in '{}', not '[]'.",
                    ).err(),
                SeqType::TypedSet { set_req, .. } => {
                    let err = op_span.error(
                        "Expected scalar element, not key-value, because the collection is a set."
                    );
                    set_req.add_context(err).err()
                }
                SeqType::UntypedSet(first, _elem) => op_span
                    .error("Expected scalar element, not key-value.")
                    .with_note(
                        *first,
                        "The collection is a set and not a dict, because it starts with a scalar value.",
                    )
                    .err(),
                SeqType::UntypedDict(_first, key_meet, value_meet) => {
                    let k = self.check_expr(&TypeReq::None, *key_span, key)?;
                    let v = self.check_expr(&TypeReq::None, *value_span, value)?;
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
                let req = match type_ {
                    None => TypeReq::None,
                    Some(type_expr) => eval_type_expr(type_expr)?,
                };
                let inferred = self.check_expr(&req, *value_span, value)?;

                // The inferred type is at least as precise as the expected type,
                // as it is a subtype. But when a user specifies a type for a
                // variable, we should bind exactly that type, even if it means
                // losing information.
                let bound_type = match type_ {
                    None => inferred,
                    Some(_) => req.to_type(),
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
                self.check_expr(&TypeReq::Condition, *condition_span, condition)?;
                self.check_expr(&TypeReq::None, *message_span, message)?;
                Ok(())
            }
            Stmt::Trace {
                message_span,
                message,
            } => {
                self.check_expr(&TypeReq::None, *message_span, message)?;
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
    ///
    /// We also track the `meet` of the elements, which should be a subtype of
    /// the requirement.
    TypedList(TypeReq, Type),

    /// We expect a set here with the following element type.
    ///
    /// We also track the `meet` of the elements, which should be a subtype of
    /// the requirement.
    TypedSet {
        /// The reason we are expecting a set.
        set_req: TypeReq,
        /// The required element type.
        elem_req: TypeReq,
        /// The inferred element type.
        elem_type: Type,
    },

    /// We expect a dict here with the following key-value types.
    ///
    /// We also track the `meet` of the elements, which should be a subtype of
    /// the requirement.
    TypedDict {
        /// The reason we are expecting a dict.
        dict_req: TypeReq,
        /// The required key type.
        key_req: TypeReq,
        /// The required value type.
        value_req: TypeReq,
        /// The inferred key type.
        key_type: Type,
        /// The inferred value type.
        value_type: Type,
    },

    /// We found a list, and the meet of the elements is as follows.
    UntypedList(Type),

    /// We found a set, as evidenced by the span of the first scalar.
    ///
    /// We also track the `meet` of all the elements.
    UntypedSet(Span, Type),

    /// We found a dict, as evidenced by the span of the first key-value.
    ///
    /// We also track the `meet` of the key and value types.
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
            SeqType::UntypedList(t) | SeqType::TypedList(_, t) => Type::List(Rc::new(t)),
            SeqType::UntypedSet(.., t) | SeqType::TypedSet { elem_type: t, .. } => {
                Type::Set(Rc::new(t))
            }
            SeqType::UntypedDict(.., k, v)
            | SeqType::TypedDict {
                key_type: k,
                value_type: v,
                ..
            } => Type::Dict(Rc::new(types::Dict { key: k, value: v })),
        }
    }
}
