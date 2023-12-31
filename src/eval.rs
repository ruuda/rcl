// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Evaluation turns ASTs into values.

use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast::{BinOp, Expr, FormatFragment, Seq, Stmt, UnOp, Yield};
use crate::error::{Error, IntoError, Result};
use crate::fmt_rcl::{self, format_rcl};
use crate::loader::Loader;
use crate::pprint::{concat, Doc};
use crate::runtime::{CallArg, Env, Function, FunctionCall, MethodCall, Value};
use crate::source::{DocId, Span};
use crate::stdlib;
use crate::tracer::Tracer;

/// An entry on the evaluation stack.
pub struct EvalContext {
    /// The document that we are evaluating.
    pub doc: DocId,
    /// The source that caused evaluation of this document.
    pub imported_from: Option<Span>,
}

pub struct Evaluator<'a> {
    pub loader: &'a mut Loader,
    pub tracer: &'a mut dyn Tracer,
    pub import_stack: Vec<EvalContext>,

    /// The single instance of the standard library.
    ///
    /// Because it is immutable, it can be reused across evaluations.
    pub stdlib: Rc<Value>,

    /// The depth of the evaluation stack.
    ///
    /// Used to error before we overflow the native stack.
    pub eval_depth: u32,
}

impl<'a> Evaluator<'a> {
    pub fn new(loader: &'a mut Loader, tracer: &'a mut dyn Tracer) -> Evaluator<'a> {
        Evaluator {
            loader,
            tracer,
            import_stack: Vec::new(),
            stdlib: stdlib::initialize(),
            eval_depth: 0,
        }
    }

    #[inline]
    fn inc_eval_depth(&mut self, at: Span) -> Result<()> {
        // Error out when the call stack gets too deep, instead of waiting for
        // the native call stack to overflow. In practice, unless you are doing
        // recursion, the call stack shouldn't be extremely deep, and for
        // recursion we need a better solution, so set a fairly low limit.
        let max_eval_depth = 150;
        self.eval_depth += 1;

        if self.eval_depth >= max_eval_depth {
            return at
                .error(concat! {
                    "Evaluation budget exceeded. "
                    "This expression exceeds the maximum evaluation depth of "
                    max_eval_depth.to_string()
                    "."
                })
                .err();
        }

        Ok(())
    }

    #[inline]
    fn dec_eval_depth(&mut self) {
        self.eval_depth -= 1;
    }

    /// Evaluate a document as the entry point of evaluation.
    pub fn eval_doc(&mut self, env: &mut Env, doc: DocId) -> Result<Rc<Value>> {
        debug_assert!(self.import_stack.is_empty());
        let expr = self.loader.get_ast(doc)?;
        let ctx = EvalContext {
            doc,
            imported_from: None,
        };
        self.import_stack.push(ctx);
        let result = self.eval_expr(env, &expr)?;
        self.import_stack.pop().expect("Push/pop are balanced.");
        Ok(result)
    }

    /// Evaluate a document for an import.
    fn eval_import(&mut self, doc: DocId, imported_from: Span) -> Result<Rc<Value>> {
        // Before we allow the import, check that this would not create a cycle.
        let mut error: Option<Error> = None;
        for ctx in &self.import_stack {
            if ctx.doc == doc {
                debug_assert!(
                    error.is_none(),
                    "There should not be a cycle in the stack already.",
                );
                let err = imported_from.error("This import creates a cycle.");
                error = Some(err);
            }
            if let Some(error) = error.as_mut() {
                if let Some(src) = ctx.imported_from {
                    error.add_note(src, "Imported here.");
                }
            }
        }
        if let Some(mut err) = error {
            // Reverse the notes: we print the error itself first, and then we
            // want to print what the offending file was imported from, etc.
            err.notes.reverse();
            return Err(err.into());
        }

        let expr = self.loader.get_ast(doc)?;
        let ctx = EvalContext {
            doc,
            imported_from: Some(imported_from),
        };

        // Evaluate the import in its own clean environment, it should not be
        // affected by the surrounding environment of the import statement.
        let mut env = Env::with_prelude();

        self.import_stack.push(ctx);
        let result = self.eval_expr(&mut env, &expr)?;
        self.import_stack.pop().expect("Push/pop are balanced.");

        Ok(result)
    }

    fn eval_expr(&mut self, env: &mut Env, expr: &Expr) -> Result<Rc<Value>> {
        match expr {
            Expr::Import {
                path_span,
                path: path_expr,
            } => {
                // We could allow an arbitrary expression that evaluates to a
                // string, but for now we limit ourselves to string literals. It
                // ensures that the import graph can be statically known by
                // traversing the AST without evaluating it, and it ensures
                // filenames can be grepped, there are no "magic" imports. This
                // requirement is artificial, we can relax it later if that turns
                // out to be useful.
                let path = match path_expr.as_ref() {
                    Expr::StringLit(path) => path,
                    Expr::Format(..) => {
                        return path_span
                            .error("Import path must be a string literal without holes.")
                            .err()
                    }
                    _ => {
                        return path_span
                            .error("Import path must be a string literal.")
                            .err()
                    }
                };
                let from = Some(path_span.doc());
                let doc = self
                    .loader
                    .load_path(path.as_ref(), from)
                    .map_err(|mut err| {
                        if err.origin.is_none() {
                            err.origin = Some(*path_span);
                        }
                        err
                    })?;
                self.eval_import(doc, *path_span)
            }

            Expr::BraceLit(seqs) => {
                let mut out = SeqOut::SetOrDict;
                for seq in seqs {
                    self.eval_seq(env, seq, &mut out)?;
                }
                match out {
                    // If we have no keys, it’s a dict, because json has no sets,
                    // and `{}` is a json value that should evaluate to itself.
                    SeqOut::SetOrDict => Ok(Rc::new(Value::Dict(BTreeMap::new()))),
                    SeqOut::Set(_, values) => {
                        let result = values.into_iter().collect();
                        Ok(Rc::new(Value::Set(result)))
                    }
                    SeqOut::Dict(_, keys, values) => {
                        assert_eq!(
                            keys.len(),
                            values.len(),
                            "Should have already reported error about mixing \
                            `k: v` and values in one comprehension."
                        );
                        let mut result = BTreeMap::new();
                        for (k, v) in keys.into_iter().zip(values) {
                            result.insert(k, v);
                        }
                        Ok(Rc::new(Value::Dict(result)))
                    }
                    SeqOut::List(_) => unreachable!("Did not start out as list."),
                }
            }
            Expr::BracketLit(seqs) => {
                let mut out = SeqOut::List(Vec::new());
                for seq in seqs {
                    self.eval_seq(env, seq, &mut out)?;
                }
                match out {
                    SeqOut::List(values) => Ok(Rc::new(Value::List(values))),
                    _ => unreachable!("SeqOut::List type is preserved."),
                }
            }

            Expr::NullLit => Ok(Rc::new(Value::Null)),

            Expr::BoolLit(b) => Ok(Rc::new(Value::Bool(*b))),

            Expr::IntegerLit(i) => Ok(Rc::new(Value::Int(*i))),

            Expr::StringLit(s) => Ok(Rc::new(Value::String(s.clone()))),

            Expr::Format(fragments) => self.eval_format(env, fragments),

            Expr::IfThenElse {
                condition_span,
                condition,
                body_then,
                body_else,
            } => {
                self.inc_eval_depth(*condition_span)?;
                let cond = self.eval_expr(env, condition)?;
                let result = match cond.as_ref() {
                    Value::Bool(true) => self.eval_expr(env, body_then),
                    Value::Bool(false) => self.eval_expr(env, body_else),
                    _ => {
                        // TODO: Make this a type error.
                        let err = condition_span.error("Condition should be a boolean.");
                        Err(err.into())
                    }
                };
                self.dec_eval_depth();
                result
            }

            Expr::Var { span, ident } => match env.lookup(ident) {
                Some(value) => Ok(value.clone()),
                None => Err(span.error("Unknown variable.").into()),
            },

            Expr::Field {
                field_span,
                field: field_name,
                inner_span,
                inner: inner_expr,
            } => {
                self.inc_eval_depth(*inner_span)?;
                let inner = self.eval_expr(env, inner_expr)?;
                self.dec_eval_depth();
                let field_name_value = Value::String(field_name.0.clone());

                let builtin = match (inner.as_ref(), field_name.as_ref()) {
                    (Value::String(_), "chars") => Some(stdlib::STRING_CHARS),
                    (Value::String(_), "contains") => Some(stdlib::STRING_CONTAINS),
                    (Value::String(_), "ends_with") => Some(stdlib::STRING_ENDS_WITH),
                    (Value::String(_), "len") => Some(stdlib::STRING_LEN),
                    (Value::String(_), "parse_int") => Some(stdlib::STRING_PARSE_INT),
                    (Value::String(_), "split") => Some(stdlib::STRING_SPLIT),
                    (Value::String(_), "split_lines") => Some(stdlib::STRING_SPLIT_LINES),
                    (Value::String(_), "starts_with") => Some(stdlib::STRING_STARTS_WITH),

                    (Value::Dict(_), "contains") => Some(stdlib::DICT_CONTAINS),
                    (Value::Dict(_), "except") => Some(stdlib::DICT_EXCEPT),
                    (Value::Dict(_), "get") => Some(stdlib::DICT_GET),
                    (Value::Dict(_), "keys") => Some(stdlib::DICT_KEYS),
                    (Value::Dict(_), "len") => Some(stdlib::DICT_LEN),
                    (Value::Dict(_), "values") => Some(stdlib::DICT_VALUES),
                    (Value::Dict(fields), _field_name) => {
                        // If it wasn't a builtin, look for a key in the dict.
                        return match fields.get(&field_name_value) {
                            Some(v) => Ok(v.clone()),
                            None => {
                                return field_span
                                    .error("Unknown field.")
                                    .with_note(
                                        *inner_span,
                                        concat! {
                                            // TODO: Printing the full value may be overkill,
                                            // the full value could be very large. We
                                            // could print the dict keys here.
                                            "On value: " format_rcl(inner.as_ref()).into_owned()
                                        },
                                    )
                                    .err();
                            }
                        };
                    }

                    (Value::List(_), "contains") => Some(stdlib::LIST_CONTAINS),
                    (Value::List(_), "fold") => Some(stdlib::LIST_FOLD),
                    (Value::List(_), "group_by") => Some(stdlib::LIST_GROUP_BY),
                    (Value::List(_), "key_by") => Some(stdlib::LIST_KEY_BY),
                    (Value::List(_), "len") => Some(stdlib::LIST_LEN),
                    (Value::List(_), "reverse") => Some(stdlib::LIST_REVERSE),

                    (Value::Set(_), "contains") => Some(stdlib::SET_CONTAINS),
                    (Value::Set(_), "except") => Some(stdlib::SET_EXCEPT),
                    (Value::Set(_), "group_by") => Some(stdlib::SET_GROUP_BY),
                    (Value::Set(_), "key_by") => Some(stdlib::SET_KEY_BY),
                    (Value::Set(_), "len") => Some(stdlib::SET_LEN),

                    _other => None,
                };
                match builtin {
                    Some(b) => Ok(Rc::new(Value::BuiltinMethod {
                        receiver_span: *inner_span,
                        receiver: inner,
                        method_span: *field_span,
                        method: b,
                    })),
                    None => {
                        field_span
                            .error("Unknown field.")
                            .with_note(
                                *inner_span,
                                concat! {
                                    // TODO: Printing the full value may be overkill,
                                    // the full value could be very large.
                                    "On value: " format_rcl(inner.as_ref()).into_owned()
                                },
                            )
                            .err()
                    }
                }
            }

            Expr::Stmt { stmt, body } => {
                let ck = env.checkpoint();
                self.eval_stmt(env, stmt)?;
                let result = self.eval_expr(env, body)?;
                env.pop(ck);
                Ok(result)
            }

            Expr::Call {
                open,
                close,
                function_span,
                function: fun_expr,
                args: args_exprs,
            } => {
                // We do strict evaluation, all arguments get evaluated before we go
                // into the call.
                let fun = self.eval_expr(env, fun_expr)?;
                let args = args_exprs
                    .iter()
                    .map(|(span, a)| {
                        Ok(CallArg {
                            span: *span,
                            value: self.eval_expr(env, a)?,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                let call = FunctionCall {
                    call_open: *open,
                    call_close: *close,
                    args: &args[..],
                };
                let error_context = || None;

                self.eval_call(*function_span, fun.as_ref(), call, error_context)
            }

            Expr::Index {
                open,
                collection_span,
                collection: collection_expr,
                index: index_expr,
                index_span,
                ..
            } => {
                let collection = self.eval_expr(env, collection_expr)?;
                let index = self.eval_expr(env, index_expr)?;
                self.eval_index(*open, collection, *collection_span, index, *index_span)
            }

            Expr::Function { span, args, body } => {
                let result = Function {
                    span: *span,
                    env: env.clone(),
                    args: args.clone(),
                    body: Rc::new((**body).clone()),
                };
                Ok(Rc::new(Value::Function(result)))
            }

            Expr::UnOp {
                op,
                op_span,
                body: value_expr,
            } => {
                self.inc_eval_depth(*op_span)?;
                let value = self.eval_expr(env, value_expr)?;
                let result = self.eval_unop(*op, *op_span, value)?;
                self.dec_eval_depth();
                Ok(result)
            }

            Expr::BinOp {
                op,
                op_span,
                lhs: lhs_expr,
                rhs: rhs_expr,
            } => {
                self.inc_eval_depth(*op_span)?;
                let lhs = self.eval_expr(env, lhs_expr)?;
                let rhs = self.eval_expr(env, rhs_expr)?;
                let result = self.eval_binop(*op, *op_span, lhs, rhs)?;
                self.dec_eval_depth();
                Ok(result)
            }
        }
    }

    pub fn eval_call<MkErr>(
        &mut self,
        callee_span: Span,
        callee: &Value,
        call: FunctionCall,
        error_context: MkErr,
    ) -> Result<Rc<Value>>
    where
        MkErr: FnOnce() -> Option<Doc<'static>>,
    {
        let call_open = call.call_open;
        self.inc_eval_depth(call_open)?;

        let result = match callee {
            Value::BuiltinMethod {
                method_span,
                method,
                receiver_span,
                receiver,
            } => {
                let method_call = MethodCall {
                    call,
                    method_span: *method_span,
                    receiver_span: *receiver_span,
                    receiver: receiver.as_ref(),
                };
                (method.f)(self, method_call).map_err(|err| {
                    let msg = match error_context() {
                        None => concat! { "In call to method '" Doc::highlight(method.name) "'." },
                        Some(ctx) => concat! { ctx ", method '" Doc::highlight(method.name) "'." },
                    };
                    err.with_call_frame(call_open, msg).into()
                })
            }
            Value::BuiltinFunction(f) => (f.f)(self, call).map_err(|err| {
                let msg = match error_context() {
                    None => concat! { "In call to function '" Doc::highlight(f.name) "'." },
                    Some(ctx) => concat! { ctx ", function '" Doc::highlight(f.name) "'." },
                };
                err.with_call_frame(call_open, msg).into()
            }),
            Value::Function(fun) => self.eval_function_call(fun, call).map_err(|err| {
                let msg = match error_context() {
                    None => "In call to function.".into(),
                    Some(ctx) => concat! { ctx "." },
                };
                err.with_call_frame(call_open, msg).into()
            }),
            // TODO: Add a proper type error.
            _ => {
                let msg = match error_context() {
                    None => "In call.".into(),
                    Some(ctx) => concat! { ctx "." },
                };
                callee_span
                    .error("This is not a function, it cannot be called.")
                    .with_call_frame(call_open, msg)
                    .err()
            }
        };

        self.dec_eval_depth();

        result
    }

    /// Evaluate a call to a lambda function.
    pub fn eval_function_call(&mut self, fun: &Function, call: FunctionCall) -> Result<Rc<Value>> {
        // TODO: Add a better name, possibly also report the source span where
        // the argument is defined, not just the span of the lambda.
        call.check_arity_dynamic(&fun.args)
            .map_err(|err| err.with_note(fun.span, "Function defined here."))?;

        // TODO: If we could stack multiple layers of envs, then we would not
        // have to clone the full thing.
        let mut env = fun.env.clone();
        for (arg_name, CallArg { value, .. }) in fun.args.iter().zip(call.args) {
            env.push(arg_name.clone(), value.clone());
        }

        self.eval_expr(&mut env, fun.body.as_ref())
    }

    /// Evaluate a format string.
    pub fn eval_format(
        &mut self,
        env: &mut Env,
        fragments: &[FormatFragment],
    ) -> Result<Rc<Value>> {
        let mut results: Vec<Rc<str>> = Vec::new();

        for fragment in fragments {
            let result = self.eval_expr(env, &fragment.body)?;
            match result.as_ref() {
                Value::String(s) => results.push(s.clone()),
                Value::Int(i) => results.push(i.to_string().into()),
                Value::Bool(b) => results.push((if *b { "true" } else { "false" }).into()),
                _ => {
                    return Err(fragment
                        .span
                        .error("This value cannot be interpolated into a string.")
                        .into())
                }
            }
        }

        let mut result = String::with_capacity(results.iter().map(|s| s.len()).sum());
        for s in results {
            result.push_str(s.as_ref());
        }

        Ok(Rc::new(Value::String(result.into())))
    }

    fn eval_index(
        &mut self,
        open_span: Span,
        collection: Rc<Value>,
        collection_span: Span,
        index: Rc<Value>,
        index_span: Span,
    ) -> Result<Rc<Value>> {
        match collection.as_ref() {
            Value::List(xs) => self.eval_index_list(xs, index.as_ref(), index_span),
            Value::Dict(dict) => {
                self.eval_index_dict(dict, collection_span, index.as_ref(), index_span)
            }
            // TODO: Implement indexing into strings.
            Value::String(..) => open_span
                .error("Indexing into a string is not yet supported.")
                .with_note(collection_span, "This is a string.")
                .err(),
            not_indexable => {
                let note = concat! {
                    "Expected a dict or list, but found: "
                    format_rcl(not_indexable).into_owned()
                    "."
                };
                open_span
                    .error("Indexing is not supported here.")
                    .with_note(collection_span, note)
                    .err()
            }
        }
    }

    fn eval_index_list(
        &mut self,
        list: &[Rc<Value>],
        index: &Value,
        index_span: Span,
    ) -> Result<Rc<Value>> {
        let i_signed = match index {
            Value::Int(i) => *i,
            _ => return index_span.error("Index must be an integer.").err(),
        };

        let i = match i_signed {
            _ if i_signed >= 0 && (i_signed as usize) < list.len() => i_signed as usize,
            _ if i_signed >= -(list.len() as i64) && i_signed < 0 => {
                list.len() - (-i_signed as usize)
            }
            _ => {
                let error = concat! {
                    "Index "
                    i_signed.to_string()
                    " is out of bounds for list of length "
                    list.len().to_string()
                    "."
                };
                return index_span.error(error).err();
            }
        };

        Ok(list[i].clone())
    }

    fn eval_index_dict(
        &mut self,
        dict: &BTreeMap<Rc<Value>, Rc<Value>>,
        dict_span: Span,
        index: &Value,
        index_span: Span,
    ) -> Result<Rc<Value>> {
        match dict.get(index) {
            None => index_span
                .error(concat! {
                    "Dict does not have a key "
                    format_rcl(index).into_owned()
                    "."
                })
                .with_note(
                    dict_span,
                    concat! {
                        "On value: "
                        fmt_rcl::dict(dict.iter()).into_owned()
                    },
                )
                .err(),
            Some(v) => Ok(v.clone()),
        }
    }

    fn eval_unop(&mut self, op: UnOp, op_span: Span, v: Rc<Value>) -> Result<Rc<Value>> {
        match (op, v.as_ref()) {
            (UnOp::Not, Value::Bool(x)) => Ok(Rc::new(Value::Bool(!x))),
            (UnOp::Neg, Value::Int(x)) => match x.checked_neg() {
                Some(nx) => Ok(Rc::new(Value::Int(nx))),
                None => {
                    let err = concat! {
                        "Negation of " x.to_string() " would overflow."
                    };
                    op_span.error(err).err()
                }
            },
            (_op, _val) => {
                // TODO: Add a proper type error, report the type of the value.
                Err(op_span
                    .error("This operator is not supported for this value.")
                    .into())
            }
        }
    }

    fn eval_binop(
        &mut self,
        op: BinOp,
        op_span: Span,
        lhs: Rc<Value>,
        rhs: Rc<Value>,
    ) -> Result<Rc<Value>> {
        match (op, lhs.as_ref(), rhs.as_ref()) {
            (BinOp::Union, Value::Dict(xs), Value::Dict(ys)) => {
                let mut result = xs.clone();
                for (k, v) in ys.iter() {
                    result.insert(k.clone(), v.clone());
                }
                Ok(Rc::new(Value::Dict(result)))
            }
            (BinOp::Union, Value::Set(xs), Value::Set(ys)) => {
                let result = xs.union(ys).cloned().collect();
                Ok(Rc::new(Value::Set(result)))
            }
            (BinOp::Union, Value::Set(xs), Value::List(ys)) => {
                let mut result = xs.clone();
                result.extend(ys.iter().cloned());
                Ok(Rc::new(Value::Set(result)))
            }
            // TODO: Could evaluate these boolean expressions lazily, if the
            // language is really pure. But if I enable external side effects like
            // running a program to read its input, that would be questionable to do.
            (BinOp::And, Value::Bool(x), Value::Bool(y)) => Ok(Rc::new(Value::Bool(*x && *y))),
            (BinOp::Or, Value::Bool(x), Value::Bool(y)) => Ok(Rc::new(Value::Bool(*x || *y))),
            (BinOp::Add, Value::Int(x), Value::Int(y)) => match x.checked_add(*y) {
                Some(z) => Ok(Rc::new(Value::Int(z))),
                None => {
                    let err = concat! {
                        "Addition " x.to_string() " + " y.to_string() " would overflow."
                    };
                    op_span.error(err).err()
                }
            },
            (BinOp::Sub, Value::Int(x), Value::Int(y)) => match x.checked_sub(*y) {
                Some(z) => Ok(Rc::new(Value::Int(z))),
                None => {
                    let err = concat! {
                        "Subtraction " x.to_string() " - " y.to_string() " would overflow."
                    };
                    op_span.error(err).err()
                }
            },
            (BinOp::Mul, Value::Int(x), Value::Int(y)) => match x.checked_mul(*y) {
                Some(z) => Ok(Rc::new(Value::Int(z))),
                None => {
                    let err = concat! {
                        "Multiplication " x.to_string() " * " y.to_string() " would overflow."
                    };
                    op_span.error(err).err()
                }
            },
            (BinOp::Div, Value::Int(x), Value::Int(y)) => {
                if *y == 0 {
                    op_span.error("Division by zero.").err()
                } else {
                    // For division, the result may not be an integer. In that case,
                    // probably the right thing to do is to add rational numbers as
                    // values and make the result a rational. However, I don't want
                    // to implement all of that right now, so the conservative thing
                    // to do is to only allow division when it results in an integer.
                    // If we'd choose integer division now, it would be a subtle
                    // change of behavior later.
                    let q = x / y;
                    if q * y == *x {
                        Ok(Rc::new(Value::Int(q)))
                    } else {
                        let err = concat! {
                            "Non-integer division: "
                            x.to_string() " is not a multiple of " y.to_string()
                            ". Non-integer division is not supported at this time."
                        };
                        op_span.error(err).err()
                    }
                }
            }
            (BinOp::Lt, Value::Int(x), Value::Int(y)) => Ok(Rc::new(Value::Bool(*x < *y))),
            (BinOp::Gt, Value::Int(x), Value::Int(y)) => Ok(Rc::new(Value::Bool(*x > *y))),
            (BinOp::LtEq, Value::Int(x), Value::Int(y)) => Ok(Rc::new(Value::Bool(*x <= *y))),
            (BinOp::GtEq, Value::Int(x), Value::Int(y)) => Ok(Rc::new(Value::Bool(*x >= *y))),
            // TODO: Throw a type error when the types are not the same, instead of
            // enabling comparing values of different types.
            (BinOp::Eq, x, y) => Ok(Rc::new(Value::Bool(*x == *y))),
            (BinOp::Neq, x, y) => Ok(Rc::new(Value::Bool(*x != *y))),
            _ => {
                // TODO: Add a proper type error.
                Err(op_span
                    .error("This operator is not supported for these values.")
                    .into())
            }
        }
    }

    fn eval_stmt(&mut self, env: &mut Env, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Let { ident, value } => {
                // Note, this is not a recursive let, the variable is not bound when
                // we evaluate the expression.
                let v = self.eval_expr(env, value)?;
                env.push(ident.clone(), v);
            }
            Stmt::Assert {
                condition_span,
                condition,
                message: message_expr,
            } => {
                let v = self.eval_expr(env, condition)?;
                match *v {
                    Value::Bool(true) => {
                        // Ok, the assertion passed, nothing else to do.
                    }
                    Value::Bool(false) => {
                        let message = self.eval_expr(env, message_expr)?;
                        let body: Doc = match message.as_ref() {
                            // If the message is a string, then we include it directly,
                            // not pretty-printed as a value.
                            Value::String(msg) => Doc::lines(msg),
                            // Otherwise, we pretty-print it as an RCL value.
                            _ => format_rcl(&message),
                        };
                        return condition_span
                            .error("Assertion failed.")
                            .with_body(body.into_owned())
                            .err();
                    }
                    _ => {
                        // TODO: Report a proper type error.
                        return condition_span
                            .error("Assertion condition must evaluate to a boolean.")
                            .err();
                    }
                }
            }
            Stmt::Trace {
                message_span,
                message: message_expr,
            } => {
                let message = self.eval_expr(env, message_expr)?;
                self.tracer
                    .trace(&self.loader.as_inputs(), *message_span, message);
            }
        }
        Ok(())
    }

    fn eval_seq(&mut self, env: &mut Env, seq: &Seq, out: &mut SeqOut) -> Result<()> {
        // For a collection enclosed in {}, now that we have a concrete seq, that
        // determines whether this is a set comprehension or a dict comprehension.
        // We really have to look at the syntax of the seq, we cannot resolve the
        // ambiguity later when we get to evaluating the inner seq, because that
        // part may be skipped due to a conditional.
        if let SeqOut::SetOrDict = out {
            match seq.innermost() {
                Yield::Elem { span, .. } => *out = SeqOut::Set(*span, Vec::new()),
                Yield::Assoc { op_span, .. } => {
                    *out = SeqOut::Dict(*op_span, Vec::new(), Vec::new())
                }
            }
        }

        match seq {
            Seq::Yield(Yield::Elem {
                span,
                value: value_expr,
            }) => {
                let out_values = out.values(*span)?;
                let value = self.eval_expr(env, value_expr)?;
                out_values.push(value);
                Ok(())
            }
            Seq::Yield(Yield::Assoc {
                op_span,
                key: key_expr,
                value: value_expr,
            }) => {
                let (out_keys, out_values) = out.keys_values(*op_span)?;
                let key = self.eval_expr(env, key_expr)?;
                let value = self.eval_expr(env, value_expr)?;
                out_keys.push(key);
                out_values.push(value);
                Ok(())
            }
            Seq::For {
                idents_span,
                idents,
                collection_span,
                collection,
                body,
            } => {
                let collection_value = self.eval_expr(env, collection)?;
                match (&idents[..], collection_value.as_ref()) {
                    ([name], Value::List(xs)) => {
                        for x in xs {
                            let ck = env.push(name.clone(), x.clone());
                            self.eval_seq(env, body, out)?;
                            env.pop(ck);
                        }
                        Ok(())
                    }
                    (_names, Value::List(..)) => {
                        let err = idents_span.error("Expected a single variable.").with_note(
                            *collection_span,
                            "This is a list, it yields one element per iteration.",
                        );
                        Err(err.into())
                    }
                    ([name], Value::Set(xs)) => {
                        for x in xs {
                            let ck = env.push(name.clone(), x.clone());
                            self.eval_seq(env, body, out)?;
                            env.pop(ck);
                        }
                        Ok(())
                    }
                    (_names, Value::Set(..)) => {
                        let err = idents_span.error("Expected a single variable.").with_note(
                            *collection_span,
                            "This is a set, it yields one element per iteration.",
                        );
                        Err(err.into())
                    }
                    ([k_name, v_name], Value::Dict(xs)) => {
                        for (k, v) in xs {
                            let ck = env.checkpoint();
                            env.push(k_name.clone(), k.clone());
                            env.push(v_name.clone(), v.clone());
                            self.eval_seq(env, body, out)?;
                            env.pop(ck);
                        }
                        Ok(())
                    }
                    (_names, Value::Dict(..)) => {
                        let err = idents_span
                            .error("Expected two variables in dict iteration.")
                            .with_note(
                                *collection_span,
                                "This is a dict, it yields a key and value per iteration.",
                            );
                        Err(err.into())
                    }
                    // TODO: Add a proper type error.
                    _ => Err(collection_span.error("This is not iterable.").into()),
                }
            }
            Seq::If {
                condition_span,
                condition,
                body,
            } => {
                let cond = self.eval_expr(env, condition)?;
                match cond.as_ref() {
                    Value::Bool(true) => self.eval_seq(env, body, out),
                    Value::Bool(false) => Ok(()),
                    _ => {
                        // TODO: Make this a type error.
                        let err = condition_span.error("Condition should be a boolean.");
                        Err(err.into())
                    }
                }
            }
            Seq::Stmt { stmt, body } => {
                let ck = env.checkpoint();
                self.eval_stmt(env, stmt)?;
                self.eval_seq(env, body, out)?;
                env.pop(ck);
                Ok(())
            }
        }
    }
}

type Values = Vec<Rc<Value>>;

/// Helper to hold evaluation output for sequences.
enum SeqOut {
    /// Only allow scalar values because we are in a list literal.
    List(Values),

    /// The sequence is definitely a set, because the first element is scalar.
    ///
    /// The span contains the previous scalar as evidence.
    Set(Span, Values),

    /// The sequence is definitely a dict, because the first element is kv.
    ///
    /// The span contains the previous key-value as evidence.
    Dict(Span, Values, Values),

    /// It's still unclear whether this is a set or a dict.
    SetOrDict,
}

impl SeqOut {
    fn values(&mut self, scalar: Span) -> Result<&mut Values> {
        match self {
            SeqOut::List(values) => Ok(values),
            SeqOut::Set(_first, values) => Ok(values),
            SeqOut::Dict(first, _keys, _values) => {
                let err = scalar
                    .error("Expected key-value, not a scalar element.")
                    .with_note(
                        *first,
                        "The collection is a dict and not a set, because of this key-value.",
                    );
                Err(err.into())
            }
            SeqOut::SetOrDict => unreachable!("Should have been replaced by now."),
        }
    }

    fn keys_values(&mut self, kv: Span) -> Result<(&mut Values, &mut Values)> {
        match self {
            SeqOut::List(_values) => {
                let err = kv
                    .error("Expected scalar element, not key-value.")
                    .with_help(
                    "Key-value pairs are allowed in dicts, which are enclosed in '{}', not '[]'.",
                );
                Err(err.into())
            }
            SeqOut::Set(first, _values) => {
                let err = kv
                    .error("Expected scalar element, not key-value.")
                    .with_note(
                        *first,
                        "The collection is a set and not a dict, because of this scalar value.",
                    );
                Err(err.into())
            }
            SeqOut::Dict(_first, keys, values) => Ok((keys, values)),
            SeqOut::SetOrDict => unreachable!("Should have been replaced by now."),
        }
    }
}
