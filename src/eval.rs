// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Evaluation turns ASTs into values.

use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::{BinOp, Expr, FormatFragment, Seq, Stmt, UnOp, Yield};
use crate::error::{Error, IntoError, Result};
use crate::fmt_rcl::format_rcl;
use crate::loader::Loader;
use crate::pprint::Doc;
use crate::runtime::{Builtin, Env, Value};
use crate::source::{DocId, Span};
use crate::tracer::Tracer;

/// An entry on the evaluation stack.
pub struct EvalContext {
    /// The document that we are evaluating.
    doc: DocId,
    /// The source that caused evaluation of this document.
    imported_from: Option<Span>,
}

pub struct Evaluator<'a> {
    loader: &'a mut Loader,
    tracer: &'a mut dyn Tracer,
    import_stack: Vec<EvalContext>,
}

impl<'a> Evaluator<'a> {
    pub fn new(loader: &'a mut Loader, tracer: &'a mut dyn Tracer) -> Evaluator<'a> {
        Evaluator {
            loader,
            tracer,
            import_stack: Vec::new(),
        }
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
    fn eval_import(&mut self, env: &mut Env, doc: DocId, imported_from: Span) -> Result<Rc<Value>> {
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
        self.import_stack.push(ctx);
        let result = self.eval_expr(env, &expr)?;
        self.import_stack.pop().expect("Push/pop are balanced.");
        Ok(result)
    }

    fn eval_expr(&mut self, env: &mut Env, expr: &Expr) -> Result<Rc<Value>> {
        match expr {
            Expr::Import {
                import_span,
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
                self.eval_import(env, doc, *import_span)
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
                let cond = self.eval_expr(env, condition)?;
                match cond.as_ref() {
                    Value::Bool(true) => self.eval_expr(env, body_then),
                    Value::Bool(false) => self.eval_expr(env, body_else),
                    _ => {
                        // TODO: Make this a type error.
                        let err = condition_span.error("Condition should be a boolean.");
                        Err(err.into())
                    }
                }
            }

            Expr::Var { span, ident } => match env.lookup(ident) {
                Some(value) => Ok(value.clone()),
                None => Err(span.error("Unknown variable.").into()),
            },

            Expr::Field {
                field_span,
                field: field_name,
                inner: inner_expr,
            } => {
                let inner = self.eval_expr(env, inner_expr)?;
                let field_name_value = Value::String(field_name.0.clone());
                let err_unknown_field = field_span.error("Unknown field.");
                match inner.as_ref() {
                    Value::String(s) => {
                        let builtin = match field_name.as_ref() {
                            "len" => Some(builtin_string_len(s)),
                            _ => None,
                        };
                        match builtin {
                            Some(b) => Ok(Rc::new(Value::Builtin(b))),
                            None => Err(err_unknown_field.into()),
                        }
                    }
                    Value::Dict(fields) => {
                        // First test for the builtin names, they shadow the values,
                        // if there are any values.
                        let builtin = match field_name.as_ref() {
                            "contains" => Some(builtin_dict_contains(inner.clone())),
                            "get" => Some(builtin_dict_get(inner.clone())),
                            "len" => Some(builtin_dict_len(fields)),
                            _ => None,
                        };
                        if let Some(b) = builtin {
                            return Ok(Rc::new(Value::Builtin(b)));
                        }
                        // If it wasn't a builtin, look for a key in the dict.
                        match fields.get(&field_name_value) {
                            Some(v) => Ok(v.clone()),
                            None => Err(err_unknown_field.into()),
                        }
                    }
                    Value::List(xs) => {
                        let builtin = match field_name.as_ref() {
                            "contains" => Some(builtin_list_contains(inner.clone())),
                            "len" => Some(builtin_list_len(xs)),
                            _ => None,
                        };
                        match builtin {
                            Some(b) => Ok(Rc::new(Value::Builtin(b))),
                            None => Err(err_unknown_field.into()),
                        }
                    }
                    Value::Set(xs) => {
                        let builtin = match field_name.as_ref() {
                            "contains" => Some(builtin_set_contains(inner.clone())),
                            "len" => Some(builtin_set_len(xs)),
                            _ => None,
                        };
                        match builtin {
                            Some(b) => Ok(Rc::new(Value::Builtin(b))),
                            None => Err(err_unknown_field.into()),
                        }
                    }
                    _other => Err(err_unknown_field.into()),
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
                function_span,
                function: fun_expr,
                args: args_exprs,
            } => {
                // We do strict evaluation, all arguments get evaluated before we go
                // into the call.
                let fun = self.eval_expr(env, fun_expr)?;
                let args = args_exprs
                    .iter()
                    .map(|a| self.eval_expr(env, a))
                    .collect::<Result<Vec<_>>>()?;

                match fun.as_ref() {
                    Value::Builtin(f) => (f.f)(*open, &args[..]),
                    // TODO: Define a value for lambdas, implement the call.
                    // TODO: Add a proper type error.
                    _ => Err(function_span
                        .error("This is not a function, it cannot be called.")
                        .into()),
                }
            }

            Expr::Lam(_args, _body) => unimplemented!("TODO: Define lambdas."),

            Expr::UnOp {
                op,
                op_span,
                body: value_expr,
            } => {
                let value = self.eval_expr(env, value_expr)?;
                self.eval_unop(*op, *op_span, value)
            }

            Expr::BinOp {
                op,
                op_span,
                lhs: lhs_expr,
                rhs: rhs_expr,
            } => {
                let lhs = self.eval_expr(env, lhs_expr)?;
                let rhs = self.eval_expr(env, rhs_expr)?;
                self.eval_binop(*op, *op_span, lhs, rhs)
            }
        }
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

    fn eval_unop(&mut self, op: UnOp, op_span: Span, v: Rc<Value>) -> Result<Rc<Value>> {
        match (op, v.as_ref()) {
            (UnOp::Neg, Value::Bool(x)) => Ok(Rc::new(Value::Bool(!x))),
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
            (BinOp::Add, Value::Int(x), Value::Int(y)) => {
                match x.checked_add(*y) {
                    Some(z) => Ok(Rc::new(Value::Int(z))),
                    // TODO: Also include the values themselves through pretty-printer.
                    None => Err(op_span.error("Addition would overflow.").into()),
                }
            }
            (BinOp::Mul, Value::Int(x), Value::Int(y)) => {
                match x.checked_mul(*y) {
                    Some(z) => Ok(Rc::new(Value::Int(z))),
                    // TODO: Also include the values themselves through pretty-printer.
                    None => Err(op_span.error("Multiplication would overflow.").into()),
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
                trace_span,
                message: message_expr,
            } => {
                let message = self.eval_expr(env, message_expr)?;
                self.tracer
                    .trace(&self.loader.as_inputs(), *trace_span, message);
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

fn builtin_dict_len(s: &BTreeMap<Rc<Value>, Rc<Value>>) -> Builtin {
    let n = Rc::new(Value::Int(s.len() as _));
    let f = move |span: Span, args: &[Rc<Value>]| {
        if !args.is_empty() {
            return span.error("Dict.len takes no arguments.").err();
        };
        Ok(n.clone())
    };
    Builtin {
        name: "Dict.len",
        f: Box::new(f),
    }
}

fn builtin_list_len(s: &[Rc<Value>]) -> Builtin {
    let n = Rc::new(Value::Int(s.len() as _));
    let f = move |span: Span, args: &[Rc<Value>]| {
        if !args.is_empty() {
            return span.error("List.len takes no arguments.").err();
        };
        Ok(n.clone())
    };
    Builtin {
        name: "List.len",
        f: Box::new(f),
    }
}

fn builtin_set_len(s: &BTreeSet<Rc<Value>>) -> Builtin {
    let n = Rc::new(Value::Int(s.len() as _));
    let f = move |span: Span, args: &[Rc<Value>]| {
        if !args.is_empty() {
            return Err(span.error("Set.len takes no arguments.").into());
        };
        Ok(n.clone())
    };
    Builtin {
        name: "Set.len",
        f: Box::new(f),
    }
}

fn builtin_string_len(s: &str) -> Builtin {
    let n = Rc::new(Value::Int(s.len() as _));
    let f = move |span: Span, args: &[Rc<Value>]| {
        if !args.is_empty() {
            return span.error("String.len takes no arguments.").err();
        };
        Ok(n.clone())
    };
    Builtin {
        name: "String.len",
        f: Box::new(f),
    }
}

fn builtin_dict_contains(v: Rc<Value>) -> Builtin {
    let f = move |span: Span, args: &[Rc<Value>]| {
        let arg = match args {
            [a] => a,
            _ => return span.error("Dict.contains takes a single argument.").err(),
        };
        match v.as_ref() {
            Value::Dict(m) => {
                let contains = m.contains_key(arg);
                Ok(Rc::new(Value::Bool(contains)))
            }
            _not_dict => panic!("Should not have made a Dict.contains for this value."),
        }
    };
    Builtin {
        name: "Dict.contains",
        f: Box::new(f),
    }
}

fn builtin_list_contains(v: Rc<Value>) -> Builtin {
    let f = move |span: Span, args: &[Rc<Value>]| {
        let arg = match args {
            [a] => a,
            _ => return span.error("List.contains takes a single argument.").err(),
        };
        match v.as_ref() {
            Value::List(m) => {
                let contains = m.contains(arg);
                Ok(Rc::new(Value::Bool(contains)))
            }
            _not_list => panic!("Should not have made a List.contains for this value."),
        }
    };
    Builtin {
        name: "List.contains",
        f: Box::new(f),
    }
}

fn builtin_set_contains(v: Rc<Value>) -> Builtin {
    let f = move |span: Span, args: &[Rc<Value>]| {
        let arg = match args {
            [a] => a,
            _ => return span.error("Set.contains takes a single argument.").err(),
        };
        match v.as_ref() {
            Value::Set(m) => {
                let contains = m.contains(arg);
                Ok(Rc::new(Value::Bool(contains)))
            }
            _not_set => panic!("Should not have made a Set.contains for this value."),
        }
    };
    Builtin {
        name: "Set.contains",
        f: Box::new(f),
    }
}

fn builtin_dict_get(v: Rc<Value>) -> Builtin {
    let f = move |span: Span, args: &[Rc<Value>]| {
        let (k, default) = match args {
            [k, default] => (k, default),
            _ => return span.error("Dict.get takes two arguments.").err(),
        };
        match v.as_ref() {
            Value::Dict(m) => match m.get(k) {
                Some(v) => Ok(v.clone()),
                None => Ok(default.clone()),
            },
            _not_dict => panic!("Should not have made a Dict.get for this value."),
        }
    };
    Builtin {
        name: "Dict.get",
        f: Box::new(f),
    }
}
