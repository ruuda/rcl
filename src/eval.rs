// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Evaluation turns ASTs into values.

use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::{BinOp, Expr, FormatFragment, Seq, Stmt, UnOp, Yield};
use crate::error::{IntoRuntimeError, Result};
use crate::runtime::{Builtin, Env, Value};
use crate::source::Span;

pub fn eval(env: &mut Env, expr: &Expr) -> Result<Rc<Value>> {
    match expr {
        Expr::BraceLit(seqs) => {
            let mut out = SeqOut::SetOrDict;
            for seq in seqs {
                eval_seq(env, seq, &mut out)?;
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
                eval_seq(env, seq, &mut out)?;
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

        Expr::Format(fragments) => eval_format(env, fragments),

        Expr::IfThenElse {
            condition_span,
            condition,
            body_then,
            body_else,
        } => {
            let cond = eval(env, condition)?;
            match cond.as_ref() {
                Value::Bool(true) => eval(env, body_then),
                Value::Bool(false) => eval(env, body_else),
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
            let inner = eval(env, inner_expr)?;
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
                Value::List(..) => {
                    let builtin = match field_name.as_ref() {
                        "contains" => Some(builtin_list_contains(inner.clone())),
                        _ => None,
                    };
                    match builtin {
                        Some(b) => Ok(Rc::new(Value::Builtin(b))),
                        None => Err(err_unknown_field.into()),
                    }
                }
                Value::Set(s) => {
                    let builtin = match field_name.as_ref() {
                        "contains" => Some(builtin_set_contains(inner.clone())),
                        "len" => Some(builtin_set_len(s)),
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
            eval_stmt(env, stmt)?;
            let result = eval(env, body)?;
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
            let fun = eval(env, fun_expr)?;
            let args = args_exprs
                .iter()
                .map(|a| eval(env, a))
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
            let value = eval(env, value_expr)?;
            eval_unop(*op, *op_span, value)
        }

        Expr::BinOp {
            op,
            op_span,
            lhs: lhs_expr,
            rhs: rhs_expr,
        } => {
            let lhs = eval(env, lhs_expr)?;
            let rhs = eval(env, rhs_expr)?;
            eval_binop(*op, *op_span, lhs, rhs)
        }
    }
}

/// Evaluate a format string.
pub fn eval_format(env: &mut Env, fragments: &[FormatFragment]) -> Result<Rc<Value>> {
    let mut results: Vec<Rc<str>> = Vec::new();

    for fragment in fragments {
        let result = eval(env, &fragment.body)?;
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

fn eval_unop(op: UnOp, op_span: Span, v: Rc<Value>) -> Result<Rc<Value>> {
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

fn eval_binop(op: BinOp, op_span: Span, lhs: Rc<Value>, rhs: Rc<Value>) -> Result<Rc<Value>> {
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

type Values = Vec<Rc<Value>>;

/// Evaluation output for sequences.
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

fn eval_stmt(env: &mut Env, stmt: &Stmt) -> Result<()> {
    match stmt {
        Stmt::Let { ident, value } => {
            // Note, this is not a recursive let, the variable is not bound when
            // we evaluate the expression.
            let v = eval(env, value)?;
            env.push(ident.clone(), v);
        }
        Stmt::Assert {
            assert_span,
            condition,
            message: message_expr,
        } => {
            let v = eval(env, condition)?;
            match *v {
                Value::Bool(true) => {
                    // Ok, the assertion passed, nothing else to do.
                }
                Value::Bool(false) => {
                    let message = eval(env, message_expr)?;
                    // TODO: Include the message in the error. We need a way to
                    // attach values in addition to source locations ...
                    #[cfg(not(fuzzing))]
                    eprintln!("Assertion failed: {message:?}");
                    #[cfg(fuzzing)]
                    let _ = message;
                    return Err(assert_span.error("Assertion failed.").into());
                }
                _ => {
                    // TODO: Report a proper type error.
                    let err = assert_span.error("Assertion condition must evaluate to a boolean.");
                    return Err(err.into());
                }
            }
        }
        Stmt::Trace {
            trace_span,
            message: message_expr,
        } => {
            // TODO: Implement proper reporting, format in the same way as
            // errors, pretty-print the value, ...
            let message = eval(env, message_expr)?;
            #[cfg(not(fuzzing))]
            eprintln!("Trace from {trace_span:?}: {message:?}");
            #[cfg(fuzzing)]
            let _ = (message, trace_span);
        }
    }
    Ok(())
}

fn eval_seq(env: &mut Env, seq: &Seq, out: &mut SeqOut) -> Result<()> {
    // For a collection enclosed in {}, now that we have a concrete seq, that
    // determines whether this is a set comprehension or a dict comprehension.
    // We really have to look at the syntax of the seq, we cannot resolve the
    // ambiguity later when we get to evaluating the inner seq, because that
    // part may be skipped due to a conditional.
    if let SeqOut::SetOrDict = out {
        match seq.innermost() {
            Yield::Elem { span, .. } => *out = SeqOut::Set(*span, Vec::new()),
            Yield::Assoc { op_span, .. } => *out = SeqOut::Dict(*op_span, Vec::new(), Vec::new()),
        }
    }

    match seq {
        Seq::Yield(Yield::Elem {
            span,
            value: value_expr,
        }) => {
            let out_values = out.values(*span)?;
            let value = eval(env, value_expr)?;
            out_values.push(value);
            Ok(())
        }
        Seq::Yield(Yield::Assoc {
            op_span,
            key: key_expr,
            value: value_expr,
        }) => {
            let (out_keys, out_values) = out.keys_values(*op_span)?;
            let key = eval(env, key_expr)?;
            let value = eval(env, value_expr)?;
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
            let collection_value = eval(env, collection)?;
            match (&idents[..], collection_value.as_ref()) {
                ([name], Value::List(xs)) => {
                    for x in xs {
                        let ck = env.push(name.clone(), x.clone());
                        eval_seq(env, body, out)?;
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
                        eval_seq(env, body, out)?;
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
                        eval_seq(env, body, out)?;
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
            let cond = eval(env, condition)?;
            match cond.as_ref() {
                Value::Bool(true) => eval_seq(env, body, out),
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
            eval_stmt(env, stmt)?;
            eval_seq(env, body, out)?;
            env.pop(ck);
            Ok(())
        }
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
            return Err(span.error("String.len takes no arguments.").into());
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
            _ => return Err(span.error("Dict.contains takes a single argument.").into()),
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
            _ => return Err(span.error("List.contains takes a single argument.").into()),
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
            _ => return Err(span.error("Set.contains takes a single argument.").into()),
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
            _ => return Err(span.error("Dict.get takes two arguments.").into()),
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
