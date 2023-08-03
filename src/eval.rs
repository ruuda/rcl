use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast::{BinOp, Compr, Expr, Seq, UnOp};
use crate::runtime::{Builtin, Env, Value};

#[derive(Debug)]
pub struct Error {
    message: &'static str,
}

impl Error {
    // TODO: Add a better error type which records source span info etc.
    pub fn new(message: &'static str) -> Error {
        Error { message }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn eval(env: &mut Env, expr: &Expr) -> Result<Rc<Value>> {
    match expr {
        Expr::MapLit(seqs) => {
            let mut keys = Vec::new();
            let mut values = Vec::new();
            for seq in seqs {
                eval_seq(env, seq, &mut keys, &mut values)?;
            }
            match values.len() {
                // If we have a value for every key, this is a map.
                n if n == keys.len() => {
                    let mut result = BTreeMap::new();
                    for (k, v) in keys.into_iter().zip(values) {
                        result.insert(k, v);
                    }
                    Ok(Rc::new(Value::Map(result)))
                }
                // If we have no values, it’s a set.
                0 => {
                    let result = keys.into_iter().collect();
                    Ok(Rc::new(Value::Set(result)))
                }
                _ => {
                    Err(Error::new("Should not mix `k: v` and values in one comprehension."))
                }
            }
        }
        Expr::ListLit(seqs) => {
            let mut keys = Vec::new();
            let mut values = Vec::new();
            for seq in seqs {
                eval_seq(env, seq, &mut keys, &mut values)?;
            }

            if !values.is_empty() {
                return Err(Error::new("`k: v` can only be used inside {}, not []."))
            }

            Ok(Rc::new(Value::List(keys)))
        }

        Expr::StringLit(s) => {
            Ok(Rc::new(Value::String(s.clone())))
        }

        Expr::IfThenElse(if_, then, else_) => {
            let cond = eval(env, if_)?;
            match cond.as_ref() {
                Value::Bool(true) => eval(env, then),
                Value::Bool(false) => eval(env, else_),
                _ => Err(Error::new("Condition should be boolean."))
            }
        }

        Expr::Var(var) => {
            match env.lookup(var) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::new("Variable not found."))
            }
        }

        Expr::Field(field_name, value_expr) => {
            let value = eval(env, value_expr)?;
            let field_name_value = Value::String(field_name.to_string());
            match value.as_ref() {
                Value::Map(fields) => {
                    // First test for the builtin names, they shadow the values,
                    // if there are any values.
                    let builtin = match *field_name {
                        "contains" => Some(builtin_map_contains(value.clone())),
                        _ => None,
                    };
                    if let Some(b) = builtin {
                        return Ok(Rc::new(Value::Builtin(b)));
                    }
                    // If it wasn't a builtin, look for a key in the map.
                    match fields.get(&field_name_value) {
                        Some(v) => Ok(v.clone()),
                        None => {
                            println!("Trying to access {} on:\n{:#?}", field_name, fields);
                            Err(Error::new("No such field in this value."))
                        }
                    }
                }
                Value::List(..) => {
                    let builtin = match *field_name {
                        "contains" => Some(builtin_list_contains(value.clone())),
                        _ => None,
                    };
                    match builtin {
                        Some(b) => Ok(Rc::new(Value::Builtin(b))),
                        None => Err(Error::new("No such field in this list.")),
                    }
                }
                not_map => {
                    println!("Trying to access {} on:\n{:#?}", field_name, not_map);
                    Err(Error::new("Can only do field access on maps for now."))
                },
            }
        }

        Expr::Let(var, value_expr, cont) => {
            // Note, this is not a recursive let, the variable is not bound when
            // we evaluate the expression.
            let value = eval(env, value_expr)?;
            env.push(var, value);
            let result = eval(env, cont)?;
            env.pop();
            Ok(result)
        }

        Expr::Call(fun_expr, args_exprs) => {
            // We do strict evaluation, all arguments get evaluated before we go
            // into the call.
            let fun = eval(env, fun_expr)?;
            let args = args_exprs.iter().map(|a| eval(env, a)).collect::<Result<Vec<_>>>()?;

            match fun.as_ref() {
                Value::Builtin(f) => (f.f)(&args[..]),
                // TODO: Define a value for lambdas, implement the call.
                _ => Err(Error::new("Can only call functions.")),
            }
        }

        Expr::Lam(_args, _body) => unimplemented!("TODO: Define lambdas."),

        Expr::UnOp(op, value_expr) => {
            let value = eval(env, value_expr)?;
            eval_unop(*op, value)
        }

        Expr::BinOp(op, lhs_expr, rhs_expr) => {
            let lhs = eval(env, lhs_expr)?;
            let rhs = eval(env, rhs_expr)?;
            eval_binop(*op, lhs, rhs)
        }
    }
}

fn eval_unop(op: UnOp, v: Rc<Value>) -> Result<Rc<Value>> {
    match (op, v.as_ref()) {
        (UnOp::Neg, Value::Bool(x)) => Ok(Rc::new(Value::Bool(!x))),
        _ => Err(Error::new("The operator is not supported for this value.")),
    }
}

fn eval_binop(op: BinOp, lhs: Rc<Value>, rhs: Rc<Value>) -> Result<Rc<Value>> {
    match (op, lhs.as_ref(), rhs.as_ref()) {
        (BinOp::Union, Value::Map(_xs), Value::Map(_ys)) => {
            unimplemented!("TODO: Implement map union.")
        }
        (BinOp::Union, Value::Set(_xs), Value::Set(_ys)) => {
            unimplemented!("TODO: Implement set union.")
        }
        (BinOp::Add, Value::Int(x), Value::Int(y)) => {
            // TODO: Make this a checked add.
            Ok(Rc::new(Value::Int(x + y)))
        }
        _ => Err(Error::new("The operator is not supported for this value.")),
    }
}

fn eval_seq(
    env: &mut Env,
    seq: &Seq,
    out_keys: &mut Vec<Rc<Value>>,
    out_values: &mut Vec<Rc<Value>>,
) -> Result<()> {
    match seq {
        Seq::Elem(elem_expr) => {
            let value = eval(env, elem_expr)?;
            out_keys.push(value);
            Ok(())
        }
        Seq::Assoc(key_expr, value_expr) => {
            let key = eval(env, key_expr)?;
            let value = eval(env, value_expr)?;
            out_keys.push(key);
            out_values.push(value);
            Ok(())
        }
        Seq::Compr(Compr::For { collection, elements, body }) => {
            let collection_value = eval(env, collection)?;
            match (&elements[..], collection_value.as_ref()) {
                (&[name], Value::List(xs)) => {
                    for x in xs {
                        env.push(name, x.clone());
                        eval_seq(env, body, out_keys, out_values)?;
                        env.pop();
                    }
                    Ok(())
                }
                (&[name], Value::Set(xs)) => {
                    for x in xs {
                        env.push(name, x.clone());
                        eval_seq(env, body, out_keys, out_values)?;
                        env.pop();
                    }
                    Ok(())
                }
                (&[k_name, v_name], Value::Map(xs)) => {
                    for (k, v) in xs {
                        env.push(k_name, k.clone());
                        env.push(v_name, v.clone());
                        eval_seq(env, body, out_keys, out_values)?;
                        env.pop();
                        env.pop();
                    }
                    Ok(())
                }
                _ => {
                    Err(Error::new("Iteration is not supported like this."))
                }
            }
        }
        Seq::Compr(Compr::If { condition, body }) => {
            let cond = eval(env, condition)?;
            match cond.as_ref() {
                Value::Bool(true) => eval_seq(env, body, out_keys, out_values),
                Value::Bool(false) => Ok(()),
                _ => Err(Error::new("Comprehension condition should be boolean."))
            }
        }
        Seq::Compr(Compr::Let { name, value, body }) => {
            let v = eval(env, value)?;
            env.push(name, v);
            eval_seq(env, body, out_keys, out_values)?;
            env.pop();
            Ok(())
        }
    }
}

fn builtin_map_contains(v: Rc<Value>) -> Builtin {
    let f = move |args: &[Rc<Value>]| {
        let arg = match args {
            [a] => a,
            _ => return Err(Error::new("Map.contains takes a single argument.")),
        };
        match v.as_ref() {
            Value::Map(m) => {
                let contains = m.contains_key(arg);
                Ok(Rc::new(Value::Bool(contains)))
            }
            _not_map => panic!("Should not have made a Map.contains for this value."),
        }
    };
    Builtin {
        name: "Map.contains",
        f: Box::new(f),
    }
}

fn builtin_list_contains(v: Rc<Value>) -> Builtin {
    let f = move |args: &[Rc<Value>]| {
        let arg = match args {
            [a] => a,
            _ => return Err(Error::new("List.contains takes a single argument.")),
        };
        match v.as_ref() {
            Value::List(m) => {
                let contains = m.contains(arg);
                Ok(Rc::new(Value::Bool(contains)))
            }
            _not_map => panic!("Should not have made a List.contains for this value."),
        }
    };
    Builtin {
        name: "List.contains",
        f: Box::new(f),
    }
}
