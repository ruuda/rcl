use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::{BinOp, Expr, UnOp};
use crate::runtime::{Env, Value};

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
        Expr::MapLit(_seqs) => unimplemented!("TODO: Map literals"),
        Expr::ListLit(_seqs) => unimplemented!("TODO: List literals"),

        Expr::StringLit(s) => {
            Ok(Rc::new(Value::String(s.clone())))
        }

        Expr::Compr(s) => unimplemented!("TODO: Comprehensions"),

        Expr::Assoc(k, v) => unimplemented!("TODO: Implement assoc"),

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
                    match fields.get(&field_name_value) {
                        Some(v) => Ok(v.clone()),
                        None => Err(Error::new("No such field in this value.")),
                    }
                }
                _not_map => Err(Error::new("Can only do field access on maps for now.")),
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
                Value::Builtin(f) => (f.f)(env, &args[..]),
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
