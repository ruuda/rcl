// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Representations of values and scopes at runtime.

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::{Expr, Ident};
use crate::error::{IntoError, Result};
use crate::eval::Evaluator;
use crate::pprint::{concat, Doc};
use crate::source::Span;
use crate::types::Type;

/// A value provided as argument to a function call.
pub struct CallArg {
    pub span: Span,
    pub value: Value,
}

/// The arguments to a function call at runtime.
pub struct FunctionCall<'a> {
    /// The opening paren for the call.
    pub call_open: Span,

    /// The closing paren for the call.
    pub call_close: Span,

    /// The arguments and their spans in the source code.
    pub args: &'a [CallArg],
}

impl<'a> FunctionCall<'a> {
    /// Return an error if the number of arguments is unexpected.
    pub fn check_arity_static(
        &self,
        name: &'static str,
        expected_args: &[&'static str],
    ) -> Result<()> {
        if self.args.len() == expected_args.len() {
            return Ok(());
        }

        if self.args.len() < expected_args.len() {
            let missing_arg = &expected_args[self.args.len()];
            let msg = concat! {
                "Missing argument '"
                Doc::highlight(missing_arg)
                "'. '"
                Doc::highlight(name)
                "' takes "
                match expected_args.len() {
                    1 => "1 argument".to_string(),
                    n => format!("{n} arguments"),
                }
                ", but got "
                self.args.len().to_string()
                "."
            };
            self.call_close.error(msg).err()
        } else {
            let excess_arg = &self.args[expected_args.len()];
            let msg = concat! {
                "Unexpected argument. '"
                Doc::highlight(name)
                "' takes "
                match expected_args.len() {
                    1 => "1 argument".to_string(),
                    n => format!("{n} arguments"),
                }
                ", but got "
                self.args.len().to_string()
                "."
            };
            excess_arg.span.error(msg).err()
        }
    }

    /// As `check_arity`, but for user-defined functions (lambdas).
    pub fn check_arity_dynamic(&self, expected_args: &[Ident]) -> Result<()> {
        if self.args.len() == expected_args.len() {
            return Ok(());
        }

        if self.args.len() < expected_args.len() {
            let missing_arg = &expected_args[self.args.len()];
            let msg = concat! {
                "Missing argument '"
                Doc::highlight(missing_arg.as_ref()).into_owned()
                "'. The function takes "
                match expected_args.len() {
                    1 => "1 argument".to_string(),
                    n => format!("{n} arguments"),
                }
                ", but got "
                self.args.len().to_string()
                "."
            };
            self.call_close.error(msg).err()
        } else {
            let excess_arg = &self.args[expected_args.len()];
            let msg = concat! {
                "Unexpected argument. The function takes "
                match expected_args.len() {
                    1 => "1 argument".to_string(),
                    n => format!("{n} arguments"),
                }
                ", but got "
                self.args.len().to_string()
                "."
            };
            excess_arg.span.error(msg).err()
        }
    }
}

/// The arguments to a method call at runtime.
pub struct MethodCall<'a> {
    /// The source code span of the receiver of the method call.
    ///
    /// In `widget.len()`, the receiver is `widget`.
    pub receiver_span: Span,

    /// The receiver of the call.
    pub receiver: &'a Value,

    /// The span of the method being called.
    ///
    /// In `widget.len()`, the method is `len`.
    pub method_span: Span,

    /// Arguments to the call.
    pub call: FunctionCall<'a>,
}

/// A built-in function.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub struct BuiltinFunction {
    pub name: &'static str,
    pub f: for<'a> fn(&'a mut Evaluator, FunctionCall<'a>) -> Result<Value>,
}

/// A built-in method.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub struct BuiltinMethod {
    pub name: &'static str,
    pub f: for<'a> fn(&'a mut Evaluator, MethodCall<'a>) -> Result<Value>,
}

/// A method and its receiver.
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct MethodInstance {
    /// The span where we refer to the method, e.g. the `keys` in `{}.keys()`.
    pub method_span: Span,
    /// The method to be called.
    pub method: &'static BuiltinMethod,
    /// The span of the receiving expression, e.g. the `{}` in `{}.keys()`.
    pub receiver_span: Span,
    /// The receiver of the call.
    pub receiver: Value,
}

impl std::fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}@{:p}", self.name, self.f)
    }
}

impl std::fmt::Debug for BuiltinMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}@{:p}", self.name, self.f)
    }
}

#[derive(Debug)]
pub struct Function {
    /// Source location of the `=>` that introduces this lambda function.
    ///
    /// This span is used to identify the function for comparison and equality,
    /// so we don't have to inspect its AST.
    pub span: Span,

    /// Captured environment at the time of the call.
    ///
    /// TODO: It might be nicer to capture only the variables that are needed,
    /// but then we need to inspect the body AST when the lambda is produced.
    pub env: Env,
    pub args: Vec<Ident>,
    pub body: Rc<Expr>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        // What matters for the identity of the lambda is where in the source
        // code it was produced. If that is the same, then the args and body are
        // necessarily the same. But the captured environment could be different,
        // so we take that into account too.
        (self.span, &self.env) == (other.span, &other.env)
    }
}

impl Eq for Function {}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Function) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Function {
    fn cmp(&self, other: &Function) -> Ordering {
        let lhs = (self.span, &self.env);
        let rhs = (other.span, &other.env);
        lhs.cmp(&rhs)
    }
}

/// A value.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Value {
    Null,

    Bool(bool),

    // TODO: Should be a bigint.
    Int(i64),

    String(Rc<str>),

    List(Rc<Vec<Value>>),

    // TODO: Should preserve insertion order.
    Set(Rc<BTreeSet<Value>>),

    // TODO: Should preserve insertion order.
    Dict(Rc<BTreeMap<Value, Value>>),

    Function(Rc<Function>),

    BuiltinFunction(&'static BuiltinFunction),

    BuiltinMethod(Rc<MethodInstance>),
}

impl Value {
    /// Extract the dict if it is one, panic otherwise.
    #[inline]
    pub fn expect_dict(&self) -> &BTreeMap<Value, Value> {
        match self {
            Value::Dict(inner) => inner,
            other => panic!("Expected Dict but got {other:?}."),
        }
    }

    /// Extract the list if it is one, panic otherwise.
    #[inline]
    pub fn expect_list(&self) -> &[Value] {
        match self {
            Value::List(inner) => inner.as_ref(),
            other => panic!("Expected List but got {other:?}."),
        }
    }

    /// Extract the list if it is one, panic otherwise.
    #[inline]
    pub fn expect_set(&self) -> &BTreeSet<Value> {
        match self {
            Value::Set(inner) => inner,
            other => panic!("Expected Set but got {other:?}."),
        }
    }

    /// Extract the string if it is one, panic otherwise.
    #[inline]
    pub fn expect_string(&self) -> &str {
        match self {
            Value::String(inner) => inner.as_ref(),
            other => panic!("Expected String but got {other:?}."),
        }
    }
}

impl<'a> From<&'a str> for Value {
    #[inline]
    fn from(value: &'a str) -> Self {
        Value::String(value.into())
    }
}

/// An environment binds names to values.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Env {
    value_bindings: Vec<(Ident, Value)>,
    type_bindings: Vec<(Ident, Rc<Type>)>,
}

/// References a version of an environment that we can later restore to.
#[derive(Copy, Clone)]
pub struct EnvCheckpoint {
    values_len: usize,
    types_len: usize,
}

impl Env {
    /// Create a new empty environment.
    pub fn new() -> Env {
        Env {
            value_bindings: Vec::new(),
            type_bindings: Vec::new(),
        }
    }

    /// Create a new environment with an initialized standard library.
    pub fn with_prelude() -> Env {
        let mut env = Env::new();
        env.push_value("std".into(), crate::stdlib::initialize());

        // The primitive types are in scope by default.
        env.push_type("Bool".into(), Rc::new(Type::Bool));
        env.push_type("Int".into(), Rc::new(Type::Int));
        env.push_type("Null".into(), Rc::new(Type::Null));
        env.push_type("String".into(), Rc::new(Type::String));

        // TODO: What to do about Dict, List, and Set? They are technically type
        // constructors. Should those exist, at this level, if they can't be
        // user-defined? It's easier to implement if we just hard-code those few,
        // but then if you write `let xs: List = [1, 2, 3]`, it will lead to a
        // confusing error.

        env
    }

    pub fn lookup_value(&self, name: &Ident) -> Option<&Value> {
        self.value_bindings
            .iter()
            .rev()
            .find(|(k, _v)| k == name)
            .map(|(_k, v)| v)
    }

    pub fn lookup_type(&self, name: &Ident) -> Option<&Rc<Type>> {
        self.type_bindings
            .iter()
            .rev()
            .find(|(k, _v)| k == name)
            .map(|(_k, v)| v)
    }

    /// Return a checkpoint of the environment to later [`Env::pop`] to.
    ///
    /// Note, the environment is a stack and the pushes and pops have to be
    /// balanced; popping can only remove bindings from the environment again,
    /// it does not _restore_ the environment to that state, like something
    /// transactional would, or a persistent data structure.
    pub fn checkpoint(&self) -> EnvCheckpoint {
        EnvCheckpoint {
            values_len: self.value_bindings.len(),
            types_len: self.type_bindings.len(),
        }
    }

    /// Push a value binding into the environment.
    ///
    /// If the name already existed, the new push will shadow the old one.
    ///
    /// Returns a checkpoint of the environment before the push.
    pub fn push_value(&mut self, name: Ident, value: Value) -> EnvCheckpoint {
        let checkpoint = self.checkpoint();
        self.value_bindings.push((name, value));
        checkpoint
    }

    /// Push a type binding into the environment.
    ///
    /// If the name already existed, the new push will shadow the old one.
    ///
    /// Returns a checkpoint of the environment before the push.
    pub fn push_type(&mut self, name: Ident, type_: Rc<Type>) -> EnvCheckpoint {
        let checkpoint = self.checkpoint();
        self.type_bindings.push((name, type_));
        checkpoint
    }

    /// Pop bindings to get back to a previous version of the environment.
    pub fn pop(&mut self, to: EnvCheckpoint) {
        let EnvCheckpoint {
            values_len,
            types_len,
        } = to;
        debug_assert!(
            self.value_bindings.len() >= values_len,
            "Cannot restore to checkpoint, more got popped already.",
        );
        debug_assert!(
            self.type_bindings.len() >= types_len,
            "Cannot restore to checkpoint, more got popped already.",
        );
        self.value_bindings.truncate(values_len);
        self.type_bindings.truncate(types_len);
    }
}

macro_rules! builtin_function {
    (
        $rcl_name:expr,
        const $rust_const:ident,
        $rust_name:ident
    ) => {
        pub const $rust_const: crate::runtime::BuiltinFunction = crate::runtime::BuiltinFunction {
            name: $rcl_name,
            f: $rust_name,
        };
    };
}
pub(crate) use builtin_function;

macro_rules! builtin_method {
    (
        $rcl_name:expr,
        const $rust_const:ident,
        $rust_name:ident
    ) => {
        pub const $rust_const: crate::runtime::BuiltinMethod = crate::runtime::BuiltinMethod {
            name: $rcl_name,
            f: $rust_name,
        };
    };
}
pub(crate) use builtin_method;
