// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Representations of values and scopes at runtime.

use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::Ident;
use crate::error::{IntoError, Result};
use crate::eval::Evaluator;
use crate::pprint::{concat, Doc};
use crate::source::Span;

/// The arguments to a function call at runtime.
pub struct FunctionCall<'a> {
    /// The opening paren for the call.
    pub call_open: Span,

    /// The closing paren for the call.
    pub call_close: Span,

    /// The arguments and their spans in the source code.
    pub args: &'a [(Span, Rc<Value>)],
}

impl<'a> FunctionCall<'a> {
    /// Return an error if the number of arguments is unexpected.
    pub fn check_arity(&self, name: &'static str, expected_args: &[&'static str]) -> Result<()> {
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
            excess_arg.0.error(msg).err()
        }
    }
}

/// The arguments to a method call at runtime.
pub struct MethodCall<'a> {
    /// The source code span of the receiver of the method call.
    pub receiver_span: Span,

    /// The receiver of the call.
    pub receiver: &'a Value,

    /// Arguments to the call.
    pub call: FunctionCall<'a>,
}

/// A built-in function.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub struct BuiltinFunction {
    pub name: &'static str,
    #[allow(clippy::type_complexity)]
    pub f: for<'a> fn(&'a mut Evaluator, FunctionCall<'a>) -> Result<Rc<Value>>,
}

/// A built-in method.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub struct BuiltinMethod {
    pub name: &'static str,
    #[allow(clippy::type_complexity)]
    pub f: for<'a> fn(&'a mut Evaluator, MethodCall<'a>) -> Result<Rc<Value>>,
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

/// A value.
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Value {
    Null,

    Bool(bool),

    // TODO: Should be a bigint.
    Int(i64),

    String(Rc<str>),

    List(Vec<Rc<Value>>),

    // TODO: Should preserve insertion order.
    Set(BTreeSet<Rc<Value>>),

    // TODO: Should preserve insertion order.
    Dict(BTreeMap<Rc<Value>, Rc<Value>>),

    BuiltinFunction(BuiltinFunction),

    BuiltinMethod(BuiltinMethod, Span, Rc<Value>),
}

impl Value {
    /// Extract the dict if it is one, panic otherwise.
    #[inline]
    pub fn expect_dict(&self) -> &BTreeMap<Rc<Value>, Rc<Value>> {
        match self {
            Value::Dict(inner) => inner,
            other => panic!("Expected Dict but got {other:?}."),
        }
    }

    /// Extract the list if it is one, panic otherwise.
    #[inline]
    pub fn expect_list(&self) -> &[Rc<Value>] {
        match self {
            Value::List(inner) => inner.as_ref(),
            other => panic!("Expected List but got {other:?}."),
        }
    }

    /// Extract the list if it is one, panic otherwise.
    #[inline]
    pub fn expect_set(&self) -> &BTreeSet<Rc<Value>> {
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
#[derive(Debug)]
pub struct Env {
    bindings: Vec<(Ident, Rc<Value>)>,
}

/// References a version of an environment that we can later restore to.
#[derive(Copy, Clone)]
pub struct EnvCheckpoint(usize);

impl Env {
    pub fn new() -> Env {
        Env {
            bindings: Vec::new(),
        }
    }

    pub fn lookup(&self, name: &Ident) -> Option<&Rc<Value>> {
        self.bindings
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
        EnvCheckpoint(self.bindings.len())
    }

    /// Push a binding into the environment.
    ///
    /// If the name already existed, the new push will shadow the old one.
    ///
    /// Returns a checkpoint of the environment before the push.
    pub fn push(&mut self, name: Ident, value: Rc<Value>) -> EnvCheckpoint {
        let checkpoint = self.checkpoint();
        self.bindings.push((name, value));
        checkpoint
    }

    /// Pop bindings to get back to a previous version of the environment.
    pub fn pop(&mut self, to: EnvCheckpoint) {
        let EnvCheckpoint(n) = to;
        debug_assert!(
            self.bindings.len() >= n,
            "Cannot restore to checkpoint, more got popped already.",
        );
        self.bindings.truncate(n);
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! builtin_function {
    (
        $rcl_name:expr,
        const $rust_const:ident,
        $rust_name:ident
    ) => {
        const $rust_const: crate::runtime::BuiltinFunction = crate::runtime::BuiltinFunction {
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
        const $rust_const: crate::runtime::BuiltinMethod = crate::runtime::BuiltinMethod {
            name: $rcl_name,
            f: $rust_name,
        };
    };
}
pub(crate) use builtin_method;
