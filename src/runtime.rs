// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Representations of values and scopes at runtime.

use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::Ident;
use crate::error::Result;
use crate::eval::Evaluator;
use crate::source::Span;

/// A built-in function.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub struct BuiltinFunction {
    pub name: &'static str,
    #[allow(clippy::type_complexity)]
    pub f: for<'a> fn(&'a mut Evaluator<'a>, Span, &'a [Rc<Value>]) -> Result<Rc<Value>>,
}

/// A built-in method.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub struct BuiltinMethod {
    pub name: &'static str,
    #[allow(clippy::type_complexity)]
    pub f: for<'a> fn(&'a mut Evaluator, Span, &'a Value, &'a [Rc<Value>]) -> Result<Rc<Value>>,
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

    BuiltinMethod(BuiltinMethod, Rc<Value>),
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

macro_rules! builtin_method {
    (
        const $rust_const:ident = $rcl_name:expr,
        fn $rust_name:ident(self: $self:pat, args: $args:pat) -> Result<Rc<Value>> $body:block
    ) => {
        fn $rust_name<'a>(
            _eval: &'a mut Evaluator,
            span: Span,
            receiver: &'a Value,
            args: &'a [Rc<Value>],
        ) -> Result<Rc<Value>> {
            match receiver {
                $self => {
                    match args {
                        $args => $body,
                        _invalid_args => {
                            // TODO: Generate a nicer error message, don't mix Rust types.
                            let err = crate::pprint::concat! {
                                "Invalid arguments for "
                                Doc::highlight($rcl_name)
                                "."
                            };
                            let help = crate::pprint::concat! {
                                "Signature of "
                                Doc::highlight($rcl_name)
                                " is "
                                Doc::highlight(stringify!($args))
                                "."
                            };
                            return span.error(err).with_help(help).err();
                        }
                    }
                }
                // If the receiver has the wrong type, that's a bug; we should
                // only use these builtin methods after the type is already known.
                invalid_self => panic!(
                    "Should not have called {} with {:?} as receiver.",
                    stringify!($rust_name),
                    invalid_self,
                ),
            }
        }

        const $rust_const: BuiltinMethod = BuiltinMethod {
            name: $rcl_name,
            f: $rust_name,
        };
    };
}
pub(crate) use builtin_method;
