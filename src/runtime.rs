// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Representations of values and scopes at runtime.

use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::Ident;
use crate::error::Result;
use crate::source::Span;

pub type BuiltinFnBox = Box<dyn Fn(Span, &[Rc<Value>]) -> Result<Rc<Value>>>;

/// A built-in function.
pub struct Builtin {
    pub name: &'static str,
    pub f: BuiltinFnBox,
}

impl std::fmt::Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(other.name)
    }
}

impl PartialOrd for Builtin {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.name.partial_cmp(other.name)
    }
}

impl Eq for Builtin {}
impl Ord for Builtin {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(other.name)
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

    Builtin(Builtin),
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

    /// Return a checkpoint of the environment to later [`pop`] to.
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
