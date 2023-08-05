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

/// A built-in function.
pub struct Builtin {
    pub name: &'static str,
    pub f: Box<dyn Fn(&[Rc<Value>]) -> Result<Rc<Value>>>,
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
    Bool(bool),

    // TODO: Should be a bigint.
    Int(i64),

    String(String),

    List(Vec<Rc<Value>>),

    // TODO: Should preserve insertion order.
    Set(BTreeSet<Rc<Value>>),

    // TODO: Should preserve insertion order.
    Map(BTreeMap<Rc<Value>, Rc<Value>>),

    Builtin(Builtin),
}

/// An environment binds names to values.
#[derive(Debug)]
pub struct Env {
    bindings: Vec<(Ident, Rc<Value>)>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            bindings: Vec::new(),
        }
    }

    pub fn lookup(&self, name: Ident) -> Option<&Rc<Value>> {
        self.bindings
            .iter()
            .rev()
            .find(|(k, _v)| *k == name)
            .map(|(_k, v)| v)
    }

    pub fn push(&mut self, name: Ident, value: Rc<Value>) {
        self.bindings.push((name, value));
    }

    pub fn pop(&mut self) {
        self.bindings.pop().expect("Environment stack underflow.");
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
