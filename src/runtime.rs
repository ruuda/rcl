use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ast::Ident;

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
}

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
        self
            .bindings
            .iter()
            .rev()
            .find(|(k, v)| *k == name)
            .map(|(k, v)| v)
    }

    pub fn push(&mut self, name: Ident, value: Rc<Value>) {
        self.bindings.push((name, value));
    }

    pub fn pop(&mut self) {
        self.bindings.pop().expect("Environment stack underflow.");
    }
}

