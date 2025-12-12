// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! An environment binds names to values or types.

use crate::ast::Ident;

/// An environment binds names to values or types.
///
/// At runtime, and at typecheck time, at every point in the AST, there are
/// names in scope (through let bindings, function arguments, or the prelude).
/// An environment tracks which values or types those names are bound to.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Env<T> {
    bindings: Vec<(Ident, T)>,
}

/// References a version of an environment that we can later restore to.
///
/// The phantom type parameter `E` is to prevent accidentally mixing checkpoints
/// from different environments.
#[derive(Copy, Clone)]
pub struct EnvCheckpoint<E>(usize, std::marker::PhantomData<E>);

impl<T> Env<T> {
    /// Create a new empty environment.
    pub fn new() -> Env<T> {
        Env {
            bindings: Vec::new(),
        }
    }

    pub fn lookup(&self, name: &Ident) -> Option<&T> {
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
    pub fn checkpoint(&self) -> EnvCheckpoint<Self> {
        EnvCheckpoint(self.bindings.len(), std::marker::PhantomData)
    }

    /// Push a binding into the environment.
    ///
    /// If the name already existed, the new push will shadow the old one.
    ///
    /// Returns a checkpoint of the environment before the push.
    pub fn push(&mut self, name: Ident, value: T) -> EnvCheckpoint<Self> {
        let checkpoint = self.checkpoint();
        self.bindings.push((name, value));
        checkpoint
    }

    /// Pop bindings to get back to a previous version of the environment.
    pub fn pop(&mut self, to: EnvCheckpoint<Self>) {
        let EnvCheckpoint(n, _phantom) = to;
        debug_assert!(
            self.bindings.len() >= n,
            // coverage:off -- Error message is not covered when we don't hit the error.
            "Cannot restore to checkpoint, more got popped already.",
            // coverage:on
        );
        self.bindings.truncate(n);
    }

    /// Return the maximum of the function `f` applied to every binding.
    pub fn map_fold_max<F: FnMut(&T) -> u32>(&self, mut f: F) -> u32 {
        self.bindings.iter().map(|b| f(&b.1)).max().unwrap_or(0)
    }
}
