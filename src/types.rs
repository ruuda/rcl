// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Representations of types.

use std::rc::Rc;

/// A type.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Type {
    /// The primitive type `Bool`.
    Bool,

    /// The primitive type `Int`.
    Int,

    /// The primitive type `Null`.
    Null,

    /// The primitive type `String`.
    String,

    /// A dict with the given key and value types.
    Dict(Rc<Type>, Rc<Type>),

    /// A list with the given element type.
    List(Rc<Type>),

    /// A set with the given element type.
    Set(Rc<Type>),

    /// A function.
    Function(Function),
}

/// A function type.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Function {
    pub args: Vec<Rc<Type>>,
    pub result: Rc<Type>,
}
