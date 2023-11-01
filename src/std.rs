// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Implementation of the standard library.

use std::collections::BTreeMap;
use std::rc::Rc;

use crate::error::{IntoError, Result};
use crate::eval::Evaluator;
use crate::pprint::Doc;
use crate::runtime::{builtin_function, BuiltinFunction, Value};
use crate::source::Span;

builtin_function!(
    const STD_LOAD_UTF8 = "std.load_utf8",
    fn builtin_std_load_utf8(args: [path]) -> Result<Rc<Value>> {
        let path_string = path.as_string();
        unimplemented!("TODO: load_utf8({path_string})");
    }
);

/// Initialize the standard library.
pub fn initialize() -> Rc<Value> {
    let mut builtins: BTreeMap<Rc<Value>, Rc<Value>> = BTreeMap::new();

    builtins.insert(
        Rc::new("load_utf8".into()),
        Rc::new(Value::BuiltinFunction(STD_LOAD_UTF8)),
    );

    Rc::new(Value::Dict(builtins))
}
