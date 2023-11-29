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
use crate::runtime::{builtin_function, FunctionCall, Value};

builtin_function!(
    "std.read_file_utf8",
    const STD_READ_FILE_UTF8,
    builtin_std_read_file_utf8
);
fn builtin_std_read_file_utf8(eval: &mut Evaluator, call: FunctionCall) -> Result<Rc<Value>> {
    call.check_arity_static("std.read_file_utf8", &["path"])?;
    let arg_span = call.args[0].0;
    let path = match call.args[0].1.as_ref() {
        Value::String(s) => s,
        _not_string => {
            // TODO: Add proper typechecking and a proper type error.
            return arg_span
                .error("Expected a String here, but got a different type.")
                .err();
        }
    };
    let from = eval.import_stack.last().map(|ctx| ctx.doc);
    let doc = eval
        .loader
        .load_path(path, from)
        .map_err(|err| err.with_origin(arg_span))?;
    Ok(Rc::new(eval.loader.get_doc(doc).data.into()))
}

/// Initialize the standard library.
pub fn initialize() -> Rc<Value> {
    let mut builtins: BTreeMap<Rc<Value>, Rc<Value>> = BTreeMap::new();

    builtins.insert(
        Rc::new("read_file_utf8".into()),
        Rc::new(Value::BuiltinFunction(STD_READ_FILE_UTF8)),
    );

    Rc::new(Value::Dict(builtins))
}
