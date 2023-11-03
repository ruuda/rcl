// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Implementation of the standard library.

use std::collections::BTreeMap;
use std::rc::Rc;

use crate::error::{Error, IntoError, Result};
use crate::eval::Evaluator;
use crate::pprint::Doc;
use crate::runtime::{builtin_function, BuiltinFunction, Value};
use crate::source::Span;

builtin_function!(
    const STD_READ_FILE_UTF8 = "std.read_file_utf8",
    fn builtin_std_read_file_utf8(eval: &mut Evaluator, args: [path]) -> Result<Rc<Value>> {
        // TODO: Do typecheck ahead of time so here we can just assume we get
        // a string.
        let path = match path.as_ref() {
            Value::String(path) => path,
            // TODO: Include arg span in the call info so we can better report
            // an error.
            _ => return Error::new("read_file_utf8 takes a String.").err(),
        };
        let from = eval.import_stack.last().map(|ctx| ctx.doc);
        let doc = eval.loader.load_path(path, from)?;
        Ok(Rc::new(eval.loader.get_doc(doc).data.into()))
    }
);

/// Initialize the standard library.
pub fn initialize() -> Rc<Value> {
    let mut builtins: BTreeMap<Rc<Value>, Rc<Value>> = BTreeMap::new();

    builtins.insert(
        Rc::new("read_file_utf8".into()),
        Rc::new(Value::BuiltinFunction(STD_READ_FILE_UTF8)),
    );

    Rc::new(Value::Dict(builtins))
}
