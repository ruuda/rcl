// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use rcl::error::Result;
use rcl::eval::Evaluator;
use rcl::loader::{Loader, VoidFilesystem};
use rcl::tracer::VoidTracer;
use wasm_bindgen::prelude::wasm_bindgen;

fn rcl_load_impl(input: &str) -> Result<()> {
    let mut loader = Loader::new();
    loader.set_filesystem(Box::new(VoidFilesystem));
    let id = loader.load_string(input.to_string());
    let mut tracer = VoidTracer;
    let mut evaluator = Evaluator::new(&mut loader, &mut tracer);
    let mut type_env = rcl::typecheck::prelude();
    let mut value_env = rcl::runtime::prelude();
    let _result = evaluator.eval_doc(&mut type_env, &mut value_env, id)?;
    Ok(())
}

#[wasm_bindgen]
pub fn rcl_load(input: &str) -> i32 {
    match rcl_load_impl(input) {
        Ok(()) => return 0,
        Err(..) => return -1,
    }
}
