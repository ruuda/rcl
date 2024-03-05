// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use rcl::error::Result;
use rcl::eval::Evaluator;
use rcl::loader::{Loader, VoidFilesystem};
use rcl::markup::MarkupMode;
use rcl::pprint;
use rcl::tracer::VoidTracer;
use wasm_bindgen::prelude::wasm_bindgen;

fn rcl_evaluate_impl<'a>(
    loader: &'a mut Loader,
    print_cfg: &pprint::Config,
    input: &'a str,
) -> Result<String> {
    loader.set_filesystem(Box::new(VoidFilesystem));
    let id = loader.load_string(input.to_string());
    let mut tracer = VoidTracer;
    let mut evaluator = Evaluator::new(loader, &mut tracer);
    let mut type_env = rcl::typecheck::prelude();
    let mut value_env = rcl::runtime::prelude();
    let value = evaluator.eval_doc(&mut type_env, &mut value_env, id)?;
    let full_span = loader.get_span(id);
    // TODO: Make output format configurable.
    let json = rcl::fmt_json::format_json(full_span, &value)?;
    Ok(json.println(print_cfg))
}

#[wasm_bindgen]
pub fn rcl_evaluate(input: &str) -> std::result::Result<String, String> {
    let mut loader = Loader::new();
    let cfg = pprint::Config {
        // TODO: Make this configurable.
        width: 60,
        markup: MarkupMode::None,
    };
    match rcl_evaluate_impl(&mut loader, &cfg, input) {
        Ok(string) => Ok(string),
        Err(err) => {
            let inputs = loader.as_inputs();
            let err_doc = err.report(&inputs);
            Err(err_doc.println(&cfg))
        }
    }
}
