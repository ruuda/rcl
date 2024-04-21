// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Smith is a fuzzer that generates likely-interesting RCL expressions.
//!
//! The key idea is that if the fuzzer is generating RCL as text bytes, although
//! this works surprisingly well, this will waste a lot of time on inputs with
//! e.g. non-matching brackets that don't pass the lexer, or inputs where all
//! indentifiers are different, so without interesting logic.
//!
//! So for this fuzzer, we treat the fuzz input as instructions for a small
//! stack-based language that builds the RCL input as we go.
//!
//! The name of this fuzzer is inspired by Csmith by John Regehr et al.
//! I don't know if the implementation resembles theirs.

#![no_main]

use libfuzzer_sys::fuzz_target;
use rcl::eval::Evaluator;
use rcl::loader::{Loader, VoidFilesystem};
use rcl::tracer::VoidTracer;
use rcl_fuzz::smith::SynthesizedProgram;

fuzz_target!(|input: SynthesizedProgram| {
    // In repro mode, also print the input when it doesn't fail, so we have a
    // way to spy at what programs the fuzzer is discovering.
    #[cfg(fuzzing_repro)]
    println!("{input:?}");

    let mut loader = Loader::new();
    loader.set_filesystem(Box::new(VoidFilesystem));
    let doc = loader.load_string(input.program);
    let mut tracer = VoidTracer;
    let mut evaluator = Evaluator::new(&mut loader, &mut tracer);
    let mut type_env = rcl::typecheck::prelude();
    let mut value_env = rcl::runtime::prelude();
    let _ = evaluator.eval_doc(&mut type_env, &mut value_env, doc);
});
