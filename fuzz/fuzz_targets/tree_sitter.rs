// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! A fuzzer for the Tree-sitter grammar.
//!
//! This fuzzer runs the same input through both the Tree-sitter parser and
//! RCL's parser, and then checks that they agree. Right now we only check
//! that they agree about the input validity, but we should do a deep comparison
//! of the parse tree. TODO: Implement that comparison.

#![no_main]

use libfuzzer_sys::fuzz_target;
use rcl::loader::Loader;

fuzz_target!(|input: &str| {
    // Step 1: Parse with Tree-sitter.
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_rcl::language())
        .expect("Failed to load grammar.");
    let ts_tree = parser
        .parse(input, None)
        .expect("Tree-sitter produces a tree when configured correctly.");

    // Step 2: Parse with RCL.
    let mut loader = Loader::new();
    let id = loader.load_string(input.to_string());
    let rcl_tree = loader.get_cst(id);

    // Step 3: Compare.
    // It would be nice to do a deep comparison, but as a first step, when the
    // grammar is still in progress, we can check that RCL can parse iff TS can
    // parse. TODO: extend to deeper comparison.
    match rcl_tree {
        Ok(..) => assert!(
            !ts_tree.root_node().has_error(),
            "RCL accepted but Tree-sitter rejected.",
        ),
        Err(..) => assert!(
            ts_tree.root_node().has_error(),
            "RCL rejected but Tree-sitter accepted.",
        ),
    }
});
