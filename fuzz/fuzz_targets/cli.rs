#![no_main]

//! This fuzzer tests that the CLI parser does not crash whatever the input.

use libfuzzer_sys::fuzz_target;

use rcl::cli::cli_parser;

fuzz_target!(|input: Vec<String>| {
    let _ = cli_parser::parse(input);
});
