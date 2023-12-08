#![no_main]

//! This fuzzer tests that the CLI parser does not crash whatever the input.

use libfuzzer_sys::fuzz_target;

use rcl::cli::{self, StdinCapabilities};

fuzz_target!(|input: (Vec<String>, bool)| {
    let argv = input.0;
    let stdin = if input.1 {
        StdinCapabilities::Terminal
    } else {
        StdinCapabilities::NonInteractive
    };
    let _ = cli::parse(argv, stdin);
});
