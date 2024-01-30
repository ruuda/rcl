#![no_main]

//! This fuzzer tests that the following diagram commutes:
//!
//!             String.chars
//!     String ───────────────► List[String]
//!        │                         │
//!        │ String.len              │ List.len
//!        │                         │
//!        ▼                         ▼
//!       Int ────────────────────► Int
//!                   id
//!
//! In other words, it tests that
//!
//!     foreach s: String: s.len() == s.chars().len()

use libfuzzer_sys::fuzz_target;

use rcl::eval::Evaluator;
use rcl::loader::{Loader};
use rcl::runtime::Value;
use rcl::string::{escape_json};
use rcl::tracer::VoidTracer;

fuzz_target!(|input: &str| {
    let mut expr_str = "let s = \"".to_string();
    escape_json(input, &mut expr_str);
    expr_str.push_str("\"; s.len() == s.chars().len()");

    let mut tracer = VoidTracer;
    let mut loader = Loader::new();
    let id = loader.load_string(expr_str);
    let mut evaluator = Evaluator::new(&mut loader, &mut tracer);
    let mut env = rcl::runtime::prelude();
    let v = evaluator.eval_doc(&mut env, id).expect("Expression is valid.");

    assert_eq!(v, Value::Bool(true));
});
