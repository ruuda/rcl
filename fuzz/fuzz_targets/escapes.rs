#![no_main]

//! This fuzzer test that json-escaping and then unescaping RCL is the identity.

use libfuzzer_sys::fuzz_target;

use rcl::loader::Loader;
use rcl::runtime::Value;
use rcl::string::escape_json;
use rcl::tracer::VoidTracer;

fuzz_target!(|input: &str| {
    let mut escaped = "\"".to_string();
    escape_json(input, &mut escaped);
    escaped.push('"');

    let mut loader = Loader::new();
    let mut tracer = VoidTracer;
    let mut env = rcl::runtime::prelude();
    let doc = loader.load_string(escaped);
    let result = loader.evaluate(doc, &mut env, &mut tracer).expect("Escaped string should be valid RCL.");
    match result {
        Value::String(unescaped) => assert_eq!(input, unescaped.as_ref()),
        _not_string => panic!("Should have evaluated to a string."),
    }
});
