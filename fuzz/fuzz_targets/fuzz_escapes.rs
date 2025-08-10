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
    // We don't use the prelude here, the expression doesn't use it.
    let mut type_env = rcl::env::Env::new();
    let mut value_env = rcl::env::Env::new();
    let doc = loader.load_string("input_escaped", escaped);
    let result = loader
        .evaluate(&mut type_env, &mut value_env, doc, &mut tracer)
        .expect("Escaped string should be valid RCL.");
    match result {
        Value::String(unescaped) => assert_eq!(input, unescaped.as_ref()),
        _not_string => panic!("Should have evaluated to a string."),
    }
});
