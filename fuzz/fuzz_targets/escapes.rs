#![no_main]

//! This fuzzer test that json-escaping and then unescaping RCL is the identity.

use libfuzzer_sys::fuzz_target;

use rcl::string::{unescape, escape_json};
use rcl::source::{DocId, Span};

fuzz_target!(|input: &str| {
    let mut escaped = String::new();
    escape_json(input, &mut escaped);

    let span = Span::new(DocId(0), 0, escaped.len());
    let unescaped = unescape(&escaped, span).unwrap();

    assert_eq!(input, unescaped);
});
