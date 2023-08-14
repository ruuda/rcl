#![no_main]

use libfuzzer_sys::fuzz_target;

use rcl::source::DocId;

fuzz_target!(|data: &[u8]| {
    // The last byte of the input sets the fuzzing mode. We take the last byte,
    // such that the prefix of the input should at least be a valid input file,
    // which makes it easier to inspect.
    let mode = match data.last() {
        None => return,
        Some(m) => m,
    };

    let input = match std::str::from_utf8(&data[..data.len() - 1]) {
        Ok(s) => s,
        Err(..) => return,
    };

    let id = DocId(0);

    match mode {
        b'a' => {
            let _ = rcl::lexer::lex(id, input);
        }
        b'b' => {
            let _ = rcl::parser::parse(id, input);
        }
        // TODO: Include eval, fmt, json.
        _ => return,
    };
});
