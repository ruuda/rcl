#![no_main]

use libfuzzer_sys::fuzz_target;

use rcl::source::DocId;

fn fuzz_eval(input: &str) -> rcl::error::Result<()> {
    let id = DocId(0);
    let (_span, cst) = rcl::parser::parse(id, input)?;
    let ast = rcl::abstraction::abstract_expr(input, &cst);
    let mut env = rcl::runtime::Env::new();
    let _ = rcl::eval::eval(&mut env, &ast)?;
    Ok(())
}

fuzz_target!(|input: &str| {
    // The last byte of the input sets the fuzzing mode. We take the last byte,
    // such that the prefix of the input should at least be a valid input file,
    // which makes it easier to inspect.
    let mode = match input.as_bytes().last() {
        None => return,
        Some(m) => m,
    };

    let id = DocId(0);

    match mode {
        b'a' => {
            let _ = rcl::lexer::lex(id, input);
        }
        b'b' => {
            let _ = rcl::parser::parse(id, input);
        }
        b'c' => {
            let _ = fuzz_eval(input);
        }
        // TODO: Include fmt, json.
        _ => return,
    };
});
