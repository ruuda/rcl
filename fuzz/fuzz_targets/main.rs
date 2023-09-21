#![no_main]

use libfuzzer_sys::fuzz_target;

use rcl::source::DocId;

fn fuzz_eval(input: &str) -> rcl::error::Result<()> {
    let id = DocId(0);
    let tokens = rcl::lexer::lex(id, input)?;
    let (_span, cst) = rcl::parser::parse(id, input, &tokens)?;
    let ast = rcl::abstraction::abstract_expr(input, &cst)?;
    let mut env = rcl::runtime::Env::new();
    let _ = rcl::eval::eval(&mut env, &ast)?;
    Ok(())
}

/// Run the formatter once.
fn run_fmt(input: &str) -> rcl::error::Result<String> {
    let id = DocId(0);
    let tokens = rcl::lexer::lex(id, input)?;
    let (_span, cst) = rcl::parser::parse(id, input, &tokens)?;
    // For the fuzzer, we set the format width somewhat lower than the default,
    // so we can explore interesting behavior with smaller inputs.
    let cfg = rcl::pprint::Config { width: 32 };
    let result = rcl::fmt::format_expr(input, &cst, &cfg);
    Ok(result)
}

/// Run the formatter twice and check for idempotency.
fn fuzz_fmt(input: &str) -> rcl::error::Result<()> {
    let out1 = run_fmt(input)?;
    let out2 = run_fmt(&out1)?;
    assert_eq!(out1, out2, "Formatting should be idempotent.");
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
            use rcl::{lexer::lex, parser::parse};
            let _ = lex(id, input).and_then(|tokens| parse(id, input, &tokens));
        }
        b'c' => {
            let _ = fuzz_eval(input);
        }
        b'd' => {
            let _ = fuzz_fmt(input);
        }
        // TODO: Include json after eval.
        _ => return,
    };
});
