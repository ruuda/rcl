#![no_main]

use libfuzzer_sys::fuzz_target;

use rcl::source::DocId;

/// Evaluate the input expression, then ignore the result.
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
    let doc = rcl::fmt::format_expr(input, &cst);
    let result = doc.print(&cfg);
    Ok(result)
}

/// Run the formatter twice and check for idempotency.
fn fuzz_fmt(input: &str) -> rcl::error::Result<()> {
    let out1 = run_fmt(input)?;
    let out2 = run_fmt(&out1)?;
    assert_eq!(out1, out2, "Formatting should be idempotent.");
    Ok(())
}

/// Evaluate the input, format as json, then evaluate the json.
///
/// The purpose of this fuzzer is twofold:
///
/// * Fuzz the json serializer.
/// * Ensure that evaluation is idempotent. That is, if a given input evaluates
///   to json value `x`, that json value should itself be a valid RCL
///   expression, which should evaluate to `x`.
fn fuzz_eval_json(input: &str) -> rcl::error::Result<()> {
    let mut loader = rcl::loader::Loader::new();
    let mut env = rcl::runtime::Env::new();
    let doc_1 = loader.load_string(input.to_string());
    let val_1 = loader.evaluate(doc_1, &mut env)?;

    let full_span = loader.get_span(doc_1);
    let json = rcl::json::format_json(full_span, val_1.as_ref())?;
    let json = json + rcl::pprint::Doc::HardBreak;

    // TODO: Take the config from the fuzz input?
    let cfg = rcl::pprint::Config { width: 32 };
    let out_1 = json.print(&cfg);
    let doc_2 = loader.load_string(out_1);
    let val_2 = loader.evaluate(doc_2, &mut env)?;

    let full_span = loader.get_span(doc_2);
    let json = rcl::json::format_json(full_span, val_2.as_ref())?;
    let json = json + rcl::pprint::Doc::HardBreak;
    let out_2 = json.print(&cfg);

    assert_eq!(
        loader.get_doc(doc_1).data,
        out_2,
        "Evaluation to json should be idempotent.",
    );

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
        b'e' => {
            let _ = fuzz_eval_json(input);
        }
        _ => return,
    };
});
