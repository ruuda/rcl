#![no_main]

use libfuzzer_sys::fuzz_target;
use arbitrary::{Arbitrary, Unstructured};

use rcl::markup::MarkupMode;
use rcl::pprint;
use rcl::source::DocId;

#[derive(Debug)]
enum Mode {
    Lex,
    Parse,
    Eval,
    Format {
        width: u32,
    },
    EvalJson {
        width: u32,
    }
}

/// Helper for `Arbitrary` to get a value in 0..=245, such that the byte is not a newline.
struct NonNewline(u8);

impl Arbitrary<'_> for NonNewline {
    fn arbitrary(u: &mut Unstructured) -> arbitrary::Result<NonNewline> {
        match u.bytes(1)? {
            [n] if *n > b'\n' => Ok(NonNewline(n - b'\n')),
            _ => Err(arbitrary::Error::IncorrectFormat),
        }
    }
}

#[derive(Debug)]
struct Input<'a> {
    mode: Mode,
    data: &'a str,
}

impl<'a> Arbitrary<'a> for Input<'a> {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Input<'a>> {
        use arbitrary::Error;

        // The fuzz inputs must start with a comment line. This should ensure
        // that we can evaluate the fuzz input as a regular file as well,
        // although in some cases the leading comment may cause a divergence.
        // In that case it's still easy to pipe through `tail -n +2`.
        if u.bytes(2)? != b"//" {
            return Err(Error::IncorrectFormat);
        }

        // The modes are characters on purpose, so the resulting fuzz input
        // remains human-readable. The simpler fuzz modes are also deliberately
        // using lower characters, so that we can "REDUCE" (in libfuzzer
        // terminology) an input that is expensive to evaluate to a simpler one
        // that still provides a unique input, but doesn't require running the
        // full evaluation pipeline.
        let mode = match u.bytes(1)? {
            b"a" => Mode::Lex,
            b"b" => Mode::Parse,
            b"c" => Mode::Eval,
            b"d" => Mode::Format {
                width: u.arbitrary::<NonNewline>()?.0 as u32,
            },
            b"e" => Mode::EvalJson {
                width: u.arbitrary::<NonNewline>()?.0 as u32,
            },
            _ => return Err(Error::IncorrectFormat),
        };

        if u.bytes(1)? != b"\n" {
            return Err(Error::IncorrectFormat);
        }

        let data = match std::str::from_utf8(u.bytes(u.len())?) {
            Ok(s) => s,
            Err(_) => return Err(Error::IncorrectFormat),
        };

        let result = Input { mode, data };
        Ok(result)
    }
}

/// Evaluate the input expression, then ignore the result.
fn fuzz_eval(input: &str) -> rcl::error::Result<()> {
    let id = DocId(0);
    let tokens = rcl::lexer::lex(id, &input)?;
    let (_span, cst) = rcl::parser::parse(id, &input, &tokens)?;
    let ast = rcl::abstraction::abstract_expr(&input, &cst)?;
    let mut env = rcl::runtime::Env::new();
    let _ = rcl::eval::eval(&mut env, &ast)?;
    Ok(())
}

/// Run the formatter once.
fn run_fmt(input: &str, cfg: &pprint::Config) -> rcl::error::Result<String> {
    let id = DocId(0);
    let tokens = rcl::lexer::lex(id, &input)?;
    let (_span, cst) = rcl::parser::parse(id, &input, &tokens)?;
    let doc = rcl::fmt::format_expr(&input, &cst);
    let result = doc.println(cfg);
    Ok(result)
}

/// Run the formatter twice and check for idempotency.
fn fuzz_fmt(input: &str, cfg: pprint::Config) -> rcl::error::Result<()> {
    let out1 = run_fmt(&input, &cfg)?;
    let out2 = run_fmt(&out1, &cfg)?;
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
fn fuzz_eval_json(input: &str, cfg: pprint::Config) -> rcl::error::Result<()> {
    let mut loader = rcl::loader::Loader::new();
    let mut env = rcl::runtime::Env::new();
    let doc_1 = loader.load_string(input.to_string());
    let val_1 = loader.evaluate(doc_1, &mut env)?;

    let full_span = loader.get_span(doc_1);
    let json = rcl::json::format_json(full_span, val_1.as_ref())?;

    let out_1 = json.println(&cfg);
    let doc_2 = loader.load_string(out_1);
    let val_2 = loader.evaluate(doc_2, &mut env)?;

    let full_span = loader.get_span(doc_2);
    let json = rcl::json::format_json(full_span, val_2.as_ref())?;
    let out_2 = json.println(&cfg);

    assert_eq!(
        loader.get_doc(doc_2).data,
        out_2,
        "Evaluation to json should be idempotent.",
    );

    Ok(())
}

fuzz_target!(|input: Input| {
    let id = DocId(0);

    match input.mode {
        Mode::Lex => {
            let _ = rcl::lexer::lex(id, input.data);
        }
        Mode::Parse => {
            use rcl::{lexer::lex, parser::parse};
            let _ = lex(id, &input.data).and_then(|tokens| parse(id, input.data, &tokens));
        }
        Mode::Eval => {
            let _ = fuzz_eval(input.data);
        }
        Mode::Format { width } => {
            let cfg = pprint::Config { width, markup: MarkupMode::None };
            let _ = fuzz_fmt(input.data, cfg);
        }
        Mode::EvalJson { width } => {
            let cfg = pprint::Config { width, markup: MarkupMode::None };
            let _ = fuzz_eval_json(input.data, cfg);
        }
    };
});
