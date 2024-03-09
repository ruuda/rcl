#![no_main]

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;

use rcl::error::Result;
use rcl::eval::Evaluator;
use rcl::loader::{Loader, VoidFilesystem};
use rcl::pprint;
use rcl::runtime::Value;
use rcl::source::Span;
use rcl::tracer::VoidTracer;

#[derive(Debug)]
enum Mode {
    Lex,
    Parse,
    Typecheck,
    Eval,
    Format { width: u32 },
    EvalJsonIdempotent { width: u32 },
    EvalJsonCheck { width: u32 },
    EvalTomlCheck { width: u32 },
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
        // full evaluation pipeline. We unconditionally have the width byte even
        // for modes that don't use it, to make samples more portable between
        // modes.
        let mode_byte = u.bytes(1);
        let width = u.arbitrary::<NonNewline>()?.0 as u32;
        let mode = match mode_byte? {
            b"A" => Mode::Lex,
            b"P" => Mode::Parse,
            b"Q" => Mode::Format { width },
            b"T" => Mode::Typecheck,
            b"a" => Mode::Eval,
            b"j" => Mode::EvalJsonIdempotent { width },
            b"k" => Mode::EvalJsonCheck { width },
            b"t" => Mode::EvalTomlCheck { width },
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
#[inline(never)]
fn eval(loader: &mut Loader, input: &str) -> Result<(Span, Value)> {
    let id = loader.load_string(input.to_string());
    let mut tracer = VoidTracer;
    let mut evaluator = Evaluator::new(loader, &mut tracer);
    let mut type_env = rcl::typecheck::prelude();
    let mut value_env = rcl::runtime::prelude();
    let result = evaluator.eval_doc(&mut type_env, &mut value_env, id)?;
    let span = loader.get_span(id);
    Ok((span, result))
}

/// Run the formatter once.
fn run_fmt(loader: &mut Loader, input: &str, cfg: &pprint::Config) -> Result<String> {
    let id = loader.load_string(input.to_string());
    let cst = loader.get_cst(id)?;
    let doc = rcl::fmt_cst::format_expr(input, &cst);
    let mut out = String::new();
    doc.println(cfg).write_string_no_markup(&mut out);
    Ok(out)
}

/// Run the formatter twice and check for idempotency.
fn fuzz_fmt(loader: &mut Loader, input: &str, cfg: pprint::Config) -> Result<()> {
    let out1 = run_fmt(loader, input, &cfg)?;
    let out2 = run_fmt(loader, &out1, &cfg)?;
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
fn fuzz_eval_json_idempotent(loader: &mut Loader, input: &str, cfg: pprint::Config) -> Result<()> {
    let mut tracer = VoidTracer;
    let mut type_env = rcl::typecheck::prelude();
    let mut value_env = rcl::runtime::prelude();
    let doc_1 = loader.load_string(input.to_string());
    let val_1 = loader.evaluate(&mut type_env, &mut value_env, doc_1, &mut tracer)?;

    let full_span = loader.get_span(doc_1);
    let json = rcl::fmt_json::format_json(full_span, &val_1)?;

    let mut out_1 = String::new();
    json.println(&cfg).write_string_no_markup(&mut out_1);
    let doc_2 = loader.load_string(out_1);
    let val_2 = loader.evaluate(&mut type_env, &mut value_env, doc_2, &mut tracer)?;

    let full_span = loader.get_span(doc_2);
    let json = rcl::fmt_json::format_json(full_span, &val_2)?;
    let mut out_2 = String::new();
    json.println(&cfg).write_string_no_markup(&mut out_2);

    assert_eq!(
        loader.get_doc(doc_2).data,
        out_2,
        "Evaluation to json should be idempotent.",
    );

    Ok(())
}

/// Evaluate the input expression into json.
///
/// Then check that the result can be parsed by the `serde_json` crate.
fn fuzz_eval_json_check(loader: &mut Loader, input: &str, cfg: pprint::Config) -> Result<()> {
    let (full_span, value) = eval(loader, input)?;
    let json_doc = rcl::fmt_json::format_json(full_span, &value)?;
    let mut json_str = String::new();
    json_doc.println(&cfg).write_string_no_markup(&mut json_str);
    match serde_json::from_str::<serde_json::Value>(&json_str[..]) {
        Ok(..) => Ok(()),
        Err(err) => panic!("RCL output should be parseable, but got {err:?}"),
    }
}

/// Evaluate the input expression into toml.
///
/// Then check that the result can be parsed by the `toml` crate.
fn fuzz_eval_toml_check(loader: &mut Loader, input: &str, cfg: pprint::Config) -> Result<()> {
    let (full_span, value) = eval(loader, input)?;
    let toml_doc = rcl::fmt_toml::format_toml(full_span, &value)?;
    let mut toml_str = String::new();
    toml_doc.println(&cfg).write_string_no_markup(&mut toml_str);
    match toml::from_str::<toml::Value>(&toml_str[..]) {
        Ok(..) => Ok(()),
        Err(err) => panic!("RCL output should be parseable, but got {err:?}"),
    }
}

fn fuzz_main(loader: &mut Loader, input: Input) -> Result<()> {
    let mut cfg = pprint::Config { width: 80 };
    match input.mode {
        Mode::Lex => {
            let doc = loader.load_string(input.data.to_string());
            let _ = loader.get_tokens(doc)?;
        }
        Mode::Parse => {
            let doc = loader.load_string(input.data.to_string());
            let _ = loader.get_cst(doc)?;
        }
        Mode::Typecheck => {
            let doc = loader.load_string(input.data.to_string());
            let mut env = rcl::typecheck::prelude();
            let _ = loader.get_typechecked_ast(&mut env, doc)?;
        }
        Mode::Eval => {
            let _ = eval(loader, input.data);
        }
        Mode::Format { width } => {
            cfg.width = width;
            let _ = fuzz_fmt(loader, input.data, cfg);
        }
        Mode::EvalJsonIdempotent { width } => {
            cfg.width = width;
            let _ = fuzz_eval_json_idempotent(loader, input.data, cfg);
        }
        Mode::EvalJsonCheck { width } => {
            cfg.width = width;
            let _ = fuzz_eval_json_check(loader, input.data, cfg);
        }
        Mode::EvalTomlCheck { width } => {
            cfg.width = width;
            let _ = fuzz_eval_toml_check(loader, input.data, cfg);
        }
    };

    Ok(())
}

fuzz_target!(|input: Input| {
    let mut loader = Loader::new();
    loader.set_filesystem(Box::new(VoidFilesystem));
    let result = fuzz_main(&mut loader, input);

    // In the error case, we do also pretty-print the error. This is mostly to
    // confirm that all the spans in the error are aligned to code point
    // boundaries, so we don't slice strings incorrectly and crash -- it would
    // be a shame if the fuzzer says the evaluator is clean, but then in
    // practice we can crash when reporting an error.
    if let Err(err) = result {
        let inputs = loader.as_inputs();
        let err_doc = err.report(&inputs);
        let cfg = pprint::Config { width: 80 };
        let _ = err_doc.println(&cfg);
    }
});
