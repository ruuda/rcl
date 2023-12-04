#![no_main]

use std::rc::Rc;

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;

use rcl::error::Error;
use rcl::error::Result;
use rcl::eval::Evaluator;
use rcl::loader::{Document, Filesystem, Loader, PathLookup};
use rcl::markup::MarkupMode;
use rcl::pprint;
use rcl::runtime::Value;
use rcl::source::{Inputs, Span};
use rcl::tracer::Tracer;

/// Tracer that ignores its messages.
pub struct VoidTracer;

impl Tracer for VoidTracer {
    fn trace(&mut self, _inputs: &Inputs, _span: Span, _message: Rc<Value>) {}
}

/// Filesystem that fails to load anything.
///
/// TODO: We could populate files from fuzz inputs to test imports.
struct VoidFilesystem;

impl Filesystem for VoidFilesystem {
    fn resolve(&self, _: &str, _: &str) -> Result<PathLookup> {
        Error::new("Void filesystem does not load files.").err()
    }
    fn resolve_entrypoint(&self, _: &str) -> Result<PathLookup> {
        Error::new("Void filesystem does not load files.").err()
    }
    fn load(&self, _: PathLookup) -> Result<Document> {
        Error::new("Void filesystem does not load files.").err()
    }

}

#[derive(Debug)]
enum Mode {
    Lex,
    Parse,
    Eval,
    Format { width: u32 },
    EvalJson { width: u32 },
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
fn fuzz_eval(loader: &mut Loader, input: &str) -> Result<()> {
    let id = loader.load_string(input.to_string());
    let mut tracer = VoidTracer;
    let mut evaluator = Evaluator::new(loader, &mut tracer);
    let mut env = rcl::runtime::Env::with_prelude();
    let _ = evaluator.eval_doc(&mut env, id)?;
    Ok(())
}

/// Run the formatter once.
fn run_fmt(loader: &mut Loader, input: &str, cfg: &pprint::Config) -> Result<String> {
    let id = loader.load_string(input.to_string());
    let cst = loader.get_cst(id)?;
    let doc = rcl::fmt_cst::format_expr(&input, &cst);
    let result = doc.println(cfg);
    Ok(result)
}

/// Run the formatter twice and check for idempotency.
fn fuzz_fmt(loader: &mut Loader, input: &str, cfg: pprint::Config) -> Result<()> {
    let out1 = run_fmt(loader, &input, &cfg)?;
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
fn fuzz_eval_json(loader: &mut Loader, input: &str, cfg: pprint::Config) -> Result<()> {
    let mut tracer = VoidTracer;
    let mut env = rcl::runtime::Env::with_prelude();
    let doc_1 = loader.load_string(input.to_string());
    let val_1 = loader.evaluate(doc_1, &mut env, &mut tracer)?;

    let full_span = loader.get_span(doc_1);
    let json = rcl::fmt_json::format_json(full_span, val_1.as_ref())?;

    let out_1 = json.println(&cfg);
    let doc_2 = loader.load_string(out_1);
    let val_2 = loader.evaluate(doc_2, &mut env, &mut tracer)?;

    let full_span = loader.get_span(doc_2);
    let json = rcl::fmt_json::format_json(full_span, val_2.as_ref())?;
    let out_2 = json.println(&cfg);

    assert_eq!(
        loader.get_doc(doc_2).data,
        out_2,
        "Evaluation to json should be idempotent.",
    );

    Ok(())
}

fn fuzz_main(loader: &mut Loader, input: Input) -> Result<()> {
    match input.mode {
        Mode::Lex => {
            let doc = loader.load_string(input.data.to_string());
            let _ = loader.get_tokens(doc)?;
        }
        Mode::Parse => {
            let doc = loader.load_string(input.data.to_string());
            let _ = loader.get_cst(doc)?;
        }
        Mode::Eval => {
            let _ = fuzz_eval(loader, input.data);
        }
        Mode::Format { width } => {
            let cfg = pprint::Config {
                width,
                markup: MarkupMode::None,
            };
            let _ = fuzz_fmt(loader, input.data, cfg);
        }
        Mode::EvalJson { width } => {
            let cfg = pprint::Config {
                width,
                markup: MarkupMode::None,
            };
            let _ = fuzz_eval_json(loader, input.data, cfg);
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
        let cfg = pprint::Config {
            width: 80,
            markup: MarkupMode::Ansi,
        };
        let _ = err_doc.println(&cfg);
    }
});
