// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The “über” fuzzers can assert many invariants from the same source code.
//!
//! Given a piece of RCL code, we can exercise many parts of the evaluation
//! pipeline. The standard ones are:
//!
//! * Lexing only.
//! * Lexing and parsing.
//! * Lexing, parsing, and abstraction (CST to AST).
//! * Lexing, parsing, abstraction, and typechecking.
//! * Lexing, parsing, abstraction, typechecking, and evaluation.
//!
//! From then on, we can also still export to json, toml, and other formats, so
//! those formatters could be exercised. But we could also stop at the CST, and
//! exercise the autoformatter instead, etc.
//!
//! So just for covering all the code, we already have many options. Covering
//! them is useful to find things like assertion failures, index out of bounds,
//! slicing strings on non-code point boundaries, etc. But it's still pretty
//! shallow. There are more interesting invariants that we can test:
//!
//! * RCL is a superset of json, so if an input _can_ be exported to json, then
//!   evaluating it again should produce the same value.
//! * The autoformatter should be idempotent.
//!
//! We could write different fuzzers for each of these cases, but the inputs are
//! always the same: a piece of RCL code, so it would be useful to share the
//! corpus. That's why all these checks live together in this “über” module.

use rcl::error::Result;
use rcl::eval::Evaluator;
use rcl::loader::{Loader, VoidFilesystem};
use rcl::pprint;
use rcl::runtime::Value;
use rcl::source::Span;
use rcl::tracer::VoidTracer;

#[derive(Debug)]
pub enum Mode {
    Eval,
    FormatIdempotent { width: u32 },
    EvalJsonIdempotent { width: u32 },
    EvalJsonCheck { width: u32 },
    EvalTomlCheck { width: u32 },
    EvalFormat { width: u32 },
    EvalJsonSuperset,
    EvalJsonLines,
}

pub fn fuzz_main(mode: Mode, input: &str) {
    let mut loader = Loader::new();
    loader.set_filesystem(Box::new(VoidFilesystem));
    let result = fuzz_main_impl(&mut loader, mode, input);

    // In the error case, we do also pretty-print the error. This is mostly to
    // confirm that all the spans in the error are aligned to code point
    // boundaries, so we don't slice strings incorrectly and crash -- it would
    // be a shame if the fuzzer says the evaluator is clean, but then in
    // practice we can crash when reporting an error.
    if let Err(err) = result {
        let inputs = loader.as_inputs();
        let err_doc = err.report(&inputs);
        let cfg = pprint::Config { width: Some(80) };
        let _ = err_doc.println(&cfg);
    }
}

fn fuzz_main_impl(loader: &mut Loader, mode: Mode, input: &str) -> Result<()> {
    let mut cfg = pprint::Config { width: Some(80) };

    match mode {
        Mode::Eval => {
            let _ = eval(loader, input);
        }
        Mode::FormatIdempotent { width } => {
            cfg.width = Some(width);
            let _ = fuzz_fmt(loader, input, cfg);
        }
        Mode::EvalJsonIdempotent { width } => {
            cfg.width = Some(width);
            let _ = fuzz_eval_json_idempotent(loader, input, cfg);
        }
        Mode::EvalJsonCheck { width } => {
            cfg.width = Some(width);
            let _ = fuzz_eval_json_check(loader, input, cfg);
        }
        Mode::EvalTomlCheck { width } => {
            cfg.width = Some(width);
            let _ = fuzz_eval_toml_check(loader, input, cfg);
        }
        Mode::EvalFormat { width } => {
            cfg.width = Some(width);
            let _ = fuzz_eval_format(loader, input, cfg);
        }
        Mode::EvalJsonSuperset => {
            fuzz_eval_json_superset(loader, input);
        }
        Mode::EvalJsonLines => {
            let _ = fuzz_eval_json_lines(loader, input);
        }
    };

    Ok(())
}

/// Evaluate the input expression, then ignore the result.
#[inline(never)]
fn eval(loader: &mut Loader, input: &str) -> Result<(Span, Value)> {
    let id = loader.load_string("input", input.to_string());
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
    let id = loader.load_string("input", input.to_string());
    let cst = loader.get_cst(id)?;
    let inputs = loader.as_inputs();
    let doc = rcl::fmt_cst::format_expr(&inputs, &cst);
    Ok(doc.println(cfg).to_string_no_markup())
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
    let doc_1 = loader.load_string("input", input.to_string());
    let val_1 = loader.evaluate(&mut type_env, &mut value_env, doc_1, &mut tracer)?;

    let body_span = loader.get_span(doc_1);
    let json = rcl::fmt_json::format_json(body_span, &val_1)?;

    let out_1 = json.println(&cfg).to_string_no_markup();
    let doc_2 = loader.load_string("out_1", out_1);
    let val_2 = loader.evaluate(&mut type_env, &mut value_env, doc_2, &mut tracer)?;

    let body_span = loader.get_span(doc_2);
    let json = rcl::fmt_json::format_json(body_span, &val_2)?;
    let out_2 = json.println(&cfg).to_string_no_markup();

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
    let json_str = json_doc.println(&cfg).to_string_no_markup();
    match serde_json::from_str::<serde_json::Value>(&json_str[..]) {
        Ok(..) => Ok(()),
        // RCL accepts numbers with larger exponents than what Serde accepts.
        // This is not a bug in the json output but a (reasonable) implementation
        // limitation of Serde, so we do allow this mismatch.
        Err(err)
            if err.is_syntax()
                && json_str.contains("e")
                && err.to_string().contains("number out of range") =>
        {
            Ok(())
        }
        Err(err) => panic!("RCL output should be parseable, but got {err:?}"),
    }
}

/// Evaluate the input expression into toml.
///
/// Then check that the result can be parsed by the `toml` crate.
fn fuzz_eval_toml_check(loader: &mut Loader, input: &str, cfg: pprint::Config) -> Result<()> {
    let (full_span, value) = eval(loader, input)?;
    let toml_doc = rcl::fmt_toml::format_toml(full_span, &value)?;
    let toml_str = toml_doc.println(&cfg).to_string_no_markup();
    match toml::from_str::<toml::Value>(&toml_str[..]) {
        Ok(..) => Ok(()),
        Err(err) if err.message().contains("invalid floating-point number") => {
            // RCL supports larger exponents on floats than what the toml crate
            // admits, this is an intentional incompatibility.
            Ok(())
        }
        Err(err) => panic!("RCL output should be parseable, but got {err:?}"),
    }
}

/// Check that formatting after evaluation is idempotent.
///
/// When evaluating and printing as RCL, an expression gets formatted through
/// the expression pretty-printer, but when formatting that again, it gets
/// formatted through the CST pretty-printer. This mode ensures that formatting
/// is idempotent. In other words, ensure that the expression and CST pretty-
/// printer agree.
fn fuzz_eval_format(loader: &mut Loader, input: &str, cfg: pprint::Config) -> Result<()> {
    let (_span, value) = eval(loader, input)?;
    let doc1 = rcl::fmt_rcl::format_rcl(&value);
    let out1 = doc1.println(&cfg).to_string_no_markup();
    let out2 = run_fmt(loader, &out1, &cfg)?;
    assert_eq!(out1, out2, "Formatting after evaluation should be a no-op.");
    Ok(())
}

/// Check that any json that Serde can parse, can be evaluated as RCL.
///
/// This is the closest we can get to testing that RCL is a json superset.
fn fuzz_eval_json_superset(loader: &mut Loader, input: &str) {
    // First try to deserialize the input using Serde. If this fails, then we
    // don't even attempt RCL; this is about json supersets.
    use serde::de::IgnoredAny;
    use serde_json::Result;
    let result: Result<IgnoredAny> = serde_json::de::from_str(input);
    if result.is_err() {
        return;
    }

    match eval(loader, input) {
        Ok(..) => {
            // Works as intended.
        }
        Err(err) => {
            let msg = format!("{err:?}");
            if msg.contains("Overflow") {
                // In cases of a large integer or large exponent, Serde parses
                // it into a different data type or into a float that goes Inf,
                // but RCL cannot represent those, so the incompatibility here
                // is intentional.
            } else if msg.contains("not a Unicode scalar value") {
                // RCL does not support surrogate pairs on purpose.
            } else {
                panic!("If Serde can parse it, RCL should be able to, but got: {msg}");
            }
        }
    }
}

/// Evaluate the input expression into json-lines.
///
/// Then check that the result contains exactly as many newline characters as
/// elements in the array, i.e. the json-lines output does not emit newlines in
/// the middle of values.
fn fuzz_eval_json_lines(loader: &mut Loader, input: &str) -> Result<()> {
    let cfg = pprint::Config { width: None };
    let (full_span, value) = eval(loader, input)?;
    let jsonl_doc = rcl::fmt_json_lines::format_json_lines(full_span, &value)?;
    let jsonl_str = jsonl_doc.println(&cfg).to_string_no_markup();
    let newlines_observed = jsonl_str.as_bytes().iter().filter(|b| **b == 0x0a).count();
    let newlines_expected = value.expect_list().len();
    assert_eq!(newlines_observed, newlines_expected);
    Ok(())
}
