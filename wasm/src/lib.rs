// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

// Needed to silence the `module = ...` attribute in the extern import,
// due to limitations in wasm-bindgen.
#![allow(unused_variables)]

use rcl::error::Result;
use rcl::eval::Evaluator;
use rcl::loader::{Loader, VoidFilesystem};
use rcl::markup::Markup;
use rcl::pprint::{self, Doc};
use rcl::runtime::Value;
use rcl::tracer::VoidTracer;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
extern "C" {
    pub type Node;

    #[wasm_bindgen(module = "rcl_dom.js")]
    fn append_span(node: &Node, class: &str, text: &str);
}

/// Return the class name for a markup span in the output.
fn markup_class(markup: Markup) -> &'static str {
    match markup {
        Markup::Builtin => "builtin",
        Markup::Comment => "comment",
        Markup::Error => "error",
        Markup::Escape => "escape",
        Markup::Field => "field",
        Markup::Highlight => "highlight",
        Markup::Keyword => "keyword",
        Markup::None => "text",
        Markup::Number => "number",
        Markup::String => "string",
        Markup::Trace => "trace",
        Markup::Type => "type",
        Markup::Warning => "warning",
    }
}

struct PrintConfig {
    /// The width for formatting.
    width: u32,
    /// After how many bytes to stop outputting.
    max_len: u32,
}

/// Pretty-print a document, append it as DOM nodes.
fn pprint_doc(cfg: &PrintConfig, doc: Doc, out_node: &Node) {
    let pprint_cfg = pprint::Config { width: cfg.width };
    let markup_string = doc.println(&pprint_cfg);
    let mut markup = Markup::None;
    let mut buffer = String::new();
    let mut n_written = 0_u32;
    for (f_str, f_markup) in markup_string.fragments.iter() {
        if markup != *f_markup {
            append_span(out_node, markup_class(markup), &buffer);
            buffer.clear();
            markup = *f_markup;
        }
        n_written += f_str.len() as u32;
        if n_written > cfg.max_len {
            break;
        }
        buffer.push_str(f_str);
    }
    append_span(out_node, markup_class(markup), &buffer);

    if n_written > cfg.max_len {
        append_span(
            out_node,
            "warning",
            "...\nTruncated output to keep your browser fast.",
        );
    }
}

fn rcl_evaluate_json_impl<'a>(
    cfg: &PrintConfig,
    loader: &'a mut Loader,
    input: &'a str,
    out_node: &Node,
) -> Result<()> {
    loader.set_filesystem(Box::new(VoidFilesystem));
    let id = loader.load_string(input.to_string());
    let mut tracer = VoidTracer;
    let mut evaluator = Evaluator::new(loader, &mut tracer);
    let mut type_env = rcl::typecheck::prelude();
    let mut value_env = rcl::runtime::prelude();
    let value = evaluator.eval_doc(&mut type_env, &mut value_env, id)?;
    let full_span = loader.get_span(id);
    let doc = rcl::fmt_json::format_json(full_span, &value)?;
    pprint_doc(cfg, doc, out_node);
    Ok(())
}

#[wasm_bindgen]
pub fn rcl_evaluate_json(input: &str, out_node: &Node, out_width: u32, max_len: u32) {
    let mut loader = Loader::new();
    let cfg = PrintConfig {
        width: out_width,
        max_len,
    };
    let result = rcl_evaluate_json_impl(&cfg, &mut loader, input, out_node);
    if let Err(err) = result {
        let inputs = loader.as_inputs();
        let err_doc = err.report(&inputs);
        pprint_doc(&cfg, err_doc, out_node);
    }
}

fn rcl_evaluate_value_impl(input: &str) -> Result<Value> {
    let mut loader = Loader::new();
    loader.set_filesystem(Box::new(VoidFilesystem));
    let id = loader.load_string(input.to_string());
    let mut tracer = VoidTracer;
    let mut evaluator = Evaluator::new(&mut loader, &mut tracer);
    let mut type_env = rcl::typecheck::prelude();
    let mut value_env = rcl::runtime::prelude();
    let value = evaluator.eval_doc(&mut type_env, &mut value_env, id)?;
    Ok(value)
}

#[wasm_bindgen]
pub fn rcl_evaluate_query_value(input: &str) -> *mut Value {
    let value = rcl_evaluate_value_impl(input).expect("Input should be known good.");
    // The js on the page calls this once and then holds on to the value for
    // the remainder of the page. We leak it intentionally here.
    Box::leak(Box::new(value)) as *mut Value
}

fn rcl_evaluate_query_impl<'a>(
    cfg: &'a PrintConfig,
    loader: &'a mut Loader,
    input: &'a Value,
    query: &'a str,
    out_node: &Node,
) -> Result<()> {
    loader.set_filesystem(Box::new(VoidFilesystem));
    let id = loader.load_string(query.to_string());
    let mut tracer = VoidTracer;
    let mut evaluator = Evaluator::new(loader, &mut tracer);
    let mut type_env = rcl::typecheck::prelude();
    let mut value_env = rcl::runtime::prelude();

    // Provide the value that we previously set in the environment.
    type_env.push("input".into(), rcl::typecheck::type_any().clone());
    value_env.push("input".into(), input.clone());

    let value = evaluator.eval_doc(&mut type_env, &mut value_env, id)?;
    let doc = rcl::fmt_rcl::format_rcl(&value);
    pprint_doc(cfg, doc, out_node);
    Ok(())
}

#[wasm_bindgen]
pub fn rcl_evaluate_query(
    input: *const Value,
    query: &str,
    out_node: &Node,
    out_width: u32,
    max_len: u32,
) {
    // Safety: We assume here that the caller passes the result of evaluate_query_value.
    let input_val: &Value = unsafe { &(*input) };
    let cfg = PrintConfig {
        width: out_width,
        max_len,
    };
    let mut loader = Loader::new();
    let result = rcl_evaluate_query_impl(&cfg, &mut loader, input_val, query, out_node);
    if let Err(err) = result {
        let inputs = loader.as_inputs();
        let err_doc = err.report(&inputs);
        pprint_doc(&cfg, err_doc, out_node);
    }
}
