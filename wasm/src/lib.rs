// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use rcl::error::Result;
use rcl::eval::Evaluator;
use rcl::loader::{Loader, VoidFilesystem};
use rcl::markup::Markup;
use rcl::pprint::{self, Doc};
use rcl::tracer::VoidTracer;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
extern "C" {
    pub type Node;

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

/// Pretty-print a document, append it as DOM nodes.
fn pprint_doc(doc: Doc, out_node: &Node) {
    // TODO: Make the print width configurable.
    let cfg = pprint::Config { width: 60 };
    let markup_string = doc.println(&cfg);
    let mut markup = Markup::None;
    let mut buffer = String::new();
    for (f_str, f_markup) in markup_string.fragments.iter() {
        if markup != *f_markup {
            append_span(out_node, markup_class(markup), &buffer);
            buffer.clear();
            markup = *f_markup;
        }
        buffer.push_str(f_str);
    }
    append_span(out_node, markup_class(markup), &buffer);
}

fn rcl_evaluate_impl<'a>(loader: &'a mut Loader, input: &'a str, out_node: &Node) -> Result<()> {
    loader.set_filesystem(Box::new(VoidFilesystem));
    let id = loader.load_string(input.to_string());
    let mut tracer = VoidTracer;
    let mut evaluator = Evaluator::new(loader, &mut tracer);
    let mut type_env = rcl::typecheck::prelude();
    let mut value_env = rcl::runtime::prelude();
    let value = evaluator.eval_doc(&mut type_env, &mut value_env, id)?;
    let full_span = loader.get_span(id);
    // TODO: Make output format configurable.
    let doc = rcl::fmt_json::format_json(full_span, &value)?;
    pprint_doc(doc, out_node);
    Ok(())
}

#[wasm_bindgen]
pub fn rcl_evaluate(input: &str, out_node: &Node) {
    let mut loader = Loader::new();
    let result = rcl_evaluate_impl(&mut loader, input, out_node);
    if let Err(err) = result {
        let inputs = loader.as_inputs();
        let err_doc = err.report(&inputs);
        pprint_doc(err_doc, out_node);
    }
}
