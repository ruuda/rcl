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
use rcl::highlight::highlight;
use rcl::loader::{Loader, VoidFilesystem};
use rcl::markup::{Markup, MarkupString};
use rcl::pprint::{self, Doc};
use rcl::runtime::Value;
use rcl::source::Span;
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

/// Output the marked-up strings as spans into the output DOM node.
fn print_markup(max_len: u32, markup_string: &MarkupString, out_node: &Node) {
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
        if n_written > max_len {
            break;
        }
        buffer.push_str(f_str);
    }
    append_span(out_node, markup_class(markup), &buffer);

    if n_written > max_len {
        append_span(
            out_node,
            "warning",
            "...\nTruncated output to keep your browser fast.",
        );
    }
}

/// Pretty-print a document, append it as DOM nodes.
fn pprint_doc(cfg: &PrintConfig, doc: Doc, out_node: &Node) {
    let pprint_cfg = pprint::Config { width: cfg.width };
    let markup_string = doc.println(&pprint_cfg);
    print_markup(cfg.max_len, &markup_string, out_node);
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
    let body_span = loader.get_span(id);
    let doc = rcl::fmt_json::format_json(body_span, &value)?;
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

/// An edit to transform a string `before` into `after`.
///
/// An edit consists of a _delete_ followed by an _insert_:
///
/// * Delete `before[off..del]`.
/// * Insert `after[off..ins]` at `before[off]`.
#[derive(Eq, PartialEq, Debug)]
struct Edit {
    off: usize,
    /// The first index in `before` that is no longer part of the delete.
    del: usize,
    /// The first index in `after` that is no longer part of the insert.
    ins: usize,
}

impl Edit {
    /// Transform a span to move with the edit.
    #[inline(always)]
    fn apply(&self, span: Span) -> Span {
        if span.end() <= self.off {
            // Still before the edit, nothing to change here.
            return span;
        }

        if span.start() > self.del {
            // The span is entirely after the edit, we only need to move it.
            return Span::new(
                span.doc(),
                span.start() + self.ins - self.del,
                span.end() + self.ins - self.del,
            );
        }

        if span.start() >= self.off && span.end() <= self.del {
            // The delete covers this span entirely, we zero it out.
            return Span::new(span.doc(), self.off, self.off);
        }

        if span.start() <= self.off && span.end() >= self.del {
            // The delete lies entirely within this span.
            return Span::new(span.doc(), span.start(), span.end() + self.ins - self.del);
        }

        if span.start() <= self.off && span.end() <= self.del {
            // The delete covers the end of this span but not the start.
            return Span::new(span.doc(), span.start(), self.off);
        }

        if span.start() >= self.off && span.end() >= self.del {
            // The delete starts before this span, but ends inside.
            return Span::new(span.doc(), self.off, span.end() + self.ins - self.del);
        }

        unreachable!("The above cases are exhaustive.");
    }
}

/// Returns the range that has changed from `before` to `after`.
fn get_edit(before: &str, after: &str) -> Edit {
    let off = after
        .as_bytes()
        .iter()
        .zip(before.as_bytes())
        .take_while(|(cx, cy)| *cx == *cy)
        .count();

    // After the prefix, there is a new piece of this length:
    let insert_len = after.len() - off;

    // But the new piece might not be entirely new, it might share a suffix
    // with the old input.
    let suffix_len = after
        .as_bytes()
        .iter()
        .rev()
        .zip(before.as_bytes().iter().rev())
        .take_while(|(cx, cy)| *cx == *cy)
        .count()
        .min(insert_len);

    let insert_len = insert_len - suffix_len;
    let delete_len = before.len() - (off + suffix_len);

    Edit {
        off,
        del: off + delete_len,
        ins: off + insert_len,
    }
}

/// Run the lexer, apply syntax highlighting, output into `out_node`.
///
/// It might happen that the lexer fails with a parse error. In particular, this
/// happens on unbalanced brackets and unclosed strings. If we don't highlight
/// at all if the input contains an error, that’s not a great user experience.
/// As a hack to improve that a little, we also accept a known-good input. If
/// lexing fails, we take the known-good input and use unstyled tokens for the
/// changes.
///
/// Returns whether the current input is good (and could be used as a known-good
/// input for a next input).
#[wasm_bindgen]
pub fn rcl_highlight(input: &str, good_input: &str, out_node: &Node) -> bool {
    use rcl::lexer::Token;

    let mut loader = Loader::new();
    loader.set_filesystem(Box::new(VoidFilesystem));

    let id = loader.load_string(input.to_string());
    let mut is_good = true;

    let tokens = match loader.get_tokens(id) {
        Ok(ts) => ts,
        Err(..) => {
            is_good = false;
            let id_good = loader.load_string(good_input.to_string());
            let mut tokens = loader.get_tokens(id_good).expect("Good input is lexable.");

            let edit = get_edit(good_input, input);
            for (_token, span) in tokens.iter_mut() {
                *span = edit.apply(*span);
            }

            if edit.off == good_input.len() {
                // Inserts modify the span after where the insert starts. If we
                // append to the document, there is no such span, so this case
                // needs special handling.
                tokens.push((Token::Space, Span::new(id_good, edit.off, edit.ins)));
            }

            tokens
        }
    };

    let result = highlight(&tokens, loader.get_doc(id).data);

    // For highlighting input, not output, it should not be so easy to
    // accidentally create a very long document, so the input is big,
    // it's probably intentional, then let's use a "big" limit.
    let max_len = 128 * 4096;
    print_markup(max_len, &result, out_node);

    is_good
}

#[cfg(test)]
mod wasm_test {
    use super::{get_edit, Edit};
    use rcl::source::{DocId, Span};
    const D: DocId = DocId(0);

    fn apply_edit(edit: Edit, spans: &mut [Span]) {
        for span in spans.iter_mut() {
            *span = edit.apply(*span);
        }
    }

    #[test]
    fn edit_apply_delete_inner() {
        let edit = get_edit("AAAABBBBCCCC", "AAAABBCCCC");
        let mut spans = [Span::new(D, 0, 4), Span::new(D, 4, 8), Span::new(D, 8, 12)];
        let expected_ = [Span::new(D, 0, 4), Span::new(D, 4, 6), Span::new(D, 6, 10)];
        apply_edit(edit, &mut spans);
        assert_eq!(spans, expected_);
    }

    #[test]
    fn edit_apply_delete_entire() {
        let edit = get_edit("AAAABBBBCCCC", "AAAACCCC");
        let mut spans = [Span::new(D, 0, 4), Span::new(D, 4, 8), Span::new(D, 8, 12)];
        let expected_ = [Span::new(D, 0, 4), Span::new(D, 4, 4), Span::new(D, 4, 8)];
        apply_edit(edit, &mut spans);
        assert_eq!(spans, expected_);
    }

    #[test]
    fn edit_apply_delete_across() {
        let edit = get_edit("AAAABBBBCCCC", "AABBCCCC");
        assert_eq!(
            edit,
            Edit {
                off: 2,
                del: 6,
                ins: 2
            }
        );
        let mut spans = [Span::new(D, 0, 4), Span::new(D, 4, 8), Span::new(D, 8, 12)];
        let expected_ = [Span::new(D, 0, 2), Span::new(D, 2, 4), Span::new(D, 4, 8)];
        apply_edit(edit, &mut spans);
        assert_eq!(spans, expected_);
    }

    #[test]
    fn edit_apply_insert_inner() {
        let edit = get_edit("AAAABBBBCCCC", "AAAABBVVBBCCCC");
        let mut spans = [Span::new(D, 0, 4), Span::new(D, 4, 8), Span::new(D, 8, 12)];
        let expected_ = [
            Span::new(D, 0, 4),
            Span::new(D, 4, 10),
            Span::new(D, 10, 14),
        ];
        apply_edit(edit, &mut spans);
        assert_eq!(spans, expected_);
    }

    #[test]
    fn edit_apply_insert_boundary() {
        // An insert between two spans extends the start of the span following
        // the insert, not the end of the one before it.
        let edit = get_edit("AAAABBBBCCCC", "AAAAVVBBBBCCCC");
        assert_eq!(
            edit,
            Edit {
                off: 4,
                del: 4,
                ins: 6,
            }
        );
        let mut spans = [Span::new(D, 0, 4), Span::new(D, 4, 8), Span::new(D, 8, 12)];
        let expected_ = [
            Span::new(D, 0, 4),
            Span::new(D, 4, 10),
            Span::new(D, 10, 14),
        ];
        apply_edit(edit, &mut spans);
        assert_eq!(spans, expected_);
    }

    #[test]
    fn edit_apply_replace_inner() {
        let edit = get_edit("AAAABBBBCCCC", "AAAABXXBCCCC");
        let mut spans = [Span::new(D, 0, 4), Span::new(D, 4, 8), Span::new(D, 8, 12)];
        let expected_ = [Span::new(D, 0, 4), Span::new(D, 4, 8), Span::new(D, 8, 12)];
        apply_edit(edit, &mut spans);
        assert_eq!(spans, expected_);
    }

    #[test]
    fn edit_apply_replace_exact() {
        let edit = get_edit("AAAABBBBCCCC", "AAAA----CCCC");
        let mut spans = [Span::new(D, 0, 4), Span::new(D, 4, 8), Span::new(D, 8, 12)];
        let expected_ = [Span::new(D, 0, 4), Span::new(D, 4, 4), Span::new(D, 4, 12)];
        apply_edit(edit, &mut spans);
        assert_eq!(spans, expected_);
    }

    #[test]
    fn edit_apply_replace_outer() {
        let edit = get_edit("AAAABBBBCCCC", "AAA<---->CCC");
        let mut spans = [Span::new(D, 0, 4), Span::new(D, 4, 8), Span::new(D, 8, 12)];
        let expected_ = [Span::new(D, 0, 3), Span::new(D, 3, 3), Span::new(D, 3, 12)];
        apply_edit(edit, &mut spans);
        assert_eq!(spans, expected_);
    }

    // TODO: Write a fuzz test for this part.
}
