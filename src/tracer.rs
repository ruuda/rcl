// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Runtime configurable behavior for trace messages.

use crate::error::highlight_span;
use crate::fmt_rcl::format_rcl;
use crate::markup::{Markup, MarkupMode};
use crate::pprint::{self, concat, Doc};
use crate::runtime::Value;
use crate::source::{Inputs, Span};

/// Configurable behavior for trace messages.
///
/// In pure contexts (such as the fuzzer, tests, or possibly when using RCL as a
/// library), we don't want to directly emit trace messages to stdout; we want
/// to ignore them, or collect them in a log/buffer. But for evaluation on the
/// command line, we do want to emit trace messages as they happen, especially
/// to that it's possible to see where a long-running loop hangs, or why
/// evaluation takes a long time. So we make the the trace behavior configurable.
///
/// The main implementation is [`StderrTracer`]. The fuzzer uses its own void
/// tracer that ignores trace messages.
pub trait Tracer {
    fn trace(&mut self, inputs: &Inputs, span: Span, message: &Value);
}

/// Tracer that writes messages to stderr.
pub struct StderrTracer {
    config: pprint::Config,
    markup: MarkupMode,
}

impl StderrTracer {
    pub fn new(markup: Option<MarkupMode>) -> StderrTracer {
        let stderr = std::io::stderr();
        StderrTracer {
            config: pprint::Config { width: Some(80) },
            markup: markup.unwrap_or_else(|| MarkupMode::default_for_fd(&stderr)),
        }
    }
}

impl Tracer for StderrTracer {
    fn trace(&mut self, inputs: &Inputs, span: Span, message: &Value) {
        let doc = concat! {
            highlight_span(inputs, span, Markup::Trace)
            Doc::from("Trace:").with_markup(Markup::Trace)
            " "
            format_rcl(message)
            Doc::HardBreak
            Doc::HardBreak
        };
        let doc_str = doc.println(&self.config);
        let mut out = std::io::stderr().lock();
        let res = doc_str.write_bytes(self.markup, &mut out);
        if res.is_err() {
            // If we fail to print to stderr, there is no point in printing an
            // error, just exit then.
            std::process::exit(1);
        }
    }
}

/// Tracer that ignores its messages.
///
/// Intended for use by the fuzzer.
pub struct VoidTracer;

// coverage:off -- The void tracer is only used by the fuzzer, not production code.
impl Tracer for VoidTracer {
    fn trace(&mut self, _inputs: &Inputs, _span: Span, _message: &Value) {}
}
// coverage:on
