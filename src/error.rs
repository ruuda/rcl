// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Error types.

use crate::source::{Inputs, Span};

pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

pub trait Error: std::fmt::Debug {
    /// The source location of the error.
    fn span(&self) -> Span;

    /// The error message.
    ///
    ///  * Shorter is better.
    ///  * Simpler is better (no jargon).
    ///  * The expected thing goes first, the actual thing goes second.
    fn message(&self) -> &str;

    /// Optionally, a note about error.
    ///
    /// For example, an unmatched parenthesis can point to the opening paren.
    fn notes(&self) -> &[(Span, &str)];

    /// Optionally, additional information, or a hint on how to fix the problem.
    fn help(&self) -> Option<&str>;
}

impl dyn Error {
    pub fn print(&self, inputs: &Inputs) {
        let bold_red = "\x1b[31;1m";
        let bold_yellow = "\x1b[33;1m";
        let reset = "\x1b[0m";

        let highlight = highlight_span_in_line(inputs, self.span(), bold_red);
        eprint!("{}", highlight);
        eprintln!("{}Error:{} {}", bold_red, reset, self.message());

        for (note_span, note) in self.notes() {
            let highlight = highlight_span_in_line(inputs, *note_span, bold_yellow);
            eprint!("\n{}", highlight);
            eprintln!("{}Note:{} {}", bold_yellow, reset, note);
        }

        if let Some(help) = self.help() {
            eprintln!("\n{}Help:{} {}", bold_yellow, reset, help);
        }
    }
}

fn highlight_span_in_line(inputs: &Inputs, span: Span, highlight_ansi: &str) -> String {
    use std::cmp;
    use std::fmt::Write;
    use unicode_width::UnicodeWidthStr;

    let doc = &inputs[span.doc().0 as usize];
    let input = doc.data;

    // Locate the line that contains the error.
    let mut line = 1;
    let mut line_start = 0;
    let mut line_end = 0;
    for (&c, i) in input.as_bytes().iter().zip(0..) {
        if i == span.start() {
            break;
        }
        if c == b'\n' {
            line += 1;
            line_start = i + 1;
        }
    }
    for (&c, i) in input.as_bytes()[line_start..].iter().zip(line_start..) {
        if c == b'\n' {
            line_end = i;
            break;
        }
    }
    if line_end <= line_start {
        line_end = input.len();
    }

    // Save this for reporting the error location, in case we adjust the line
    // start below. TODO: It has an off by one, column should start counting at
    // 1. But I need to rewrite all the goldens if I change this, better to do
    // it later.
    let column = span.start() - line_start;

    // If there is a really long line (for example, because you are evaluating
    // a multi-megabyte json document that is formatted without whitespace, on
    // a single line), then printing the error to the terminal is going to line
    // wrap that enormous content and not do anything productive. In that case,
    // we would rather just truncate. If we do truncate, add an ellipsis to
    // clarify that we did.
    let mut trunc_prefix = "";
    let mut trunc_suffix = "";
    if span.start() - line_start > 100 {
        line_start = span.start() - 20;
        // Ensure we don't slice code points in half. Slightly nicer would be
        // to not slice grapheme clusters in half (and also measure whether the
        // line is long from its width, not in bytes), but that is way overkill
        // for a fringe case like this.
        while !input.is_char_boundary(line_start) {
            line_start -= 1;
        }
        trunc_prefix = "…";
    }
    if line_end > span.end() + 100 {
        line_end = span.end() + 20;
        while !input.is_char_boundary(line_end) {
            line_end += 1;
        }
        trunc_suffix = "…";
    }

    let line_content = &input[line_start..line_end];

    // The length of the mark can be longer than the line, for example when
    // token to mark was a multiline string literal. In that case, highlight
    // only up to the newline, don't extend the tildes too far.
    let indent_content = &line_content[..span.start() - line_start];
    let as_of_error = &line_content[span.start() - line_start..];
    let error_content = &as_of_error[..cmp::min(span.len(), as_of_error.len())];

    // The width of the error is not necessarily the number of bytes,
    // measure the Unicode width of the span to underline.
    let indent_width = indent_content.width() + trunc_prefix.width();
    let mark_width = cmp::max(1, error_content.width());

    let line_num_str = line.to_string();
    let line_num_pad: String = line_num_str.chars().map(|_| ' ').collect();
    let mark_indent: String = " ".repeat(indent_width);
    let mark_under: String = "~".repeat(mark_width);

    let reset = "\x1b[0m";

    let mut result = String::new();
    // Note, the unwraps here are safe because writing to a string does not fail.
    writeln!(
        &mut result,
        "{}--> {}:{}:{}",
        line_num_pad, doc.name, line, column,
    )
    .unwrap();
    writeln!(&mut result, "{} |", line_num_pad).unwrap();
    writeln!(
        &mut result,
        "{} | {}{}{}",
        line_num_str, trunc_prefix, line_content, trunc_suffix
    )
    .unwrap();
    writeln!(
        &mut result,
        "{} | {}{}^{}{}",
        line_num_pad,
        mark_indent,
        highlight_ansi,
        &mark_under[1..],
        reset
    )
    .unwrap();

    result
}

/// We encountered some IO failure.
#[derive(Debug)]
pub struct IoError {
    // TODO: Make optional.
    pub span: Span,
    pub message: String,
}

impl IoError {
    pub fn new(message: String) -> IoError {
        IoError {
            span: Span::new(crate::source::DocId(0), 0, 0),
            message,
        }
    }
}

impl From<String> for IoError {
    fn from(err: String) -> Self {
        IoError::new(err)
    }
}

impl From<&str> for IoError {
    fn from(err: &str) -> Self {
        IoError::new(err.to_string())
    }
}

impl From<IoError> for Box<dyn Error> {
    fn from(err: IoError) -> Self {
        Box::new(err)
    }
}

impl Error for IoError {
    fn span(&self) -> Span {
        self.span
    }
    fn message(&self) -> &str {
        &self.message
    }
    fn notes(&self) -> &[(Span, &str)] {
        &[]
    }
    fn help(&self) -> Option<&str> {
        None
    }
}

/// A syntax error that causes lexing or parsing to fail.
#[derive(Debug)]
pub struct ParseError {
    pub span: Span,
    pub message: &'static str,
    pub note: Option<(Span, &'static str)>,
    pub help: Option<&'static str>,
}

impl ParseError {
    pub fn with_help(mut self, help: &'static str) -> Self {
        self.help = Some(help);
        self
    }

    pub fn with_note(mut self, at: Span, note: &'static str) -> Self {
        self.note = Some((at, note));
        self
    }
}

impl From<ParseError> for Box<dyn Error> {
    fn from(err: ParseError) -> Self {
        Box::new(err)
    }
}

impl Error for ParseError {
    fn span(&self) -> Span {
        self.span
    }
    fn message(&self) -> &str {
        self.message
    }
    fn notes(&self) -> &[(Span, &str)] {
        match self.note {
            Some(ref n) => std::slice::from_ref(n),
            None => &[],
        }
    }
    fn help(&self) -> Option<&str> {
        self.help
    }
}

pub trait IntoParseError {
    fn error(&self, message: &'static str) -> ParseError;
}

impl IntoParseError for Span {
    fn error(&self, message: &'static str) -> ParseError {
        ParseError {
            span: *self,
            message,
            note: None,
            help: None,
        }
    }
}

/// An error during evaluation.
#[derive(Debug)]
pub struct RuntimeError {
    pub span: Span,
    pub message: &'static str,
    pub notes: Vec<(Span, &'static str)>,
    pub help: Option<&'static str>,
}

impl RuntimeError {
    pub fn with_help(mut self, help: &'static str) -> Self {
        self.help = Some(help);
        self
    }

    pub fn with_note(mut self, at: Span, note: &'static str) -> Self {
        self.notes.push((at, note));
        self
    }
}

impl From<RuntimeError> for Box<dyn Error> {
    fn from(err: RuntimeError) -> Self {
        Box::new(err)
    }
}

impl Error for RuntimeError {
    fn span(&self) -> Span {
        self.span
    }
    fn message(&self) -> &str {
        self.message
    }
    fn notes(&self) -> &[(Span, &str)] {
        &self.notes[..]
    }
    fn help(&self) -> Option<&str> {
        self.help
    }
}

pub trait IntoRuntimeError {
    fn error(&self, message: &'static str) -> RuntimeError;
}

impl IntoRuntimeError for Span {
    fn error(&self, message: &'static str) -> RuntimeError {
        RuntimeError {
            span: *self,
            message,
            notes: Vec::new(),
            help: None,
        }
    }
}

/// A value cannot be represented in the desired way.
///
/// For example, when trying to serialize a function as json.
#[derive(Debug)]
pub struct ValueError {
    /// The span where serialization was requested.
    ///
    /// This is not the span where the offending value was produced, as that can
    /// be difficult to track in general.
    pub span: Span,
    pub message: &'static str,
    // TODO: This may be expanded with a "call stack" of where in the value we are.
}

impl From<ValueError> for Box<dyn Error> {
    fn from(err: ValueError) -> Self {
        Box::new(err)
    }
}

impl Error for ValueError {
    fn span(&self) -> Span {
        self.span
    }
    fn message(&self) -> &str {
        self.message
    }
    fn notes(&self) -> &[(Span, &str)] {
        &[]
    }
    fn help(&self) -> Option<&str> {
        None
    }
}
