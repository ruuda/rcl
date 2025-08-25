// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Types and functions for error reporting.

use crate::fmt_rcl::format_rcl;
use crate::markup::Markup;
use crate::pprint::{concat, Doc};
use crate::runtime::Value;
use crate::source::{Inputs, Span};

pub type Result<T> = std::result::Result<T, Box<Error>>;

/// Element of a path through a value.
// TODO: Record the value itself as well, so we can *show* the thing that's wrong.
#[derive(Debug)]
pub enum PathElement {
    Key(Value),
    Index(usize),
}

/// Any type of error that occurred in the program.
///
/// Errors use [`Doc`] for pretty-printing them. This enables a few things:
///
/// * Coloring and whether to enable or disable it can be handled by the
///   existing pretty-printer.
/// * Values that can be pretty-printed can be embedded in error messages, and
///   they will be line-wrapped and colored appropriately.
///
/// Most `Error`s are constructed from a `Span` using [`IntoError::error`], and
/// optionally augmented using one of the `with_*` methods.
///
/// ## Historical Note
///
/// In the past we represented different types of errors as different structs,
/// with a common `Error` trait, but in practice they were all very similar, and
/// we don't need to pattern match on them or handle them, these are intended
/// for the end user. So we merge all of them into a single error type, but
/// preserve enough of the structure to allow for alternative reporting formats
/// (e.g. json output) to enable tooling.
#[derive(Debug)]
pub struct Error {
    /// The main message of the error.
    ///
    ///  * Shorter is better.
    ///  * Simpler is better (no jargon).
    ///  * The expected thing goes first, the actual thing goes second.
    pub message: Doc<'static>,

    /// Optionally an extended message body.
    ///
    /// For example, in the case of a type error, the body could show the
    /// expected type and actual type. We separate the body from the main
    /// message to have a title in case we need to report structured errors
    /// in limited form (e.g. GitHub Actions source annotations).
    pub body: Option<Doc<'static>>,

    /// The source location of the error.
    pub origin: Option<Span>,

    /// The call stack around the origin of the error.
    pub call_stack: Vec<(Span, Doc<'static>)>,

    /// For errors that originated from a value, the path in the value.
    pub path: Vec<PathElement>,

    /// Any other relevant spans.
    ///
    /// For example, an unmatched parenthesis can point to the opening paren.
    pub notes: Vec<(Span, Doc<'static>)>,

    /// Additional information, or a hint on how to fix the problem.
    ///
    /// For example, when the user writes a `#`, we can explain that comments
    /// are written with `//` instead.
    pub help: Option<Doc<'static>>,
}

impl Error {
    /// Create a new error, with nothing but the message set.
    pub fn new<M>(message: M) -> Error
    where
        Doc<'static>: From<M>,
    {
        Error {
            message: message.into(),
            body: None,
            origin: None,
            call_stack: Vec::new(),
            path: Vec::new(),
            notes: Vec::new(),
            help: None,
        }
    }

    /// Replace the origin of the error with the given span.
    pub fn with_origin(mut self, origin: Span) -> Error {
        self.origin = Some(origin);
        self
    }

    /// Replace the body of the error with the given content.
    pub fn with_body<M>(mut self, body: M) -> Error
    where
        Doc<'static>: From<M>,
    {
        self.body = Some(body.into());
        self
    }

    /// Append a frame to the error's call stack.
    pub fn add_call_frame<M>(&mut self, at: Span, message: M)
    where
        Doc<'static>: From<M>,
    {
        self.call_stack.push((at, message.into()));
    }

    /// Replace the message at the top of the call stack.
    ///
    /// The span that is currently at the top (if there is one at all) must
    /// match exactly to replace the call frame. If not, then we push the frame
    /// like normal without popping.
    pub fn replace_call_frame<M>(&mut self, at: Span, message: M)
    where
        Doc<'static>: From<M>,
    {
        match self.call_stack.last() {
            Some(frame) if frame.0 == at => _ = self.call_stack.pop(),
            _ => {}
        }
        self.add_call_frame(at, message)
    }

    /// Append a frame to the error's call stack.
    pub fn with_call_frame<M>(mut self, at: Span, message: M) -> Error
    where
        Doc<'static>: From<M>,
    {
        self.add_call_frame(at, message);
        self
    }

    /// Extend the error with a note at a given source location.
    pub fn add_note<M>(&mut self, at: Span, note: M)
    where
        Doc<'static>: From<M>,
    {
        self.notes.push((at, note.into()));
    }

    /// Extend the error with a note at a given source location.
    pub fn with_note<M>(mut self, at: Span, note: M) -> Error
    where
        Doc<'static>: From<M>,
    {
        self.add_note(at, note);
        self
    }

    /// Replace the help message of the error with a given message.
    pub fn set_help<M>(&mut self, help: M)
    where
        Doc<'static>: From<M>,
    {
        self.help = Some(help.into());
    }

    /// Replace the help message of the error with a given message.
    pub fn with_help<M>(mut self, help: M) -> Error
    where
        Doc<'static>: From<M>,
    {
        self.set_help(help);
        self
    }

    /// Replace the value path with the given path.
    pub fn with_path(mut self, path: Vec<PathElement>) -> Error {
        self.path = path;
        self
    }

    /// Append the path element to the value path.
    pub fn with_path_element(mut self, element: PathElement) -> Error {
        self.path.push(element);
        self
    }

    /// Wrap the error in a `Result::Err`.
    pub fn err<T>(self) -> Result<T> {
        Err(Box::new(self))
    }

    fn report_path(&self) -> Doc<'static> {
        // TODO: Find a prettier way to report the value path of an error.
        // For now this will do.
        if self.path.is_empty() {
            return Doc::Empty;
        }
        let mut path_doc = vec![Doc::from("in value"), Doc::HardBreak];
        for elem in self.path.iter().rev() {
            match elem {
                PathElement::Key(k) => {
                    path_doc.push("at key ".into());
                    path_doc.push(format_rcl(k).into_owned());
                }
                PathElement::Index(i) => {
                    let v = i.to_string();
                    path_doc.push("at index ".into());
                    path_doc.push(Doc::from(v).with_markup(Markup::Number));
                }
            }
            path_doc.push(Doc::HardBreak);
        }
        Doc::Concat(path_doc)
    }

    /// Format the error into a [`Doc`] that can be printed to stderr.
    pub fn report<'a>(self, inputs: &'a Inputs) -> Doc<'a> {
        let mut result = Vec::new();

        if let Some(span) = self.origin {
            result.push(highlight_span(inputs, span, Markup::Error))
        }

        result.push(self.report_path());

        result.push(Doc::from("Error:").with_markup(Markup::Error));
        result.push(" ".into());
        result.push(self.message);

        if let Some(body) = self.body {
            result.push(" ".into());
            result.push(body);
        }

        for (note_span, note_message) in self.notes {
            result.push(Doc::HardBreak);
            result.push(Doc::HardBreak);
            result.push(highlight_span(inputs, note_span, Markup::Warning));
            result.push(Doc::from("Note:").with_markup(Markup::Warning));
            result.push(" ".into());
            result.push(note_message);
        }

        if let Some(help_message) = self.help {
            result.push(Doc::HardBreak);
            result.push(Doc::HardBreak);
            result.push(Doc::from("Help:").with_markup(Markup::Warning));
            result.push(" ".into());
            result.push(help_message);
        }

        // We print the call stack last, after the help and notes, because the
        // help and notes refer to the inner error. If the call stack is
        // enormous, that probably means the error is exceeding the call depth
        // (stack overflow), so truncate it.
        let mut call_stack = self.call_stack;
        let truncate = call_stack.len() > 5;
        if truncate {
            call_stack.truncate(5);
        }
        for (call_span, call_frame_message) in call_stack {
            result.push(Doc::HardBreak);
            result.push(Doc::HardBreak);
            result.push(highlight_span(inputs, call_span, Markup::Error));
            result.push(call_frame_message);
        }
        if truncate {
            result.push(Doc::HardBreak);
            result.push(Doc::HardBreak);
            result.push(Doc::from("Note:").with_markup(Markup::Warning));
            result.push(
                " The call stack is too deep to display in full. \
                Only the innermost calls are shown above."
                    .into(),
            );
        }

        Doc::Concat(result)
    }
}

pub trait IntoError {
    fn error<M>(self, message: M) -> Error
    where
        Doc<'static>: From<M>;
}

impl IntoError for Span {
    fn error<M>(self, message: M) -> Error
    where
        Doc<'static>: From<M>,
    {
        Error {
            origin: Some(self),
            ..Error::new(message)
        }
    }
}

/// Highlight a span in a line.
pub fn highlight_span<'a>(inputs: &'a Inputs, span: Span, markup: Markup) -> Doc<'a> {
    use std::cmp;
    use unicode_width::UnicodeWidthStr;

    let doc = &inputs[span.doc()];
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
    // start below. Add 1 because the first column is column 1, not 0.
    // TODO: This should measure width, not count bytes.
    let column = 1 + span.start() - line_start;

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
    let mark_under: String = "~".repeat(mark_width - 1);
    let doc_under = concat! { "^" mark_under };

    concat! {
        doc.name format!(":{line}:{column}")
        Doc::HardBreak
        line_num_pad.clone() " " Doc::from("╷").with_markup(markup)
        Doc::HardBreak
        line_num_str " " Doc::from("│").with_markup(markup) " " trunc_prefix line_content trunc_suffix
        Doc::HardBreak
        line_num_pad " " Doc::from("╵").with_markup(markup) " " mark_indent doc_under.with_markup(markup)
        Doc::HardBreak
    }
}
