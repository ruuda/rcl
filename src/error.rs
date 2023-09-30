// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Types and functions for error reporting.

use crate::error_old;
use crate::markup::Markup;
use crate::pprint::Doc;
use crate::source::{Inputs, Span};

pub type Result<T> = std::result::Result<T, Box<Error>>;

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
    pub message: Doc<'static>,
    pub origin: Option<Span>,
    pub notes: Vec<(Span, Doc<'static>)>,
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
            origin: None,
            notes: Vec::new(),
            help: None,
        }
    }
    /// Extend the error with a note at a given source location.
    pub fn with_note<M>(mut self, at: Span, note: M) -> Error
    where
        Doc<'static>: From<M>,
    {
        self.notes.push((at, note.into()));
        self
    }

    /// Replace the help message of the error with a given message.
    pub fn with_help<M>(mut self, help: M) -> Error
    where
        Doc<'static>: From<M>,
    {
        self.help = Some(help.into());
        self
    }

    /// Wrap the error in a `Result::Err`.
    pub fn err<T>(self) -> Result<T> {
        Err(Box::new(self))
    }

    /// Format the error into a [`Doc`] that can be printed to stderr.
    pub fn report<'a>(self, inputs: &'a Inputs) -> Doc<'a> {
        let mut result = Vec::new();

        if let Some(span) = self.origin {
            result.push(highlight_span(inputs, span, Markup::Error))
        }

        result.push(Doc::from("Error:").with_markup(Markup::Error));
        result.push(" ".into());
        result.push(self.message);

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

    use crate::pprint::concat;

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

// For backwards compatibility while we migrate all errors.
impl From<Box<dyn error_old::Error>> for Box<Error> {
    fn from(err: Box<dyn error_old::Error>) -> Box<Error> {
        fn mk_doc(msg: &str) -> Doc<'static> {
            let mut result = Vec::new();
            for line in msg.lines() {
                if !result.is_empty() {
                    result.push(Doc::HardBreak);
                }
                result.push(line.to_string().into());
            }
            Doc::Concat(result)
        }
        let err = Error {
            message: mk_doc(err.message()),
            origin: err.span(),
            notes: err
                .notes()
                .iter()
                .map(|(span, msg)| (*span, mk_doc(msg)))
                .collect(),
            help: err.help().map(mk_doc),
        };
        Box::new(err)
    }
}
