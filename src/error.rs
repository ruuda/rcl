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
pub struct Error {
    message: Doc<'static>,
    origin: Option<Span>,
    notes: Vec<(Span, Doc<'static>)>,
    help: Option<Doc<'static>>,
}

impl Error {
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
            result.push(highlight_span(inputs, note_span, Markup::Warning));
            result.push(Doc::from("Note:").with_markup(Markup::Warning));
            result.push(" ".into());
            result.push(note_message);
        }

        if let Some(help_message) = self.help {
            result.push(Doc::from("Help:").with_markup(Markup::Warning));
            result.push(" ".into());
            result.push(help_message);
        }

        Doc::Concat(result)
    }
}

/// Highlight a span in a line.
pub fn highlight_span<'a>(_inputs: &'a Inputs, span: Span, _markup: Markup) -> Doc<'a> {
    use crate::pprint::concat;
    concat! {
        // TODO: Do the real source reporting, port over from error_old.
        Doc::from(format!("{span:?}"))
        Doc::HardBreak
    }
}

// For backwards compatibility while we migrate all errors.
impl From<Box<dyn error_old::Error>> for Error {
    fn from(err: Box<dyn error_old::Error>) -> Error {
        Error {
            message: err.message().to_string().into(),
            origin: err.span(),
            notes: err
                .notes()
                .iter()
                .map(|(span, msg)| (*span, msg.to_string().into()))
                .collect(),
            help: err.help().map(|s| s.to_string().into()),
        }
    }
}
