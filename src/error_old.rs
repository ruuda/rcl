// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Error types.

use std::rc::Rc;

use crate::source::Span;

pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

pub trait Error: std::fmt::Debug {
    /// The error message.
    ///
    ///  * Shorter is better.
    ///  * Simpler is better (no jargon).
    ///  * The expected thing goes first, the actual thing goes second.
    fn message(&self) -> &str;

    /// The source location of the error.
    fn span(&self) -> Option<Span>;

    /// Optionally, a note about error.
    ///
    /// For example, an unmatched parenthesis can point to the opening paren.
    fn notes(&self) -> &[(Span, &str)];

    /// Optionally, additional information, or a hint on how to fix the problem.
    fn help(&self) -> Option<&str>;
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

    /// Wrap the error in a `Result::Err`.
    pub fn err<T>(self) -> std::result::Result<T, Self> {
        Err(self)
    }
}

impl From<ParseError> for Box<dyn Error> {
    fn from(err: ParseError) -> Self {
        Box::new(err)
    }
}

impl Error for ParseError {
    fn message(&self) -> &str {
        self.message
    }
    fn span(&self) -> Option<Span> {
        Some(self.span)
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
    fn message(&self) -> &str {
        self.message
    }
    fn span(&self) -> Option<Span> {
        Some(self.span)
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

/// Element of a path through a value.
#[derive(Debug)]
pub enum PathElement {
    Key(Rc<str>),
    Index(usize),
}

/// A value cannot be represented in the desired way.
///
/// For example, when trying to serialize a function as json.
#[derive(Debug)]
pub struct ValueError {
    /// The span where the failure was triggered.
    ///
    /// This is not the span where the offending value was produced, as that can
    /// be difficult to track in general.
    pub span: Span,

    /// Path in the value where the error originated.
    pub path: Vec<PathElement>,

    /// Description of what went wrong, but not where.
    pub description: &'static str,

    /// Combined description and path.
    pub message: String,
}

impl ValueError {
    pub fn new(span: Span, path: Vec<PathElement>, description: &'static str) -> ValueError {
        let mut message = description.to_string();

        // TODO: Find a prettier way to report the value path of an error.
        // For now this will do.
        for elem in path.iter().rev() {
            match elem {
                PathElement::Key(k) => {
                    message.push_str("\n  at key \"");
                    crate::string::escape_json(k, &mut message);
                    message.push('"');
                }
                PathElement::Index(i) => {
                    message.push_str("\n  at index ");
                    message.push_str(&i.to_string());
                }
            }
        }

        ValueError {
            span,
            path,
            description,
            message,
        }
    }
}

impl From<ValueError> for Box<dyn Error> {
    fn from(err: ValueError) -> Self {
        Box::new(err)
    }
}

impl Error for ValueError {
    fn message(&self) -> &str {
        &self.message[..]
    }
    fn span(&self) -> Option<Span> {
        Some(self.span)
    }
    fn notes(&self) -> &[(Span, &str)] {
        &[]
    }
    fn help(&self) -> Option<&str> {
        None
    }
}
