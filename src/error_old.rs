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
