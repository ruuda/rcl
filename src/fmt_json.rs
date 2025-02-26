// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Formatter that prints values as json.

use crate::error::{IntoError, PathElement, Result};
use crate::markup::Markup;
use crate::pprint::{concat, group, indent, Doc};
use crate::runtime::Value;
use crate::source::Span;
use crate::string::escape_json;

/// Render a value as json.
pub fn format_json(caller: Span, v: &Value) -> Result<Doc> {
    let mut formatter = Formatter::new(caller);
    formatter.value(v)
}

/// Helper for formatting values as json.
///
/// The formatter tracks the path in the value that we are formatting from, such
/// that we can report the location of an error, in case an error occurs.
pub struct Formatter {
    /// The source location where json formatting was triggered from.
    pub caller: Span,

    /// Where we currently are in the value to be formatted.
    pub path: Vec<PathElement>,
}

impl Formatter {
    pub fn new(caller: Span) -> Formatter {
        Formatter {
            caller,
            path: Vec::new(),
        }
    }

    /// Report an error at the current value path.
    fn error<T>(&mut self, message: &'static str) -> Result<T> {
        // Steal the path from the formatter and move it into the error. We have
        // to leave an empty path in its place. This is fine, because returning
        // the error prevents further formatting.
        let mut path = Vec::new();
        std::mem::swap(&mut self.path, &mut path);
        self.caller.error(message).with_path(path).err()
    }

    fn string<'a>(&self, s: &str) -> Doc<'a> {
        let mut into = String::with_capacity(s.len());
        // TODO: Escape into a Doc so we can highlight escape sequences.
        escape_json(s, &mut into);
        concat! { "\"" into "\"" }
    }

    fn list<'a>(&mut self, vs: impl Iterator<Item = &'a Value>) -> Result<Doc<'a>> {
        let mut elements = Vec::new();
        for (i, v) in vs.enumerate() {
            if !elements.is_empty() {
                elements.push(",".into());
                elements.push(Doc::Sep);
            }
            self.path.push(PathElement::Index(i));
            elements.push(self.value(v)?);
            self.path.pop().expect("Push and pop are balanced.");
        }
        let result = group! {
            "["
            Doc::SoftBreak
            indent! { Doc::Concat(elements) }
            Doc::SoftBreak
            "]"
        };
        Ok(result)
    }

    fn dict<'a>(&mut self, vs: impl Iterator<Item = (&'a Value, &'a Value)>) -> Result<Doc<'a>> {
        let mut elements = Vec::new();
        for (k, v) in vs {
            if !elements.is_empty() {
                elements.push(",".into());
                elements.push(Doc::Sep);
            }
            self.path.push(PathElement::Key(k.clone()));
            match k {
                Value::String(k_str) => {
                    elements.push(self.string(k_str).with_markup(Markup::Field))
                }
                _ => return self.error("To export as json, keys must be strings."),
            };
            elements.push(": ".into());
            elements.push(self.value(v)?);
            self.path.pop().expect("Push and pop are balanced.");
        }
        let result = group! {
            "{"
            Doc::SoftBreak
            indent! { Doc::Concat(elements) }
            Doc::SoftBreak
            "}"
        };
        Ok(result)
    }

    pub fn value<'a>(&mut self, v: &'a Value) -> Result<Doc<'a>> {
        let result: Doc = match v {
            Value::Null => Doc::from("null").with_markup(Markup::Keyword),
            Value::Bool(true) => Doc::from("true").with_markup(Markup::Keyword),
            Value::Bool(false) => Doc::from("false").with_markup(Markup::Keyword),
            Value::Number(d) => Doc::from(d.format()).with_markup(Markup::Number),
            Value::String(s) => self.string(s).with_markup(Markup::String),
            Value::List(vs) => self.list(vs.iter())?,
            Value::Set(vs) => self.list(vs.iter())?,
            Value::Dict(vs) => self.dict(vs.iter())?,
            Value::Function(..) => self.error("Functions cannot be exported as json.")?,
            Value::BuiltinFunction(..) => self.error("Functions cannot be exported as json.")?,
            Value::BuiltinMethod { .. } => self.error("Methods cannot be exported as json.")?,
        };
        Ok(result)
    }
}
