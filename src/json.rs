// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Conversion from and to json.

use std::rc::Rc;

use crate::error::{PathElement, Result, ValueError};
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
struct Formatter {
    /// The source location where json formatting was triggered from.
    caller: Span,

    /// Where we currently are in the value to be formatted.
    path: Vec<PathElement>,
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

        let err = ValueError::new(self.caller, path, message);
        Err(err.into())
    }

    fn list<'a>(&mut self, vs: impl Iterator<Item = &'a Rc<Value>>) -> Result<Doc<'a>> {
        let mut elements = Vec::new();
        let mut is_first = true;
        for (i, v) in vs.enumerate() {
            if !is_first {
                elements.push(",".into());
                elements.push(Doc::Sep);
            }
            self.path.push(PathElement::Index(i));
            elements.push(self.value(v)?);
            self.path.pop().expect("Push and pop are balanced.");
            is_first = false;
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

    fn object<'a>(
        &mut self,
        vs: impl Iterator<Item = (&'a Rc<Value>, &'a Rc<Value>)>,
    ) -> Result<Doc<'a>> {
        let mut elements = Vec::new();
        let mut is_first = true;
        for (k, v) in vs {
            if !is_first {
                elements.push(",".into());
                elements.push(Doc::Sep);
            }
            match k.as_ref() {
                Value::String(k_str) => {
                    self.path.push(PathElement::Key(k_str.clone()));
                    elements.push(self.value(k)?)
                }
                // TODO: Include information about the encountered type.
                _ => return self.error("To export as json, keys must be strings."),
            };
            elements.push(": ".into());
            elements.push(self.value(v)?);
            self.path.pop().expect("Push and pop are balanced.");
            is_first = false;
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

    fn value<'a>(&mut self, v: &'a Value) -> Result<Doc<'a>> {
        let result: Doc = match v {
            Value::Null => "null".into(),
            Value::Bool(true) => "true".into(),
            Value::Bool(false) => "false".into(),
            Value::Int(i) => i.to_string().into(),
            Value::String(s) => {
                let mut into = String::with_capacity(s.len());
                escape_json(s, &mut into);
                concat! { "\"" into "\"" }
            }
            Value::List(vs) => self.list(vs.iter())?,
            Value::Set(vs) => self.list(vs.iter())?,
            Value::Map(vs) => self.object(vs.iter())?,
            Value::Builtin(..) => self.error("Functions cannot be exported as json.")?,
        };
        Ok(result)
    }
}
