// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Formatter that prints supported values as one value per line.

use crate::error::{Error, IntoError, PathElement, Result};
use crate::fmt_rcl::format_rcl;
use crate::pprint::{concat, Doc};
use crate::runtime::Value;
use crate::source::Span;

/// Render a value in raw format.
///
/// It is a bit wasteful to go the `Doc` route, we could print directly to
/// stdout, but taking the same approach as the other formatters makes error
/// reporting easier, and output handling more uniform.
pub fn format_raw(caller: Span, v: &Value) -> Result<Doc> {
    let mut formatter = Formatter::new(caller);
    formatter.value(v)
}

/// Helper for formatting raw values.
struct Formatter {
    /// The source location where raw formatting was triggered from.
    caller: Span,
}

impl Formatter {
    pub fn new(caller: Span) -> Formatter {
        Formatter { caller }
    }

    fn error_not_string(&self, v: &Value) -> Error {
        self.caller.error(concat! {
            "Expected a string for raw output, but got non-string value: "
            format_rcl(v).into_owned()
        })
    }

    fn list<'a>(&mut self, vs: impl Iterator<Item = &'a Value>) -> Result<Doc<'a>> {
        let mut elements = Vec::with_capacity(vs.size_hint().0 * 2);
        for (i, v) in vs.enumerate() {
            match v {
                Value::String(s) => elements.push(Doc::lines(s)),
                _not_str => {
                    return self
                        .error_not_string(v)
                        .with_path(vec![PathElement::Index(i)])
                        .err();
                }
            }
            elements.push(Doc::HardBreak);
        }
        Ok(Doc::Concat(elements))
    }

    fn value<'a>(&mut self, v: &'a Value) -> Result<Doc<'a>> {
        let result: Doc = match v {
            Value::String(s) => Doc::lines(s),
            Value::List(vs) => self.list(vs.iter())?,
            Value::Set(vs) => self.list(vs.iter())?,
            _not_str => return self.error_not_string(v).err(),
        };
        Ok(result)
    }
}
