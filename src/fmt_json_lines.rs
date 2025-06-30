// RCL -- A reasonable configuration language.
// Copyright 2025 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Formatter that prints lists in JSON Lines format, one JSON value per line.

use crate::error::{IntoError, PathElement, Result};
use crate::fmt_json::Formatter;
use crate::pprint::Doc;
use crate::runtime::Value;
use crate::source::Span;

/// Render a value in JSON Lines format, one JSON value per line.
pub fn format_json_lines(caller: Span, v: &Value) -> Result<Doc<'static>> {
    let elements = match v {
        Value::List(xs) => xs,
        _ => {
            return caller
                .error("To format as JSON Lines, the top-level value must be a list.")
                .err()
        }
    };

    let mut formatter = Formatter::new(caller);
    let mut parts = Vec::new();

    for (i, element) in elements.iter().enumerate() {
        formatter.path.push(PathElement::Index(i));
        let value_doc = formatter.value(element)?;
        let wide_line = value_doc.print_wide();
        parts.push(Doc::from(wide_line));
        parts.push(Doc::HardBreak);
        formatter.path.pop();
    }

    Ok(Doc::Concat(parts))
}
