// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Formatter that prints list elements prefixed by `---` YAML document separators.

use crate::error::{IntoError, PathElement, Result};
use crate::fmt_json::Formatter;
use crate::markup::Markup;
use crate::pprint::Doc;
use crate::runtime::Value;
use crate::source::Span;

/// Render a value in YAML stream format.
pub fn format_yaml_stream(caller: Span, v: &Value) -> Result<Doc> {
    let elements = match v {
        Value::List(xs) => xs,
        _ => {
            return caller
                .error("To format as YAML stream, the top-level value must be a list.")
                .err()
        }
    };

    let mut formatter = Formatter::new(caller);
    let mut parts = Vec::new();

    for (i, element) in elements.iter().enumerate() {
        formatter.path.push(PathElement::Index(i));
        parts.push(Doc::str("---").with_markup(Markup::Comment));
        parts.push(Doc::HardBreak);
        parts.push(formatter.value(element)?);
        parts.push(Doc::HardBreak);
        formatter.path.pop();
    }

    Ok(Doc::Concat(parts))
}
