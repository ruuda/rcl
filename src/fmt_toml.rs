// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Formatter that prints values as TOML.
//!
//! This formatter is similar to the one in [`fmt_json`].

use crate::error::Result;
use crate::markup::Markup;
use crate::pprint::{concat, group, indent, Doc};
use crate::runtime::Value;
use crate::source::Span;
use crate::string::escape_json;

/// Render a value as TOML.
pub fn format_toml(_span: Span, v: &Value) -> Result<Doc> {
    Ok(value(v))
}

/// Format a string.
fn string<'a>(s: &str) -> Doc<'a> {
    let mut into = String::with_capacity(s.len());
    // Note, json escaping works unmodified for TOML too, the characters that
    // need escaping are identical and with the same escape sequences (which
    // TOML probably did on purpose). <https://toml.io/en/v1.0.0#string>
    escape_json(s, &mut into);
    concat! { "\"" into "\"" }
}

fn list<'a>(open: &'a str, close: &'a str, vs: impl Iterator<Item = &'a Value>) -> Doc<'a> {
    let mut elements = Vec::new();
    for v in vs {
        if !elements.is_empty() {
            elements.push(",".into());
            elements.push(Doc::Sep);
        }
        elements.push(value(v));
    }

    if elements.is_empty() {
        // An empty collection we always format without space in between.
        concat! { open close }
    } else {
        // Add a trailing comma in tall mode.
        elements.push(Doc::tall(","));

        group! {
            open
            Doc::SoftBreak
            indent! { Doc::Concat(elements) }
            Doc::SoftBreak
            close
        }
    }
}

pub fn dict<'a>(_vs: impl Iterator<Item = (&'a Value, &'a Value)>) -> Doc<'a> {
    unimplemented!("TODO: Toml inline tables.");
}

fn value(v: &Value) -> Doc {
    match v {
        Value::Bool(true) => Doc::from("true").with_markup(Markup::Keyword),
        Value::Bool(false) => Doc::from("false").with_markup(Markup::Keyword),
        Value::Int(i) => Doc::from(i.to_string()).with_markup(Markup::Number),
        Value::String(s) => string(s).with_markup(Markup::String),
        Value::List(vs) => list("[", "]", vs.iter()),
        Value::Set(vs) => list("[", "]", vs.iter()),
        Value::Dict(vs) => dict(vs.iter()),
        _ => unimplemented!("TODO: Toml formatter errors."),
    }
}
