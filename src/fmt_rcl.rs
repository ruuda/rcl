// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Formatter that prints values as RCL.
//!
//! This formatter is very similar to the one in [`fmt_json`].

use std::rc::Rc;

use crate::markup::Markup;
use crate::pprint::{concat, group, indent, Doc};
use crate::runtime::Value;
use crate::string::{escape_json, is_identifier};

/// Render a value as RCL.
pub fn format_rcl(v: &Value) -> Doc {
    value(v)
}

/// Format a string.
fn string<'a>(s: &str) -> Doc<'a> {
    // TODO: Check if the string is multiline, and possibly format using a """-string.
    let mut into = String::with_capacity(s.len());
    escape_json(s, &mut into);
    concat! { "\"" into "\"" }
}

fn list<'a>(open: &'a str, close: &'a str, vs: impl Iterator<Item = &'a Rc<Value>>) -> Doc<'a> {
    let mut elements = Vec::new();
    for v in vs {
        if !elements.is_empty() {
            elements.push(",".into());
            elements.push(Doc::Sep);
        }
        elements.push(value(v));
    }

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

fn dict<'a>(vs: impl Iterator<Item = (&'a Rc<Value>, &'a Rc<Value>)>) -> Doc<'a> {
    let mut elements = Vec::new();

    for (k, v) in vs {
        if !elements.is_empty() {
            elements.push(",".into());
        }
        elements.push(Doc::Sep);
        match k.as_ref() {
            // Format as identifier if we can, or as string if we have to.
            Value::String(k_str) if is_identifier(k_str) => {
                elements.push(Doc::from(k_str.as_ref()).with_markup(Markup::Identifier));
                elements.push(" = ".into());
            }
            Value::String(k_str) => {
                elements.push(string(k_str).with_markup(Markup::Identifier));
                elements.push(": ".into());
            }
            _not_string => {
                elements.push(value(k));
                elements.push(": ".into());
            }
        };
        elements.push(value(v));
    }

    // Add a trailing separator in tall mode.
    elements.push(Doc::tall(","));

    // With record syntax, in wide mode, we want a space before the closing }.
    elements.push(Doc::Sep);

    let result = group! {
        "{"
        indent! { Doc::Concat(elements) }
        "}"
    };
    result
}

fn value(v: &Value) -> Doc {
    match v {
        Value::Null => Doc::from("null").with_markup(Markup::Keyword),
        Value::Bool(true) => Doc::from("true").with_markup(Markup::Keyword),
        Value::Bool(false) => Doc::from("false").with_markup(Markup::Keyword),
        Value::Int(i) => Doc::from(i.to_string()).with_markup(Markup::Number),
        Value::String(s) => string(s).with_markup(Markup::String),
        Value::List(vs) => list("[", "]", vs.iter()),
        // TODO: An empty set should print as {}, that would be a non-idempotency,
        // because {} is the empty dict.
        Value::Set(vs) => list("{", "}", vs.iter()),
        Value::Dict(vs) => dict(vs.iter()),
        // TODO: Add a more proper printer for functions/builtins. For now this will do.
        Value::Function(..) => Doc::from("«function»").with_markup(Markup::Keyword),
        Value::BuiltinFunction(b) => Doc::from(b.name).with_markup(Markup::Builtin),
        Value::BuiltinMethod(b, _sr, _sm, _receiver) => {
            Doc::from(b.name).with_markup(Markup::Builtin)
        }
    }
}
