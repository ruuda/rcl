// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Formatter that prints values as TOML.
//!
//! This formatter is similar to the one in [`fmt_json`].

use std::collections::BTreeMap;

use crate::error::{IntoError, PathElement, Result};
use crate::markup::Markup;
use crate::pprint::{concat, group, indent, Doc};
use crate::runtime::Value;
use crate::source::Span;
use crate::string::escape_json;

/// Render a value as TOML.
pub fn format_toml(caller: Span, v: &Value) -> Result<Doc> {
    let mut formatter = Formatter::new(caller);

    match v {
        Value::Dict(kv) => formatter.top_level(kv),
        _ => formatter.error("Expected a dict for TOML format."),
    }
}

/// Helper for formatting values as TOML.
///
/// The formatter tracks the path in the value that we are formatting from, such
/// that we can report the location of an error, in case an error occurs.
struct Formatter {
    /// The source location where TOML formatting was triggered from.
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
        self.caller.error(message).with_path(path).err()
    }

    /// Format a string.
    fn string<'a>(&self, s: &str) -> Doc<'a> {
        let mut into = String::with_capacity(s.len());
        // Note, json escaping works unmodified for TOML too, the characters that
        // need escaping are identical and with the same escape sequences (which
        // TOML probably did on purpose). <https://toml.io/en/v1.0.0#string>
        escape_json(s, &mut into);
        concat! { "\"" into "\"" }
    }

    /// Format as a key in a key-value pair, or table header (without brackets).
    fn key<'a>(&self, ident: &'a str) -> Doc<'a> {
        let bytes = ident.as_bytes();

        if bytes.is_empty() {
            return "\"\"".into();
        }

        // If the key is ASCII alphanumeric, underscores, or dashes, then we can
        // use it unquoted. <https://toml.io/en/v1.0.0#keys> Even if it starts
        // with a digit.
        if bytes
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'_' || *b == b'-')
        {
            ident.into()
        } else {
            self.string(ident)
        }
    }

    /// Format a key, and push it to as path, or return an error on non-strings.
    fn push_key<'a>(&mut self, key: &'a Value) -> Result<Doc<'a>> {
        match key {
            Value::String(k_str) => {
                self.path.push(PathElement::Key(k_str.clone()));
                Ok(self.key(k_str).with_markup(Markup::Identifier))
            }
            _ => self.error("To export as TOML, keys must be strings."),
        }
    }

    /// Format a list as a TOML array.
    fn array<'a>(&mut self, vs: impl Iterator<Item = &'a Value>) -> Result<Doc<'a>> {
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

        let result = if elements.is_empty() {
            // An empty collection we always format without space in between.
            "[]".into()
        } else {
            // Add a trailing comma in tall mode.
            elements.push(Doc::tall(","));

            group! {
                "["
                Doc::SoftBreak
                indent! { Doc::Concat(elements) }
                Doc::SoftBreak
                "]"
            }
        };

        Ok(result)
    }

    /// Format a dict as a TOML "inline table".
    pub fn inline_table<'a>(
        &mut self,
        vs: impl Iterator<Item = (&'a Value, &'a Value)>,
    ) -> Result<Doc<'a>> {
        let mut elements = Vec::new();
        for (k, v) in vs {
            if !elements.is_empty() {
                elements.push(",".into());
                elements.push(Doc::Sep);
            }
            elements.push(self.push_key(k)?);
            elements.push(" = ".into());
            elements.push(self.value(v)?);
            self.path.pop().expect("Push and pop are balanced.");
        }

        let result = if elements.is_empty() {
            // An empty collection we always format without space in between.
            "{}".into()
        } else {
            // Add a trailing comma in tall mode.
            elements.push(Doc::tall(","));

            group! {
                "{"
                Doc::Sep
                indent! { Doc::Concat(elements) }
                Doc::Sep
                "}"
            }
        };
        Ok(result)
    }

    /// Format a key-value pair. <https://toml.io/en/v1.0.0#keyvalue-pair>
    fn key_value<'a>(&mut self, key: &'a Value, value: &'a Value) -> Result<Doc<'a>> {
        let result = concat! {
            self.push_key(key)? " = " self.value(value)? Doc::HardBreak
        };
        self.path.pop().expect("We pushed the key before.");
        Ok(result)
    }

    /// Format a dict as (top-level) table body.
    fn table<'a>(&mut self, vs: impl Iterator<Item = (&'a Value, &'a Value)>) -> Result<Doc<'a>> {
        let mut doc = Doc::empty();
        for (k, v) in vs {
            doc = doc + self.key_value(k, v)?;
        }
        Ok(doc)
    }

    fn value<'a>(&mut self, v: &'a Value) -> Result<Doc<'a>> {
        let result = match v {
            Value::Null => self.error("Null cannot be exported as TOML.")?,
            Value::Bool(true) => Doc::from("true").with_markup(Markup::Keyword),
            Value::Bool(false) => Doc::from("false").with_markup(Markup::Keyword),
            Value::Int(i) => Doc::from(i.to_string()).with_markup(Markup::Number),
            Value::String(s) => self.string(s).with_markup(Markup::String),
            Value::List(vs) => self.array(vs.iter())?,
            // TOML has no set type, we format sets as arrays (lists).
            Value::Set(vs) => self.array(vs.iter())?,
            Value::Dict(vs) => self.inline_table(vs.iter())?,
            Value::Function(..) => self.error("Functions cannot be exported as TOML.")?,
            Value::BuiltinFunction(..) => self.error("Functions cannot be exported as TOML.")?,
            Value::BuiltinMethod { .. } => self.error("Methods cannot be exported as TOML.")?,
        };
        Ok(result)
    }

    fn top_level<'a>(&mut self, kv: &'a BTreeMap<Value, Value>) -> Result<Doc<'a>> {
        let mut values: Vec<Doc> = Vec::new();
        let mut tables: Vec<Doc> = Vec::new();
        let mut arrays: Vec<Doc> = Vec::new();

        for (k, v) in kv {
            match v {
                // List of dicts has a special "Array of Tables" syntax in TOML.
                // <https://toml.io/en/v1.0.0#array-of-tables>
                Value::List(xs) if xs.iter().all(|x| matches!(x, Value::Dict(..))) => {
                    let table_header = self.push_key(k)?;
                    for (i, x) in xs.iter().enumerate() {
                        self.path.push(PathElement::Index(i));
                        let table_body = match x {
                            Value::Dict(table_inner) => self.table(table_inner.iter())?,
                            _ => unreachable!("We checked before that all elements are dicts."),
                        };
                        self.path.pop().expect("We pushed the index before.");
                        arrays.push(concat! {
                            "[[" table_header.clone() "]]"
                            Doc::HardBreak
                            table_body
                        });
                    }
                }
                // Top-level dicts get formatted as tables.
                Value::Dict(table_inner) => {
                    let table_header = self.push_key(k)?;
                    let table_body = self.table(table_inner.iter())?;
                    self.path.pop().expect("We pushed the key before.");
                    tables.push(concat! {
                        "[" table_header "]"
                        Doc::HardBreak
                        table_body
                    });
                }
                // Anything else becomes a top-level key-value.
                top_level_value => values.push(self.key_value(k, top_level_value)?),
            }
        }

        // We put the top-level values first, then tables, then arrays.
        for table in tables.into_iter().chain(arrays.into_iter()) {
            // Separate tables by a blank line.
            if !values.is_empty() {
                values.push(Doc::HardBreak);
            }
            values.push(table);
        }

        Ok(Doc::Concat(values))
    }
}
