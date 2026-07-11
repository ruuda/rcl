// RCL -- A reasonable configuration language.
// Copyright 2026 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Formatter that prints values as systemd units.
//!
//! This formatter is similar to the one in [`crate::fmt_toml`].

use std::collections::BTreeMap;

use crate::error::{IntoError, PathElement, Result};
use crate::markup::Markup;
use crate::pprint::{concat, group, indent, Doc};
use crate::runtime::Value;
use crate::source::Span;

/// Render a value as systemd unit.
pub fn format_systemd(caller: Span, v: &Value) -> Result<Doc> {
    let mut formatter = Formatter::new(caller);

    match v {
        Value::Dict(kv) => formatter.top_level(kv),
        _ => formatter.error("To format as systemd unit, the top-level value must be a dict."),
    }
}

/// Helper for formatting values as systemd unit.
///
/// The formatter tracks the path in the value that we are formatting from, such
/// that we can report the location of an error, in case an error occurs.
struct Formatter {
    /// The source location where formatting was triggered from.
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

    /// Format a string, quoted when needed.
    ///
    /// Similar to [`crate::string::escape_json`], but specialized for systemd.
    /// See also <https://www.freedesktop.org/software/systemd/man/latest/systemd.syntax.html#Quoting>.
    fn string<'a>(&self, str: &str) -> Doc<'a> {
        use std::fmt::Write;

        let mut into = String::with_capacity(str.len());
        let mut needs_quotes = false;

        for ch in str.chars() {
            let mut escaped = true;
            match ch {
                '\x07' => into.push_str(r#"\a"#),
                '\x08' => into.push_str(r#"\b"#),
                '\x0c' => into.push_str(r#"\f"#),
                '\n' => into.push_str(r#"\n"#),
                '\r' => into.push_str(r#"\r"#),
                '\t' => into.push_str(r#"\t"#),
                '\x0b' => into.push_str(r#"\v"#),
                '\\' => into.push_str(r#"\\"#),
                // Single quotes do trigger surrounding quotes, but because we
                // always quote in double quotes, the single quote does not need
                // to be escaped.
                '\'' => into.push('\''),
                '\"' => into.push_str(r#"\""#),
                ch if ch.is_ascii_control() => write!(into, "\\x{:02x}", ch as u32)
                    .expect("Writing into &mut String does not fail."),
                ch => {
                    escaped = false;
                    into.push(ch);
                }
            }

            // If the value contains whitespace, we quote it. Without this, it
            // would not be possible to e.g. have whitespace at the start of
            // values. It does mean we err on the side of quoting, for example
            // in Description=, the spaces don't really matter, and it would
            // read easier if the description were not quoted. But for
            // ExecStart=, being able to specify the exact argv matters: `"a b"`
            // is not the same as `a b`. We're not going to make the behavior
            // dependent on the key, so quote conservatively.
            needs_quotes |= escaped || ch.is_ascii_whitespace();
        }

        match needs_quotes {
            true => concat! { "\"" into "\"" },
            false => into.into(),
        }
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
        self.path.push(PathElement::Key(key.clone()));
        match key {
            Value::String(k_str) => Ok(self.key(k_str).with_markup(Markup::Field)),
            _ => self.error("To export as TOML, keys must be strings."),
        }
    }

    /// Format a key-value pair inside a section.
    ///
    /// If the value is a list or set, we repeat the key.
    fn key_value<'a>(&mut self, key: &'a Value, value: &'a Value) -> Result<Doc<'a>> {
        match value {
            Value::List(vs) => self.key_values(key, vs.iter()),
            Value::Set(vs) => self.key_values(key, vs.iter()),
            v => {
                let result = concat! {
                    self.push_key(key)? "=" self.value(v)? Doc::HardBreak
                };
                self.path.pop().expect("We pushed the key before.");
                Ok(result)
            }
        }
    }

    /// Format a repeated key-value pair inside a section.
    fn key_values<'a>(
        &mut self,
        key: &'a Value,
        values: impl Iterator<Item = &'a Value>,
    ) -> Result<Doc<'a>> {
        let mut parts: Vec<Doc<'a>> = Vec::new();
        for v in values {
            parts.push(self.push_key(key)?);
            parts.push("=".into());
            parts.push(self.value(v)?);
            parts.push(Doc::HardBreak);
            self.path.pop().expect("We pushed the key before.");
        }
        Ok(Doc::Concat(parts))
    }

    /// Format a space-separated list of values.
    ///
    /// In wide mode, the separator is just spaces. In tall mode, the spaces
    /// turn into line breaks preceded by a backslash, which in systemd are
    /// equivalent to a space.
    fn space_separated<'a>(&mut self, values: impl Iterator<Item = &'a Value>) -> Result<Doc<'a>> {
        let mut head = None;
        let mut tail: Vec<Doc<'a>> = Vec::new();
        for (i, v) in values.enumerate() {
            match i {
                0 => head = Some(self.value(v)?),
                _ => {
                    tail.push(Doc::tall("\\"));
                    tail.push(Doc::Sep);
                    tail.push(self.value(v)?);
                }
            }
        }
        let result = group! {
            head
            indent! { Doc::Concat(tail) }
        };
        Ok(result)
    }

    fn value<'a>(&mut self, v: &'a Value) -> Result<Doc<'a>> {
        let result = match v {
            Value::Null => Doc::Empty,
            Value::Bool(true) => Doc::from("true").with_markup(Markup::Keyword),
            Value::Bool(false) => Doc::from("false").with_markup(Markup::Keyword),
            Value::Number(d) => Doc::from(d.format()).with_markup(Markup::Number),
            Value::String(s) => self.string(s).with_markup(Markup::String),

            // For collections, the outer formatter already formats them by
            // repeating the key. If we still get here, that's a collection
            // inside a collection, and then we format them space-separated.
            Value::List(vs) => self.space_separated(vs.iter())?,
            Value::Set(vs) => self.space_separated(vs.iter())?,

            // If we get a dict at this level, we can't really decide for the
            // user how to format that. E.g. BindPaths= takes colon-separated
            // "key-values", Environment= takes fully quoted 'key=value' pairs
            // separated by spaces. Probably the latter makes sense. But for
            // now we just ban dicts and let the user make strings.
            Value::Dict(..) => self
                .error("Dicts cannot be exported in systemd units.")
                .map_err(|err| {
                    err.with_help(
                        "Format key-values as strings first, for example with a comprehension.",
                    )
                })?,
            Value::Function(..) => self.error("Functions cannot be exported in systemd units.")?,
            Value::BuiltinFunction(..) => {
                self.error("Functions cannot be exported in systemd units.")?
            }
            Value::BuiltinMethod { .. } => {
                self.error("Methods cannot be exported in systemd units.")?
            }
        };
        Ok(result)
    }

    fn section<'a>(
        &mut self,
        section_header: &'a Value,
        inner: &'a Value,
        result: &mut Vec<Doc<'a>>,
    ) -> Result<()> {
        let section_header = self.push_key(section_header)?;

        // Separate sections by a blank line.
        if !result.is_empty() {
            result.push(Doc::HardBreak);
        }

        result.push(concat! { "[" section_header "]" });
        result.push(Doc::HardBreak);

        match inner {
            // When we format a section, the value must be a dict of `Key=value` pairs.
            Value::Dict(kv) => {
                for (inner_k, inner_v) in kv.iter() {
                    result.push(self.key_value(inner_k, inner_v)?);
                }
            }

            // Anything else is invalid at this level.
            _ => self.error("Expected a dict, e.g. { WantedBy = \"multi-user.target\" }.")?,
        }

        self.path.pop().expect("We pushed the key before.");

        Ok(())
    }

    fn top_level<'a>(&mut self, kv: &'a BTreeMap<Value, Value>) -> Result<Doc<'a>> {
        let mut result: Vec<Doc> = Vec::new();

        for (k, v) in kv {
            match v {
                // For a list or set, we repeat the entire section.
                Value::List(kvs) => {
                    for (i, kv) in kvs.iter().enumerate() {
                        self.path.push(PathElement::Index(i));
                        self.section(k, kv, &mut result)?;
                        self.path.pop();
                    }
                }
                Value::Set(kvs) => {
                    for (i, kv) in kvs.iter().enumerate() {
                        self.path.push(PathElement::Index(i));
                        self.section(k, kv, &mut result)?;
                        self.path.pop();
                    }
                }

                // Anything else must be a dict, but we match that inside the
                // `section` method.
                _ => self.section(k, v, &mut result)?,
            }
        }

        Ok(Doc::Concat(result))
    }
}
