// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Conversion from and to json.

use crate::error::{Result, ValueError};
use crate::pprint::{concat, group, indent, Doc};
use crate::runtime::Value;
use crate::source::Span;
use crate::string::escape_json;

/// Render a value as json.
pub fn format_json(caller: Span, v: &Value) -> Result<Doc> {
    // TODO: Instead of the caller span, we should have a dedicated error type
    // for reporting runtime errors.
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
        Value::List(vs) => {
            let mut elements = Vec::new();
            let mut is_first = true;
            for v in vs {
                if !is_first {
                    elements.push(",".into());
                    elements.push(Doc::Sep);
                }
                elements.push(format_json(caller, v)?);
                is_first = false;
            }
            group! {
                "["
                Doc::SoftBreak
                indent! { Doc::Concat(elements) }
                Doc::SoftBreak
                "]"
            }
        }
        Value::Set(vs) => {
            // TODO: Deduplicate between list/set.
            let mut elements = Vec::new();
            let mut is_first = true;
            for v in vs {
                if !is_first {
                    elements.push(",".into());
                    elements.push(Doc::Sep);
                }
                elements.push(format_json(caller, v)?);
                is_first = false;
            }
            group! {
                "["
                Doc::SoftBreak
                indent! { Doc::Concat(elements) }
                Doc::SoftBreak
                "]"
            }
        }
        Value::Map(vs) => {
            let mut elements = Vec::new();
            let mut is_first = true;
            for (k, v) in vs {
                if !is_first {
                    elements.push(",".into());
                    elements.push(Doc::Sep);
                }
                match k.as_ref() {
                    Value::String(..) => elements.push(format_json(caller, k)?),
                    _ => {
                        let err = ValueError {
                            span: caller,
                            // TODO: Include information about the encountered type.
                            message: "To export as json, keys must be strings.",
                        };
                        return Err(err.into());
                    }
                };
                elements.push(": ".into());
                elements.push(format_json(caller, v)?);
                is_first = false;
            }
            group! {
                "{"
                Doc::SoftBreak
                indent! { Doc::Concat(elements) }
                Doc::SoftBreak
                "}"
            }
        }
        Value::Builtin(..) => {
            let err = ValueError {
                span: caller,
                message: "Functions cannot be exported as json.",
            };
            return Err(err.into());
        }
    };

    Ok(result)
}
