// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Conversion from and to json.

use crate::error::{Result, ValueError};
use crate::runtime::Value;
use crate::source::Span;
use crate::string::escape_json;

/// Render a value as json.
pub fn format_json(caller: Span, v: &Value, into: &mut String) -> Result<()> {
    // TODO: Instead of the caller span, we should have a dedicated error type
    // for reporting runtime errors.
    match v {
        Value::Null => into.push_str("null"),
        Value::Bool(true) => into.push_str("true"),
        Value::Bool(false) => into.push_str("false"),
        Value::Int(i) => into.push_str(&i.to_string()),
        Value::String(s) => {
            into.push('"');
            escape_json(s, into);
            into.push('"');
        }
        Value::List(vs) => {
            into.push('[');
            let mut is_first = true;
            for v in vs {
                if !is_first {
                    into.push(',');
                }
                format_json(caller, v, into)?;
                is_first = false;
            }
            into.push(']');
        }
        Value::Set(vs) => {
            into.push('[');
            let mut is_first = true;
            for v in vs {
                if !is_first {
                    into.push(',');
                }
                format_json(caller, v, into)?;
                is_first = false;
            }
            into.push(']');
        }
        Value::Map(vs) => {
            into.push('{');
            let mut is_first = true;
            for (k, v) in vs {
                if !is_first {
                    into.push(',');
                }
                match k.as_ref() {
                    Value::String(..) => format_json(caller, k, into)?,
                    _ => {
                        let err = ValueError {
                            span: caller,
                            // TODO: Include information about the encountered type.
                            message: "To export as json, keys must be strings.",
                        };
                        return Err(err.into());
                    }
                };
                into.push(':');
                format_json(caller, v, into)?;
                is_first = false;
            }
            into.push('}');
        }
        Value::Builtin(..) => {
            let err = ValueError {
                span: caller,
                message: "Functions cannot be exported as json.",
            };
            return Err(err.into());
        }
    }

    Ok(())
}
