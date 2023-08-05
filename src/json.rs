//! Conversion from and to json.

use crate::error::Result;
use crate::runtime::Value;

/// Render a value as json.
pub fn format_json(v: &Value, into: &mut String) -> Result<()> {
    match v {
        Value::Bool(true) => into.push_str("true"),
        Value::Bool(false) => into.push_str("false"),
        Value::Int(i) => into.extend(i.to_string().chars()),
        Value::String(s) => {
            into.push('"');
            // TODO: Implement proper json escaping.
            into.extend(s.escape_default());
            into.push('"');
        }
        Value::List(vs) => {
            into.push('[');
            let mut is_first = true;
            for v in vs {
                if !is_first {
                    into.push(',');
                    format_json(v, into)?;
                }
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
                format_json(v, into)?;
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
                    Value::String(..) => format_json(k, into)?,
                    _ => return Err("To export as json, keys must be strings.".into()),
                };
                into.push(':');
                format_json(v, into)?;
                is_first = false;
            }
            into.push('}');
        }
        Value::Builtin(..) => {
            return Err("Functions cannot be exported as json.".into());
        }
    }

    Ok(())
}
