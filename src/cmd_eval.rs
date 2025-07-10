// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Functions for implementing parts of `rcl evaluate` and `rcl query`.

use crate::cli::OutputFormat;
use crate::error::Result;
use crate::pprint::Doc;
use crate::runtime::Value;
use crate::source::Span;

pub fn format_value(format: OutputFormat, value_span: Span, value: &Value) -> Result<Doc> {
    let result = match format {
        OutputFormat::Json => crate::fmt_json::format_json(value_span, value)?,
        OutputFormat::JsonLines => crate::fmt_json_lines::format_json_lines(value_span, value)?,
        OutputFormat::Raw => crate::fmt_raw::format_raw(value_span, value)?,
        OutputFormat::Rcl => crate::fmt_rcl::format_rcl(value),
        OutputFormat::Toml => crate::fmt_toml::format_toml(value_span, value)?,
        OutputFormat::YamlStream => crate::fmt_yaml_stream::format_yaml_stream(value_span, value)?,
    };
    Ok(result)
}
