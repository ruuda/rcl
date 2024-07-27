// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Implementation of the `rcl build` subcommand.

use std::rc::Rc;

use crate::cli::OutputFormat;
use crate::error::{Error, PathElement, Result};
use crate::fmt_rcl::format_rcl;
use crate::loader::Loader;
use crate::pprint::{concat, Config, Doc};
use crate::runtime::Value;
use crate::source::{DocId, Span};
use crate::type_source::Source;
use crate::types::{Dict, SourcedType, Type};

/// Return the type to typecheck a build file against.
fn get_build_file_type() -> SourcedType {
    // TODO: Once we have record types, we can turn this into a record.
    let build_target_dict = Dict {
        key: SourcedType {
            type_: Type::String,
            source: Source::BuildFile("Build targets have named fields."),
        },
        value: SourcedType {
            type_: Type::Any,
            source: Source::None,
        },
    };
    let build_file_dict = Dict {
        key: SourcedType {
            type_: Type::String,
            source: Source::BuildFile("The keys in the build file are output paths."),
        },
        value: SourcedType {
            type_: Type::Dict(Rc::new(build_target_dict)),
            source: Source::BuildFile("Build targets are dicts."),
        },
    };
    SourcedType {
        type_: Type::Dict(Rc::new(build_file_dict)),
        source: Source::BuildFile("A build file is a dict with output paths and targets."),
    }
}

struct Target {
    out_path: Rc<str>,
    banner: Option<Rc<str>>,
    contents: Value,
    format: OutputFormat,
    width: u32,
}

fn parse_format(format: &str) -> Option<OutputFormat> {
    // Note, this is duplicated between the CLI parser.
    let f = match format {
        "json" => OutputFormat::Json,
        "raw" => OutputFormat::Raw,
        "rcl" => OutputFormat::Rcl,
        "toml" => OutputFormat::Toml,
        "yaml-stream" => OutputFormat::YamlStream,
        _ => return None,
    };
    Some(f)
}

fn parse_targets(doc_span: Span, targets_value: Value) -> Result<Vec<Target>> {
    // Confirm the high-level shape of the value: a dict of dicts.
    // TODO: Would be better to feed in the requirement already during doc evaluation.
    targets_value.is_instance_of(doc_span, &get_build_file_type())?;

    let banner: Option<Rc<str>> = None;

    // After we did the typecheck, we can use `expect_` safely here.
    let targets = targets_value.expect_dict();
    let mut result = Vec::with_capacity(targets.len());

    for (out_path_value, target_value) in targets.iter() {
        // Note, the format does not have a default value on purpose. When you
        // generate files, that file needs to be in a particular format. Better
        // be explicit about it.
        let mut format: Option<OutputFormat> = None;
        let mut contents: Option<Value> = None;
        let mut target = Target {
            out_path: out_path_value.expect_string_clone(),
            banner: banner.clone(),
            contents: Value::Null,
            format: OutputFormat::Rcl,
            width: 80,
        };
        for (k, v) in target_value.expect_dict().iter() {
            let make_error = |message: Doc<'static>| {
                Error::new(message)
                    .with_path_element(PathElement::Key(k.clone()))
                    .with_path_element(PathElement::Key(out_path_value.clone()))
            };
            match k.expect_string() {
                "banner" => match v {
                    Value::String(banner) => target.banner = Some(banner.clone()),
                    Value::Null => target.banner = None,
                    _not_str => return make_error("Banner must be a string or null.".into()).err(),
                },
                "contents" => contents = Some(v.clone()),
                "format" => {
                    if let Value::String(format_str) = v {
                        if let Some(f) = parse_format(format_str.as_ref()) {
                            format = Some(f);
                            continue;
                        }
                    }
                    let msg = concat! { "Invalid output format: " format_rcl(v).into_owned() "." };
                    return make_error(msg)
                        .with_help("See 'rcl evaluate --help' for supported output formats.")
                        .err();
                }
                "width" => match v {
                    Value::Int(w) if *w > 0 && *w <= u32::MAX as i64 => target.width = *w as u32,
                    _not_int => {
                        return make_error("Width must be a positive integer.".into()).err()
                    }
                },
                unknown => {
                    return make_error(concat! {
                        "Unknown build target field: '" Doc::highlight(unknown).into_owned() "'."
                    })
                    .err()
                }
            }
        }

        match contents {
            Some(v) => target.contents = v,
            None => {
                let msg = concat! {
                    "Build targets must have a '" Doc::highlight("contents") "' field."
                };
                return Error::new(msg)
                    .with_path_element(PathElement::Key(out_path_value.clone()))
                    .err();
            }
        }
        match format {
            Some(f) => target.format = f,
            None => {
                let msg = concat! {
                    "Build targets must have a '" Doc::highlight("format") "' field."
                };
                return Error::new(msg)
                    .with_path_element(PathElement::Key(out_path_value.clone()))
                    .err();
            }
        }

        result.push(target);
    }

    Ok(result)
}

/// Take a build specification and write the outputs to files.
pub fn execute_build(
    loader: &Loader,
    buildfile: DocId,
    doc_span: Span,
    targets_value: Value,
) -> Result<()> {
    let targets = parse_targets(doc_span, targets_value).map_err(|mut err| {
        err.origin = Some(err.origin.unwrap_or(doc_span));
        err
    })?;

    for (i, target) in targets.iter().enumerate() {
        println!("[{}/{}] Writing {}", i + 1, targets.len(), target.out_path);

        let mut out_file = loader.open_build_output(target.out_path.as_ref(), buildfile)?;

        let mut doc = crate::cmd_eval::format_value(target.format, doc_span, &target.contents)?;

        if let Some(banner) = target.banner.as_ref() {
            doc = concat! {
                Doc::lines(banner)
                Doc::HardBreak
                doc
            };
        }

        let print_cfg = Config {
            width: target.width,
        };
        let result = doc.println(&print_cfg);

        match result.write_bytes_no_markup(&mut out_file) {
            Ok(()) => continue,
            Err(err) => panic!("TODO: Report IO error: {err:?}"),
        }
    }

    Ok(())
}
