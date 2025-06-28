// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Implementation of the `rcl build` subcommand.

use std::io::Read;
use std::rc::Rc;

use crate::cli::OutputFormat;
use crate::error::{Error, PathElement, Result};
use crate::fmt_rcl::format_rcl;
use crate::loader::{Loader, OpenMode};
use crate::pprint::{concat, Config, Doc};
use crate::runtime::Value;
use crate::source::{DocId, Span};
use crate::type_source::Source;
use crate::types::{Dict, SourcedType, Type};

/// What to do with an evaluated build target.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BuildMode {
    /// Print to stdout, do not write to the file system.
    DryRun,
    /// Confirm that the target file matches the generated contents, do not write.
    Check,
    /// Write to the target file, overwriting any existing file there.
    WriteFilesystem,
}

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
        "json-lines" => OutputFormat::JsonLines,
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
                "width" => match v.to_i64() {
                    // Technically the upper limit is u32::MAX, but if we have
                    // to be technically correct in the error message, then let's
                    // put a prettier number there.
                    Some(w) if w > 0 && w < 16_000 => target.width = w as u32,
                    _ => {
                        return make_error("Width must be a positive integer below 16,000.".into())
                            .err()
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
    mode: BuildMode,
    buildfile: DocId,
    doc_span: Span,
    targets_value: Value,
) -> Result<()> {
    let targets = parse_targets(doc_span, targets_value).map_err(|mut err| {
        err.origin = Some(err.origin.unwrap_or(doc_span));
        err
    })?;

    let mut n_changed = 0;

    for (i, target) in targets.iter().enumerate() {
        println!("[{}/{}] {}", i + 1, targets.len(), target.out_path);

        let mut doc = crate::cmd_eval::format_value(target.format, doc_span, &target.contents)?;

        if let Some(banner) = target.banner.as_ref() {
            doc = concat! {
                Doc::lines(banner)
                Doc::HardBreak
                doc
            };
        }

        let print_cfg = Config {
            width: Some(target.width),
        };
        let result = doc.println(&print_cfg);

        match mode {
            BuildMode::WriteFilesystem => {
                // coverage:off -- We don't test writing to the file system in tests.
                let mut out_file = loader.open_build_output(
                    target.out_path.as_ref(),
                    buildfile,
                    OpenMode::Write,
                )?;
                match result.write_bytes_no_markup(&mut out_file) {
                    Ok(()) => continue,
                    Err(err) => {
                        return Error::new(concat! {
                            "Failed to write to '" Doc::path(target.out_path.as_ref()) "': "
                            err.to_string()
                        })
                        .err()
                    }
                }
                // coverage:on
            }
            BuildMode::Check => {
                let mut out_file = loader.open_build_output(
                    target.out_path.as_ref(),
                    buildfile,
                    OpenMode::Read,
                )?;
                let mut expected = Vec::new();
                result
                    .write_bytes_no_markup(&mut expected)
                    .expect("Writing in memory does not fail.");
                let mut actual = Vec::with_capacity(expected.len());
                match out_file.read_to_end(&mut actual) {
                    Ok(_) => {
                        if actual != expected {
                            // coverage:off -- All files in the repo should exist and be compliant.
                            println!("Would rewrite {}", target.out_path.as_ref());
                            n_changed += 1;
                        }
                    }
                    Err(err) => {
                        return Error::new(concat! {
                            "Failed to read '" Doc::path(target.out_path.as_ref()) "': "
                            err.to_string()
                        })
                        .err()
                    }
                }
                // coverage:on
            }
            BuildMode::DryRun => {
                let mut stdout = std::io::stdout().lock();
                // Ignore the result here, if we fail to write to stdout,
                // then we have no good way of reporting the error anyway.
                let _ = result.write_bytes_no_markup(&mut stdout);
            }
        }
    }

    // In check mode, fail if anything would be rewritten.
    match mode {
        BuildMode::Check if n_changed > 0 => {
            // coverage:off -- All generated files in the repo should be up to date.
            let parts = vec![
                n_changed.to_string().into(),
                Doc::str(" of "),
                targets.len().to_string().into(),
                Doc::str(" files would be rewritten."),
            ];
            Error::new(Doc::Concat(parts)).err()
            // coverage:on
        }
        BuildMode::Check => {
            println!("All {} files are up to date.", targets.len());
            Ok(())
        }
        _ => Ok(()),
    }
}
