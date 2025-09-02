// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! # About
//!
//! RCL — A reasonable configuration language
//!
//! * [Homepage](https://rcl-lang.org)
//! * [Documentation](https://docs.ruuda.nl/rcl/)
//! * [Codeberg](https://codeberg.org/ruuda/rcl)
//! * [GitHub](https://github.com/ruuda/rcl)
//!
//! This crate contains the core implementation of the RCL configuration
//! language, as well as the `rcl` command-line program. RCL is primarily
//! intended to be used through the command-line interface, and alternatively
//! through its Python bindings (not part of this crate). Calling the RCL
//! interpreter from a Rust project is possible with this crate, though the
//! interface is not optimized for that use case at this time, and fairly
//! low-level.
//!
//! # Using RCL from Rust
//!
//! A small example to get started:
//! ```
//! use rcl::env::Env;
//! use rcl::loader::Loader;
//! use rcl::runtime::Value;
//! use rcl::tracer::VoidTracer;
//!
//! // The RCL document that we want to evaluate.
//! let input = "std.range(3, 10).sum()";
//!
//! // The loader is the main way of interacting with RCL. It stores the input
//! // for all documents that we evaluate, so that in case of errors, errors can
//! // highlight the source span where the error happened.
//! let mut loader = Loader::new();
//! let doc_id = loader.load_string("example_input", input.to_string());
//!
//! // Define the environment in which we evaluate the expression. If we want to
//! // provide additional inputs, we can add them to the environment here.
//! let mut type_env = rcl::typecheck::prelude();
//! let mut value_env = rcl::runtime::prelude();
//!
//! // If the RCL program traces anything, we ignore that. If we wanted to print
//! // trace messages to stderr, we could use `StderrTracer` instead. If we
//! // wanted to capture them, we could provide a custom tracer implementation.
//! let mut tracer = VoidTracer;
//!
//! // Parse, typecheck, and then evaluate the document into a `Value`.
//! let value = loader.evaluate(
//!     &mut type_env,
//!     &mut value_env,
//!     doc_id,
//!     &mut tracer,
//! ).unwrap();
//! assert_eq!(value, Value::Number(42.into()));
//!
//! // We can also pretty-print this value in various formats, like RCL, TOML,
//! // or JSON. This produces a `Doc`: a tree of text fragments.
//! let span = loader.get_span(doc_id);
//! let pretty_doc = rcl::fmt_json::format_json(span, &value).unwrap();
//!
//! // We need to pretty-print this doc at a given width to get a `MarkupString`.
//! // Besides the output text, it contains markup information, so we can print
//! // the result in color. Below we discard that to get a regular `String`.
//! let config = rcl::pprint::Config { width: Some(80) };
//! let markup_string = pretty_doc.println(&config);
//! assert_eq!(&markup_string.to_string_no_markup(), "42\n");
//! ```
//! To learn more about how to drive RCL, the best way is to study, `main.rs`,
//! which implements the CLI.

#![allow(clippy::comparison_to_empty)]
#![allow(clippy::from_str_radix_10)]
#![allow(clippy::len_without_is_empty)]
#![allow(clippy::manual_range_contains)]
#![allow(clippy::new_without_default)]

mod cli_utils;

pub mod abstraction;
pub mod ast;
pub mod cli;
pub mod cmd_build;
pub mod cmd_eval;
pub mod cst;
pub mod decimal;
pub mod env;
pub mod error;
pub mod eval;
pub mod fmt_cst;
pub mod fmt_json;
pub mod fmt_json_lines;
pub mod fmt_raw;
pub mod fmt_rcl;
pub mod fmt_toml;
pub mod fmt_type;
pub mod fmt_yaml_stream;
pub mod highlight;
pub mod lexer;
pub mod loader;
pub mod markup;
pub mod parser;
pub mod patch;
pub mod pprint;
pub mod runtime;
pub mod source;
pub mod stdlib;
pub mod string;
pub mod tracer;
pub mod type_diff;
pub mod type_source;
pub mod typecheck;
pub mod types;
