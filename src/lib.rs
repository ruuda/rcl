// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

#![allow(clippy::comparison_to_empty)]
#![allow(clippy::from_str_radix_10)]
#![allow(clippy::len_without_is_empty)]
#![allow(clippy::manual_range_contains)]

pub mod abstraction;
pub mod ast;
pub mod cli_command;
pub mod cli_parser;
pub mod cst;
pub mod error;
pub mod eval;
pub mod fmt_cst;
pub mod fmt_json;
pub mod fmt_rcl;
pub mod highlight;
pub mod lexer;
pub mod loader;
pub mod markup;
pub mod parser;
pub mod platform_utils;
pub mod pprint;
pub mod runtime;
pub mod source;
pub mod string;
