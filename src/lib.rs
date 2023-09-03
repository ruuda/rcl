// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

#![allow(clippy::from_str_radix_10)]
#![allow(clippy::len_without_is_empty)]
#![allow(clippy::manual_range_contains)]

pub mod abstraction;
pub mod ast;
pub mod cst;
pub mod error;
pub mod eval;
pub mod fmt;
pub mod fmt_utils;
pub mod highlight;
pub mod json;
pub mod lexer;
pub mod parser;
pub mod pprint;
pub mod runtime;
pub mod source;
pub mod string;

/// A placeholder just like regular `todo!`, except it doesn't prevent fuzzing.
#[macro_export]
macro_rules! todo_placeholder {
    ($message:expr, $placeholder:expr,) => {
        if cfg!(fuzzing) {
            $placeholder
        } else {
            todo!($message);
        }
    };
}
