// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Smith is a fuzzer that generates likely-interesting RCL expressions.
//!
//! The key idea is that if the fuzzer is generating RCL as text bytes, although
//! this works surprisingly well, this will waste a lot of time on inputs with
//! e.g. non-matching brackets that don't pass the lexer, or inputs where all
//! indentifiers are different, so without interesting logic.
//!
//! So for this fuzzer, we treat the fuzz input as instructions for a small
//! stack-based language that builds the RCL input as we go.
//!
//! The name of this fuzzer is inspired by Csmith by John Regehr et al.
//! I don't know if the implementation resembles theirs.

#![no_main]

use libfuzzer_sys::fuzz_target;
use rcl::loader::Loader;

#[derive(Debug)]
#[repr(u8)]
enum Instr {
    /// Add a fresh identifier to the identifier stack, from the fuzz input.
    IdentPushInput = 0x00,
    /// Push the name of a builtin method or value to the identifier stack.
    IdentPushBuiltin = 0x01,
    /// Remove the top of the identifier stack.
    IdentPop = 0x02,

    /// Add a fresh name to the type stack, from the fuzz input.
    TypePushInput = 0x10,
    /// Add the name of a builtin type to the identifier stack.
    TypePushBuiltin = 0x11,
    /// Treat the top as a type constructor, and apply it to `n` other elements.
    TypeApply = 0x12,

    /// Push an identifier from the identifier stack onto the expression stack.
    ExprPushIdent = 0x20,
    /// Wrap the top of the expression stack in `()`.
    ExprWrapParens = 0x21,
    /// Wrap the top of the expression stack in `{}`.
    ExprWrapBraces = 0x22,
    /// Wrap the top of the expression stack in `[]`.
    ExprWrapBrackets = 0x23,
    /// Wrap the top of the expression stack in `"`.
    ExprWrapQuotes = 0x24,
    /// Join the top `n` elements with a `, ` in between.
    ExprJoinComma = 0x25,
    /// Join the top `n` elements with a `.` in between.
    ExprJoinDot = 0x26,
    /// Join the top `2n` elements as `:` key-value pairs.
    ExprJoinColon = 0x27,
    /// Join the top `2n` elements as `=` key-value pairs.
    ExprJoinRecord = 0x28,
}

fuzz_target!(|input: &str| {});
