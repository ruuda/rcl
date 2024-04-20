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

macro_rules! define_ops {
    { $(#[doc = $doc:expr] $opcode:expr => $name:ident,)+ } => {
        /// An opcode for our little source builder language.
        ///
        /// Instruction opcodes are 1 byte, and every opcode is followed by a 1-byte
        /// _width_ argument, also called _n_ below. The meaning of this argument
        /// differs per instruction. For some it's an index into one of the stacks,
        /// for some it's the length of data to pull from the end of the input.
        #[derive(Debug)]
        #[repr(u8)]
        enum Op {
            $( #[doc = $doc] $name = $opcode ),+
        }

        fn parse_op(opcode: u8) -> Option<Op> {
            match opcode {
                $( $opcode => Some(Op::$name), )+
                _ => None,
            }
        }
    }
}

define_ops! {
    /// Add a fresh identifier to the identifier stack, from the fuzz input.
    0x00 => IdentPushInput,
    /// Push the name of a builtin method or value to the identifier stack.
    0x01 => IdentPushBuiltin,
    /// Remove the top of the identifier stack.
    0x02 => IdentPop,

    /// Add a fresh name to the type stack, from the fuzz input.
    0x10 => TypePushInput,
    /// Add the name of a builtin type to the identifier stack.
    0x11 => TypePushBuiltin,
    /// Treat the top as a type constructor, and apply it to _n_ other elements.
    0x12 => TypeApply,

    /// Push an identifier from the identifier stack onto the expression stack.
    0x20 => ExprPushIdent,
    /// Push an identifier from the identifier stack, wrapped in quotes.
    0x21 => ExprPushIdentString,
    /// Push an integer in decimal form onto the expression stack.
    0x22 => ExprPushDecimal,
    /// Push an integer in hexadecimal form onto the expression stack.
    0x23 => ExprPushHexadecimal,
    /// Push an integer in binary form onto the expression stack.
    0x24 => ExprPushBinary,
    /// Push one of `true`, `false`, `null`.
    0x25 => ExprPushLiteral,

    /// Wrap the top of the expression stack in `()`.
    0x30 => ExprWrapParens,
    /// Combine the top _n_ elements into a list.
    0x31 => ExprList,
    /// Combine the top _n_ elements into a set.
    0x32 => ExprSet,
    /// Combine the top _2n_ elements into a dict with `:` to separate keys from values.
    0x33 => ExprDictColon,
    /// Combine the top _2n_ elements into a dict with `=` to separate keys from values.
    0x34 => ExprDictRecord,
    /// Call the top of the stack, with _n_ elements as arguments.
    0x35 => ExprCall,
    /// Make the top of the stack a function body, with _n_ elements as arguments.
    0x36 => ExprFunction,
    /// Join the top _n_ elements with a `.` in between.
    0x37 => ExprField,
    /// Combine the top _2n + 1_ elements into an f-string.
    0x38 => ExprFormatString,
    /// Prepend an unary operator to the element a the top.
    0x39 => ExprUnop,
    /// Combine the top _n_ elements with a binary operator in between.
    0x3a => ExprBinop,

    /// Replace the top 2 elements with `let ... = {0}; {1}`.
    0x50 => ExprLet,
    /// Replace the top 2 elements with `let ...: {T} = {0}; {1}`.
    0x51 => ExprTypedLet,
    /// Replace the top 3 elements with `assert {0}, {1}; {2}`.
    0x52 => ExprAssert,
    /// Replace the top 2 elements with `trace {0}; {2}`.
    0x53 => ExprTrace,
    /// Replace the top 3 elements with `if {0}: {1} else {2}`.
    0x54 => ExprIfElse,
    /// Replace the top 2 elements with `if {0}: {1}`.
    0x55 => ExprIf,
    /// Replace the top 2 elements with `for ... in {0}: {1}`.
    0x56 => ExprFor,

    /// Render the program, run the evaluator on it.
    0xef => CheckEval,
    // TODO: Extend with all the same evaluation modes as the `main` fuzzer.
}

fn parse_program(input: &[u8]) -> Vec<(Op, u8)> {
    let mut instructions = Vec::with_capacity(input.len() / 2);

    for i in (0..instructions.len()).step_by(2) {
        match parse_op(input[i]) {
            Some(op) => instructions.push((op, input[i + 1])),
            // Skip over unknown instructions. Or would it be better to exit the
            // fuzzer early? Hmm ...
            None => continue,
        }
    }

    instructions
}

fuzz_target!(|input: &[u8]| {
    let _instructions = parse_program(input);
});
