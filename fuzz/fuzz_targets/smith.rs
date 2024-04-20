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

// TODO: Deduplicate these between various sources ...
/// Names of built-in variables and methods.
const BUILTINS: &[&str] = &[
    // Methods
    "chars",
    "contains",
    "ends_with",
    "enumerate",
    "except",
    "fold",
    "get",
    "group_by",
    "join",
    "key_by",
    "keys",
    "len",
    "parse_int",
    "remove_prefix",
    "remove_suffix",
    "replace",
    "reverse",
    "split",
    "split_lines",
    "starts_with",
    "to_lowercase",
    "to_uppercase",
    "values",
    // Stdlib and its functions
    "range",
    "read_file_utf8",
    "std",
];

const BUILTIN_TYPES: &[&str] = &[
    "Any", "Bool", "Dict", "Int", "List", "Null", "Set", "String", "Union", "Void",
];

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
    /// Remove the top of the identifier stack.
    0x00 => IdentPop,
    /// Add a fresh identifier to the identifier stack, from the fuzz input.
    0x01 => IdentPushInput,
    /// Push the name of a builtin method or value to the identifier stack.
    0x02 => IdentPushBuiltin,

    /// Remove the top of the type stack.
    0x10 => TypePop,
    /// Add a fresh name to the type stack, from the fuzz input.
    0x11 => TypePushInput,
    /// Add the name of a builtin type to the identifier stack.
    0x12 => TypePushBuiltin,
    /// Treat the top as a type constructor, and apply it to _n_ other elements.
    0x13 => TypeApply,

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

struct ProgramBuilder<'a> {
    ident_stack: Vec<String>,
    type_stack: Vec<String>,
    expr_stack: Vec<String>,

    input: &'a [u8],
    head: usize,
    tail: usize,
}

impl<'a> ProgramBuilder<'a> {
    pub fn new(input: &'a [u8]) -> ProgramBuilder<'a> {
        ProgramBuilder {
            ident_stack: Vec::new(),
            type_stack: Vec::new(),
            expr_stack: Vec::new(),
            input,
            head: 0,
            tail: input.len(),
        }
    }

    /// Consume a string from the end of the input.
    fn take_str(&mut self, len: u8) -> &str {
        let n = self.tail.min(len as usize);
        let start = self.tail - n;
        let bytes = &self.input[start..self.tail];
        self.tail = start;
        std::str::from_utf8(bytes).ok().unwrap_or("")
    }

    /// Join elements from a stack with separators and surround them with open/close tokens.
    ///
    /// This is used to build lists and dicts, function calls, etc.
    fn join(
        n: u8,
        from: &mut Vec<String>,
        mut into: String,
        open: char,
        sep_even: &'static str,
        sep_odd: &'static str,
        close: char,
    ) -> String {
        into.push(open);
        for i in 0..n {
            match from.pop() {
                None => break,
                Some(t) => {
                    let sep = if i % 2 == 0 { sep_even } else { sep_odd };
                    into.push_str(&t);
                    into.push_str(sep);
                }
            }
        }
        into.push(close);
        into
    }

    /// Execute a single instruction if possible. Returns whether we can execute more.
    pub fn execute_instruction(&mut self) -> bool {
        // We need at least one opcode and one argument.
        if self.head + 2 >= self.tail {
            return false;
        }

        let op_byte = self.input[self.head];
        let n = self.input[self.head + 1];
        self.head += 2;

        let op = match parse_op(op_byte) {
            None => return true,
            Some(op) => op,
        };

        // In reproduce mode (but not while fuzzing), print the program as we execute it.
        #[cfg(fuzzing_repro)]
        println!("{op:?}, {n}");

        match op {
            Op::IdentPop => {
                self.ident_stack.pop();
            }
            Op::IdentPushInput => {
                let arg = self.take_str(n).into();
                self.ident_stack.push(arg);
            }
            Op::IdentPushBuiltin => {
                let i = (BUILTINS.len() - 1).min(n as usize);
                self.ident_stack.push(BUILTINS[i].into());
            }

            Op::TypePop => {
                self.type_stack.pop();
            }
            Op::TypePushInput => {
                let arg = self.take_str(n).into();
                self.type_stack.push(arg);
            }
            Op::TypePushBuiltin => {
                let i = (BUILTIN_TYPES.len() - 1).min(n as usize);
                self.type_stack.push(BUILTIN_TYPES[i].into());
            }
            Op::TypeApply => {
                let constructor = self.type_stack.pop().unwrap_or("List".into());
                let applied = ProgramBuilder::join(
                    n,
                    &mut self.type_stack,
                    constructor,
                    '[',
                    ", ",
                    ", ",
                    ']',
                );
                self.type_stack.push(applied);
            }

            _ => return true,
        }

        true
    }
}

fuzz_target!(|input: &[u8]| {
    let mut builder = ProgramBuilder::new(input);
    let mut has_more = true;
    while has_more {
        has_more = builder.execute_instruction();
    }
    assert!(builder.type_stack.len() < 5);
    assert!(builder.ident_stack.len() < 5);
});
