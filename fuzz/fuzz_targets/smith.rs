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

use std::fmt::Formatter;

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;
use rcl::eval::Evaluator;
use rcl::loader::{Loader, VoidFilesystem};
use rcl::tracer::VoidTracer;

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

const LITERALS: &[&str] = &["true", "false", "null"];

/// Return a copy of the nth element of the array, clamping to the last.
pub fn nth<S: ToString>(xs: &[S], n: u8) -> Option<String> {
    if xs.is_empty() {
        None
    } else {
        let i = (xs.len() - 1).min(n as usize);
        Some(xs[i].to_string())
    }
}

macro_rules! define_ops {
    { $(#[doc = $doc:expr] $opcode:expr => $name:ident,)+ } => {
        /// An opcode for our little source builder language.
        ///
        /// Instruction opcodes are 1 byte, and every opcode is followed by a 1-byte
        /// _width_ argument, also called _n_ below. The meaning of this argument
        /// differs per instruction. For some it's an index into one of the stacks,
        /// for some it's the length of data to pull from the end of the input.
        #[derive(Copy, Clone, Debug)]
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
    /// Push an integer in decimal form onto the expression stack.
    0x21 => ExprPushDecimal,
    /// Push an integer in hexadecimal form onto the expression stack.
    0x22 => ExprPushHexadecimal,
    /// Push an integer in binary form onto the expression stack.
    0x23 => ExprPushBinary,
    /// Push one of `true`, `false`, `null`.
    0x24 => ExprPushLiteral,

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
    /// Combine the top _2n + 1_ elements into a string or f-string.
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
    /// Replace the top element with an import expression.
    0x57 => ExprImport,

    /// Render the program, run the evaluator on it.
    0xef => CheckEval,
    // TODO: Extend with all the same evaluation modes as the `main` fuzzer.
}

struct ProgramBuilder<'a> {
    ident_stack: Vec<String>,
    type_stack: Vec<String>,
    expr_stack: Vec<String>,

    /// A trace of the executed instructions.
    trace: Vec<(Op, u8)>,

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
            trace: Vec::with_capacity(input.len() / 2),
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

    /// Consume `len` bytes (or fewer if we run out, and at most 8) for a `u64`.
    fn take_u64(&mut self, len: u8) -> u64 {
        let n = self.tail.min(len as usize).min(8);
        let start = self.tail - n;
        let mut out = [0u8; 8];
        out[..n].copy_from_slice(&self.input[start..self.tail]);
        self.tail = start;
        u64::from_le_bytes(out)
    }

    /// Join elements from a stack with separators and surround them with open/close tokens.
    ///
    /// This is used to build lists and dicts, function calls, etc.
    ///
    /// The 4 characters are open, sep_even, sep_odd, close.
    fn join(n: u8, from: &mut Vec<String>, mut into: String, chars: &[u8; 4]) -> String {
        into.push(chars[0] as char);
        for i in 0..n {
            match from.pop() {
                None => break,
                Some(t) => {
                    let sep = if i % 2 == 0 { chars[1] } else { chars[2] };
                    into.push_str(&t);
                    into.push(sep as char);
                }
            }
        }
        into.push(chars[3] as char);
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

        self.trace.push((op, n));

        match op {
            Op::IdentPop => {
                self.ident_stack.pop();
            }
            Op::IdentPushInput => {
                let arg: String = self.take_str(n).into();
                // Temp hack; if we allow punctuation, the fuzzer is just going
                // to mutate this and not the instructions. But at some point we
                // do want a way for exotic values to get in here ...
                if arg.as_bytes().iter().all(|b| b.is_ascii_alphanumeric()) {
                    self.ident_stack.push(arg);
                }
            }
            Op::IdentPushBuiltin => {
                self.ident_stack.push(nth(BUILTINS, n).unwrap());
            }

            Op::TypePop => {
                self.type_stack.pop();
            }
            Op::TypePushInput => {
                let arg = self.take_str(n).into();
                self.type_stack.push(arg);
            }
            Op::TypePushBuiltin => {
                self.type_stack.push(nth(BUILTIN_TYPES, n).unwrap());
            }
            Op::TypeApply => {
                let constructor = self.type_stack.pop().unwrap_or("List".into());
                let applied = ProgramBuilder::join(n, &mut self.type_stack, constructor, b"[,,]");
                self.type_stack.push(applied);
            }

            Op::ExprPushIdent => {
                if let Some(ident) = nth(&self.ident_stack[..], n) {
                    self.expr_stack.push(ident);
                }
            }
            Op::ExprPushDecimal => {
                let k = self.take_u64(n);
                self.expr_stack.push(k.to_string());
            }
            Op::ExprPushHexadecimal => {
                let k = self.take_u64(n);
                self.expr_stack.push(format!("0x{k:x}"));
            }
            Op::ExprPushBinary => {
                let k = self.take_u64(n);
                self.expr_stack.push(format!("0b{k:b}"));
            }
            Op::ExprPushLiteral => {
                self.expr_stack.push(nth(LITERALS, n).unwrap());
            }

            Op::ExprWrapParens => {
                if let Some(t) = self.expr_stack.last_mut() {
                    t.insert(0, '(');
                    t.push(')');
                }
            }
            Op::ExprList => {
                let result = ProgramBuilder::join(n, &mut self.expr_stack, String::new(), b"[,,]");
                self.expr_stack.push(result);
            }
            Op::ExprSet => {
                let result = ProgramBuilder::join(n, &mut self.expr_stack, String::new(), b"{,,}");
                self.expr_stack.push(result);
            }
            Op::ExprDictColon => {
                let result = ProgramBuilder::join(n, &mut self.expr_stack, String::new(), b"{:,}");
                self.expr_stack.push(result);
            }
            Op::ExprDictRecord => {
                let result = ProgramBuilder::join(n, &mut self.expr_stack, String::new(), b"{=,}");
                self.expr_stack.push(result);
            }
            Op::ExprCall => {
                let function = match self.expr_stack.pop() {
                    Some(f) => f,
                    None => return true,
                };
                let applied = ProgramBuilder::join(n, &mut self.type_stack, function, b"(,,)");
                self.expr_stack.push(applied);
            }
            Op::ExprFunction => {
                let body = match self.expr_stack.pop() {
                    Some(b) => b,
                    None => return true,
                };
                let mut res = ProgramBuilder::join(n, &mut self.type_stack, String::new(), b"(,,)");
                res.push_str("=>");
                res.push_str(&body);
                self.expr_stack.push(res);
            }
            Op::ExprField => {
                // Reuse the join function, which then adds surrounding space,
                // but we can cut that off afterwards. Could be more efficient,
                // but this is simpler.
                let mut res = ProgramBuilder::join(n, &mut self.expr_stack, String::new(), b" .. ");
                res.pop();
                res.remove(0);
                self.expr_stack.push(res);
            }
            Op::ExprFormatString => {
                // TODO: Respect `n`, *actually* make a format string.
                let mut s = self.expr_stack.pop().unwrap_or("".into());
                s.insert(0, '"');
                s.push('"');
                self.expr_stack.push(s);
            }

            Op::ExprLet => {
                let ident = nth(&self.ident_stack[..], n);
                let value = self.expr_stack.pop();
                let body = self.expr_stack.pop();
                match (ident, value, body) {
                    (Some(ident), Some(value), Some(body)) => {
                        self.expr_stack
                            .push(format!("let {ident} = {value}; {body}"));
                    }
                    _ => return true,
                }
            }
            Op::ExprTypedLet => {
                let ident = nth(&self.ident_stack[..], n);
                let type_ = self.type_stack.pop();
                let value = self.expr_stack.pop();
                let body = self.expr_stack.pop();
                match (ident, type_, value, body) {
                    (Some(ident), Some(type_), Some(value), Some(body)) => {
                        self.expr_stack
                            .push(format!("let {ident}: {type_} = {value}; {body}"));
                    }
                    _ => return true,
                }
            }
            Op::ExprFor => {
                let collection = self.expr_stack.pop();
                let body = self.expr_stack.pop();
                match (collection, body) {
                    (Some(collection), Some(body)) => {
                        let mut result = "for ".to_string();
                        for i in 0..n {
                            let m = self.take_u64(1) as u8;
                            if let Some(ident) = nth(&self.ident_stack[..], m) {
                                if i > 0 {
                                    result.push_str(", ");
                                }
                                result.push_str(&ident);
                            }
                        }
                        result.push_str(" in ");
                        result.push_str(&collection);
                        result.push(':');
                        result.push_str(&body);
                    }
                    _ => return true,
                }
            }
            Op::ExprImport => {
                let mut s = self.expr_stack.pop().unwrap_or("\"\"".into());
                s.insert_str(0, "import ");
                self.expr_stack.push(s);
            }

            _ => return true,
        }

        true
    }

    /// Return the final synthesized RCL expression and a trace of what executed.
    fn into_program(mut self) -> SynthesizedProgram {
        SynthesizedProgram {
            trace: self.trace,
            program: self.expr_stack.pop().unwrap_or("".into()),
        }
    }
}

/// The output of running the program builder.
struct SynthesizedProgram {
    trace: Vec<(Op, u8)>,
    program: String,
}

// To control the `Debug` output in the libfuzzer_sys crate,
// it demands an `Arbitrary` instance, even though we have our own way of
// consuming the buffer. Fortunately we can get access to the underlying buffer.
impl<'a> Arbitrary<'a> for SynthesizedProgram {
    fn arbitrary(_: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        unreachable!("Only arbitrary_take_rest is used.");
    }
    fn arbitrary_take_rest(u: Unstructured<'a>) -> arbitrary::Result<Self> {
        let mut builder = ProgramBuilder::new(u.take_rest());

        let mut has_more = true;
        while has_more {
            has_more = builder.execute_instruction();
        }

        Ok(builder.into_program())
    }
}

impl std::fmt::Debug for SynthesizedProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "╭──────╴ Opcode (hex)")?;
        writeln!(f, "│  ╭───╴ Argument (hex)")?;
        writeln!(f, "│  │  ╭╴ Operation, argument (decimal)")?;
        for (op, n) in &self.trace {
            writeln!(f, "{:02x} {:02x} {:?}, {}", *op as u8, n, op, n)?;
        }
        writeln!(f, "-->\n{}", self.program)
    }
}

fuzz_target!(|input: SynthesizedProgram| {
    // In repro mode, also print the input when it doesn't fail, so we have a
    // way to spy at what programs the fuzzer is discovering.
    #[cfg(fuzzing_repro)]
    println!("{input:?}");

    let mut loader = Loader::new();
    loader.set_filesystem(Box::new(VoidFilesystem));
    let doc = loader.load_string(input.program);
    let mut tracer = VoidTracer;
    let mut evaluator = Evaluator::new(&mut loader, &mut tracer);
    let mut type_env = rcl::typecheck::prelude();
    let mut value_env = rcl::runtime::prelude();
    let _ = evaluator.eval_doc(&mut type_env, &mut value_env, doc);
});
