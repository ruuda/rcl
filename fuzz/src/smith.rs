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

use std::fmt::Formatter;

use arbitrary::{Arbitrary, Unstructured};

use crate::uber::Mode;

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

const UNOPS: &[&str] = &["not", "-"];

const BINOPS: &[&str] = &[
    "and", "or", "|", "+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!=",
];

/// Return a copy of the nth last element of the array, clamping to the first.
///
/// We take from the back, so that if an opcode references an identifier, it
/// takes relative to the stack, so that if a mutation inserts a push instruction,
/// it actually changes the behavior of the program.
pub fn nth<S: ToString>(xs: &[S], n: u8) -> Option<String> {
    if xs.is_empty() {
        None
    } else {
        let i = xs.len().min(n as usize + 1);
        Some(xs[xs.len() - i].to_string())
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
        pub enum Op {
            $( #[doc = $doc] $name = $opcode ),+
        }

        pub fn parse_opcode(opcode: u8) -> Option<Op> {
            match opcode {
                $( $opcode => Some(Op::$name), )+
                _ => None,
            }
        }
    }
}

define_ops! {
    // 0x00 was used to pop an identifier in the past, but it's not needed. The
    // opcode can be removed in the future, but maybe it's good to keep it
    // unused for mutations that mis-align all instructions, so a 0-arg doesn't
    // become a valid instruction. Is that helpful though?
    /// Add a fresh identifier to the identifier stack, from the fuzz input.
    0x01 => IdentPushInput,
    /// Push the name of a builtin method or value to the identifier stack.
    0x02 => IdentPushBuiltin,

    // 0x10 was used for type pop in the past, but it's not needed. The opcode
    // can be reused in the future.
    /// Add a fresh name to the type stack, from the fuzz input.
    0x11 => TypePushInput,
    /// Add the name of a builtin type to the identifier stack.
    0x12 => TypePushBuiltin,
    /// Treat the top as a type constructor, and apply it to _n_ other elements.
    0x13 => TypeApply,
    /// Make the top of the stack a function type, with _n_ elements as arguments.
    0x14 => TypeFunction,

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
    /// Combine the top two elements with a binary operator in between.
    0x3a => ExprBinop,
    /// Combine the top two elements in an indexing operation.
    0x3b => ExprIndex,

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

    // The instructions below modify the fuzz mode. The default mode is `Eval`,
    // and because it's the default, there is no instruction to set it.
    /// Set the check mode to `FormatIdempotent`.
    0xe1 => ModeFormatIdempotent,
    /// Set the check mode to `JsonCheck`.
    0xe2 => ModeJsonIdempotent,
    /// Set the check mode to `JsonCheck`.
    0xe3 => ModeJsonCheck,
    /// Set the check mode to `TomlCheck`.
    0xe4 => ModeTomlCheck,
}

/// A helper for visualizing program execution for debug purposes.
pub enum TraceEvent<'a> {
    Instruction { operation: Op, argument: u8 },
    TakeString { len: u8, result: &'a str },
    TakeU64 { len: u8, result: u64 },
}

struct ProgramBuilder<'a> {
    ident_stack: Vec<String>,
    type_stack: Vec<String>,
    expr_stack: Vec<String>,

    /// Fuzz mode to use for the final program.
    mode: Mode,

    /// A trace of the executed instructions.
    trace: Vec<TraceEvent<'a>>,

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
            mode: Mode::Eval,
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
        let result = std::str::from_utf8(bytes).ok().unwrap_or("");
        let event = TraceEvent::TakeString { len, result };
        self.trace.push(event);
        result
    }

    /// Consume `len` bytes (or fewer if we run out, and at most 8) for a `u64`.
    fn take_u64(&mut self, len: u8) -> u64 {
        let n = self.tail.min(len as usize).min(8);
        let start = self.tail - n;
        let mut out = [0u8; 8];
        out[..n].copy_from_slice(&self.input[start..self.tail]);
        self.tail = start;
        let result = u64::from_le_bytes(out);
        let event = TraceEvent::TakeU64 { len, result };
        self.trace.push(event);
        result
    }

    /// Join elements from a stack with separators and surround them with open/close tokens.
    ///
    /// This is used to build lists and dicts, function calls, etc.
    ///
    /// The 4 characters are open, sep_even, sep_odd, close.
    fn join(n: u8, from: &mut Vec<String>, mut into: String, chars: &[u8; 4]) -> Option<String> {
        into.push(chars[0] as char);
        let mut sep = None;
        for i in 0..n {
            let t = from.pop()?;
            if let Some(sep) = sep {
                into.push(sep as char);
            }
            into.push_str(&t);
            sep = Some(if i % 2 == 0 { chars[1] } else { chars[2] });
        }
        into.push(chars[3] as char);
        Some(into)
    }

    /// Return whether there is an instruction next that we can execute.
    pub fn has_next(&self) -> bool {
        // We need at least one opcode and one argument.
        self.head + 2 < self.tail
    }

    /// Execute a single instruction if possible.
    ///
    /// Returns `Some` when the instruction was successful, or `None` when
    /// something failed, e.g. a stack underflow, or an invalid opcode.
    pub fn execute_instruction(&mut self) -> Option<()> {
        let op_byte = self.input[self.head];
        let n = self.input[self.head + 1];
        self.head += 2;

        let op = match parse_opcode(op_byte) {
            None => return None,
            Some(op) => op,
        };

        let event = TraceEvent::Instruction {
            operation: op,
            argument: n,
        };
        self.trace.push(event);

        match op {
            Op::IdentPushInput => {
                match n {
                    // Have a short encoding to get a short variable name.
                    // We reserve the first 16 args for this, otherwise we take
                    // from the end of the input. Without this, the fuzzer will
                    // try to use `IdentPushBuiltin` because it's a shorter fuzz
                    // input, but it leads to a longer RCL program.
                    0..=0xf => {
                        let chars = "abcfghijpqruwxyz";
                        let i = n as usize;
                        self.ident_stack.push(chars[i..i + 1].to_string());
                    }
                    _ => {
                        let arg: String = self.take_str(n - 0x10).into();
                        // Temp hack; if we allow punctuation, the fuzzer is just going
                        // to mutate this and not the instructions. But at some point we
                        // do want a way for exotic values to get in here ...
                        if arg.as_bytes().iter().all(|b| b.is_ascii_alphanumeric()) {
                            self.ident_stack.push(arg);
                        }
                    }
                }
            }
            Op::IdentPushBuiltin => {
                self.ident_stack.push(nth(BUILTINS, n).unwrap());
            }

            Op::TypePushInput => {
                let arg: String = self.take_str(n).into();
                // See also the note in `IdentPushInput`.
                if arg.as_bytes().iter().all(|b| b.is_ascii_alphanumeric()) {
                    self.type_stack.push(arg);
                }
            }
            Op::TypePushBuiltin => {
                self.type_stack.push(nth(BUILTIN_TYPES, n).unwrap());
            }
            Op::TypeApply => {
                let constructor = self.type_stack.pop()?;
                let applied = ProgramBuilder::join(n, &mut self.type_stack, constructor, b"[,,]")?;
                self.type_stack.push(applied);
            }
            Op::TypeFunction => {
                let result_type = self.type_stack.pop()?;
                let mut args =
                    ProgramBuilder::join(n, &mut self.type_stack, String::new(), b"(,,)")?;
                args.push_str(" -> ");
                args.push_str(&result_type);
                self.type_stack.push(args);
            }

            Op::ExprPushIdent => {
                let ident = nth(&self.ident_stack[..], n)?;
                self.expr_stack.push(ident);
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
                let t = self.expr_stack.last_mut()?;
                t.insert(0, '(');
                t.push(')');
            }
            Op::ExprList => {
                let result = ProgramBuilder::join(n, &mut self.expr_stack, String::new(), b"[,,]")?;
                self.expr_stack.push(result);
            }
            Op::ExprSet => {
                let result = ProgramBuilder::join(n, &mut self.expr_stack, String::new(), b"{,,}")?;
                self.expr_stack.push(result);
            }
            Op::ExprDictColon => {
                let result = ProgramBuilder::join(n, &mut self.expr_stack, String::new(), b"{:,}")?;
                self.expr_stack.push(result);
            }
            Op::ExprDictRecord => {
                let result = ProgramBuilder::join(n, &mut self.expr_stack, String::new(), b"{=,}")?;
                self.expr_stack.push(result);
            }
            Op::ExprCall => {
                let function = self.expr_stack.pop()?;
                let applied = ProgramBuilder::join(n, &mut self.expr_stack, function, b"(,,)")?;
                self.expr_stack.push(applied);
            }
            Op::ExprFunction => {
                let body = self.expr_stack.pop()?;
                let mut res =
                    ProgramBuilder::join(n, &mut self.expr_stack, String::new(), b"(,,)")?;
                res.push_str("=>");
                res.push_str(&body);
                self.expr_stack.push(res);
            }
            Op::ExprField => {
                // Reuse the join function, which then adds surrounding space,
                // but we can cut that off afterwards. Could be more efficient,
                // but this is simpler.
                let mut res =
                    ProgramBuilder::join(n, &mut self.expr_stack, String::new(), b" .. ")?;
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
            Op::ExprUnop => {
                let s = self.expr_stack.pop()?;
                let unop = nth(UNOPS, n).unwrap();
                self.expr_stack.push(format!("{unop} {s}"));
            }
            Op::ExprBinop => {
                let lhs = self.expr_stack.pop()?;
                let rhs = self.expr_stack.pop()?;
                let binop = nth(BINOPS, n).unwrap();
                self.expr_stack.push(format!("{lhs} {binop} {rhs}"));
            }
            Op::ExprIndex => {
                let collection = self.expr_stack.pop()?;
                let index = self.expr_stack.pop()?;
                self.expr_stack.push(format!("{collection}[{index}]"));
            }

            Op::ExprLet => {
                let ident = nth(&self.ident_stack[..], n)?;
                let value = self.expr_stack.pop()?;
                let body = self.expr_stack.pop()?;
                self.expr_stack
                    .push(format!("let {ident} = {value}; {body}"));
            }
            Op::ExprTypedLet => {
                let ident = nth(&self.ident_stack[..], n)?;
                let type_ = self.type_stack.pop()?;
                let value = self.expr_stack.pop()?;
                let body = self.expr_stack.pop()?;
                self.expr_stack
                    .push(format!("let {ident}: {type_} = {value}; {body}"));
            }
            Op::ExprAssert => {
                let condition = self.expr_stack.pop()?;
                let message = self.expr_stack.pop()?;
                let body = self.expr_stack.pop()?;
                self.expr_stack
                    .push(format!("assert {condition}, {message}; {body}"));
            }
            Op::ExprTrace => {
                let message = self.expr_stack.pop()?;
                let body = self.expr_stack.pop()?;
                self.expr_stack.push(format!("trace {message}; {body}"));
            }
            Op::ExprIfElse => {
                let condition = self.expr_stack.pop()?;
                let body_then = self.expr_stack.pop()?;
                let body_else = self.expr_stack.pop()?;
                self.expr_stack
                    .push(format!("if {condition}: {body_then} else {body_else}"));
            }
            Op::ExprIf => {
                let condition = self.expr_stack.pop()?;
                let body = self.expr_stack.pop()?;
                self.expr_stack.push(format!("if {condition}: {body}"));
            }
            Op::ExprFor => {
                let collection = self.expr_stack.pop()?;
                let body = self.expr_stack.pop()?;
                let mut result = "for ".to_string();
                for i in 0..(n % 4) {
                    let m = self.take_u64(1) as u8;
                    let ident = nth(&self.ident_stack[..], m)?;
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&ident);
                }
                result.push_str(" in ");
                result.push_str(&collection);
                result.push(':');
                result.push_str(&body);
                self.expr_stack.push(result);
            }
            Op::ExprImport => {
                let mut s = self.expr_stack.pop()?;
                s.insert_str(0, "import ");
                self.expr_stack.push(s);
            }

            Op::ModeFormatIdempotent => {
                self.mode = Mode::FormatIdempotent { width: n as u32 };
            }
            Op::ModeJsonIdempotent => {
                self.mode = Mode::EvalJsonIdempotent { width: n as u32 };
            }
            Op::ModeJsonCheck => {
                self.mode = Mode::EvalJsonCheck { width: n as u32 };
            }
            Op::ModeTomlCheck => {
                self.mode = Mode::EvalTomlCheck { width: n as u32 };
            }
        }

        Some(())
    }

    /// Return the final synthesized RCL expression and a trace of what executed.
    fn into_program(mut self) -> SynthesizedProgram<'a> {
        SynthesizedProgram {
            trace: self.trace,
            program: self.expr_stack.pop().unwrap_or("".into()),
            mode: self.mode,
            is_minimal: self.expr_stack.is_empty() && self.type_stack.is_empty(),
        }
    }
}

/// The output of running the program builder.
pub struct SynthesizedProgram<'a> {
    pub trace: Vec<TraceEvent<'a>>,
    pub program: String,
    pub mode: Mode,

    /// Whether there is no redundant data left on one of the stacks.
    ///
    /// If there is, it means that a smaller smith program could evaluate to the
    /// same RCL program. We can skip the RCL part of the fuzzer for those to
    /// focus more mutations on interesting programs.
    pub is_minimal: bool,
}

impl<'a> SynthesizedProgram<'a> {
    pub fn new(bytecode: &'a [u8]) -> SynthesizedProgram<'a> {
        let mut builder = ProgramBuilder::new(bytecode);
        let mut is_ok = true;

        while builder.has_next() {
            let instr_ok = builder.execute_instruction().is_some();
            is_ok = is_ok && instr_ok;
        }

        let mut result = builder.into_program();
        result.is_minimal = result.is_minimal && is_ok;
        result
    }
}

// To control the `Debug` output in the libfuzzer_sys crate,
// it demands an `Arbitrary` instance, even though we have our own way of
// consuming the buffer. Fortunately we can get access to the underlying buffer.
impl<'a> Arbitrary<'a> for SynthesizedProgram<'a> {
    fn arbitrary(_: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        unreachable!("Only arbitrary_take_rest is used.");
    }
    fn arbitrary_take_rest(u: Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(SynthesizedProgram::new(u.take_rest()))
    }
}

impl<'a> std::fmt::Debug for SynthesizedProgram<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "╭──────╴ Opcode (hex)")?;
        writeln!(f, "│  ╭───╴ Argument (hex)")?;
        writeln!(f, "│  │  ╭╴ Operation, argument (decimal)")?;
        for event in &self.trace {
            match event {
                TraceEvent::Instruction {
                    operation: op,
                    argument: n,
                } => {
                    writeln!(f, "{:02x} {:02x} {:?}, {}", *op as u8, n, op, n)?;
                }
                TraceEvent::TakeString { len, result } => {
                    writeln!(f, "      take_str, {len:<3} → {result:?}")?;
                }
                TraceEvent::TakeU64 { len, result } => {
                    writeln!(f, "      take_u64, {len:<3} → {result} (0x{result:x})")?;
                }
            }
        }
        writeln!(f, "{:?} -->\n{}", self.mode, self.program)
    }
}
