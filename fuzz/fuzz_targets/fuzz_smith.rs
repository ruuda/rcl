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

use libfuzzer_sys::{fuzz_mutator, fuzz_target};
use nanorand::{Rng, WyRand};
use rcl::eval::Evaluator;
use rcl::loader::{Loader, VoidFilesystem};
use rcl::tracer::VoidTracer;
use rcl_fuzz::smith::SynthesizedProgram;

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

struct Mutator<'a> {
    data: &'a mut [u8],
    size: usize,
    max_size: usize,
    rng: WyRand,
}

impl<'a> Mutator<'a> {
    /// Return the byte offset of an arbitrary instruction in the buffer.
    fn gen_instruction_index(&mut self) -> usize {
        // Subtract 1 so we are sure to have an index of a full 2-byte instruction,
        // not a trailing 1-byte leftover.
        let i = std::cmp::min(self.size - 1, self.max_size - 1) / 2;
        self.rng.generate_range(0..i) * 2
    }

    /// Return an arbitrary index into the data buffer.
    fn gen_data_index(&mut self) -> usize {
        // Bias indices towards the end of the data; the instructions are at the
        // start and auxiliary data is at the end. Instructions are 2 bytes, so
        // if we delete one byte in the middle then the part after it becomes
        // meaningless (they might still be valid instructions, but it's not a
        // small mutation). We should have more luck deleting in e.g. a string
        // at the end.
        let n = std::cmp::min(self.size, self.max_size);
        match self.rng.generate_range(0..3) {
            0 => n - 1,
            1 => self.rng.generate_range((n / 2)..n),
            2 => self.rng.generate_range(0..n),
            _ => unreachable!(),
        }
    }

    /// Generate a random valid opcode.
    fn gen_opcode(&mut self) -> u8 {
        loop {
            let opcode: u8 = self.rng.generate();
            if rcl_fuzz::smith::parse_opcode(opcode).is_some() {
                return opcode;
            }
        }
    }

    /// Generate an instruction argument.
    fn gen_argument(&mut self) -> u8 {
        // We bias the argument towards smaller numbers, because often they are
        // lengths or indexes into the stack, and those are all small.
        match self.rng.generate_range(0..4) {
            0 => 0,
            1 => 1,
            2 => self.rng.generate_range(0..10),
            3 => self.rng.generate(),
            _ => unreachable!(),
        }
    }

    fn mutate(&mut self) {
        match self.rng.generate_range(0..9) {
            0 => self.insert_instruction(),
            1 => self.remove_instruction(),
            2 => self.replace_instruction(),
            3 => self.swap_instructions(),
            4 => self.increment_argument(),
            5 => self.decrement_argument(),
            6 => self.replace_argument(),
            7 => self.append_byte(),
            8 => self.remove_byte(),
            _ => unreachable!(),
        }
    }

    fn insert_instruction(&mut self) {
        let i = self.gen_instruction_index();

        // Move everything behind the insertion place one instruction ahead.
        self.data.copy_within(i..self.data.len() - 2, i + 2);

        // Then insert the new instruction.
        self.data[i] = self.gen_opcode();
        self.data[i + 1] = self.gen_argument();
        self.size += 2;
    }

    fn remove_instruction(&mut self) {
        let i = self.gen_instruction_index();

        // Move everything back one place.
        self.data.copy_within(i + 2.., i);
        self.size -= 2;
    }

    fn replace_instruction(&mut self) {
        let i = self.gen_instruction_index();
        self.data[i] = self.gen_opcode();
        self.data[i + 1] = self.gen_argument();
    }

    fn swap_instructions(&mut self) {
        let i = self.gen_instruction_index();
        let j = self.gen_instruction_index();
        self.data.swap(i, j);
        self.data.swap(i + 1, j + 1);
    }

    fn increment_argument(&mut self) {
        let i = self.gen_instruction_index();
        self.data[i + 1] = self.data[i + 1].saturating_add(1);
    }

    fn decrement_argument(&mut self) {
        let i = self.gen_instruction_index();
        self.data[i + 1] = self.data[i + 1].saturating_sub(1);
    }

    fn replace_argument(&mut self) {
        let i = self.gen_instruction_index();
        self.data[i + 1] = self.gen_argument();
    }

    fn append_byte(&mut self) {
        if self.size >= self.data.len() || self.max_size >= self.data.len() {
            return;
        }
        // Bias values towards 0 or printable ASCII, the auxiliary data at the
        // end is often used for indices or strings.
        let b = match self.rng.generate_range(0..2) {
            0 => 0,
            1 => self.rng.generate_range(0x20..0x7f),
            2 => self.rng.generate(),
            _ => unreachable!(),
        };
        self.data[self.size] = b;
        self.size += 1;
    }

    fn remove_byte(&mut self) {
        let i = self.gen_data_index();
        self.data.copy_within(i + 1.., i);
        self.size -= 1;
    }
}

fuzz_mutator!(|data: &mut [u8], size: usize, max_size: usize, seed: u32| {
    let rng = WyRand::new_seed(seed as u64);
    let mut mutator = Mutator {
        data,
        size,
        max_size,
        rng,
    };
    mutator.mutate();
    mutator.size
});
