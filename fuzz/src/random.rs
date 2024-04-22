// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! This module contains an implementation of the Wyhash PRNG.
//!
//! Based on [_Modern Non-Cryptographic Hash Function and Pseudorandom Number
//! Generator_](https://github.com/wangyi-fudan/wyhash) by Yi Wang,
//! Diego Barrios Romero, Daniel Lemire, and Li Jin.
//!
//! The reference implementation in C/C++ is licensed under the Unlicense which
//! places it in the public domain.

use std::ops::Range;

// The `mum` function from page 11 of the paper translated to Rust.
#[inline(always)]
fn mum(a: u64, b: u64) -> u64 {
    let c = (a as u128) * (b as u128);
    ((c >> 64) ^ c) as u64
}

// The `wyrand` function from page 12 of the paper translated to Rust.
// The primes are taken from the C source linked above.
#[inline(always)]
fn wyrand(seed: &mut u64) -> u64 {
    const P0: u64 = 0xa0761d6478bd642f;
    const P1: u64 = 0xe7037ed1a0b428db;
    *seed = seed.wrapping_add(P0);
    mum(*seed ^ P1, *seed)
}

pub struct WyRand(u64);

impl WyRand {
    pub fn new(seed: u64) -> WyRand {
        WyRand(seed)
    }

    #[inline]
    pub fn next_u64(&mut self) -> u64 {
        wyrand(&mut self.0)
    }

    #[inline]
    pub fn next_u8(&mut self) -> u8 {
        self.next_u64() as u8
    }

    #[inline]
    pub fn next_range_usize(&mut self, r: Range<usize>) -> usize {
        // Note, this implementation is biased. When the length of the range is
        // not a divisor of 2^64, then the last "bucket" for the modulo operation
        // is smaller, so some outcomes are less likely. This is noticeable when
        // the size of the range is large w.r.t. 2^64, but my use case is tiny
        // ranges so it's acceptable. A more proper way would be to re-sample
        // if we generate a number in the final bucket.
        r.start + (self.next_u64() as usize % r.len())
    }

    #[inline]
    pub fn next_range_u8(&mut self, r: Range<u8>) -> u8 {
        // See also the note in `next_range_usize`. For `u8` it matters even
        // less because the range is tiny.
        r.start + (self.next_u64() % r.len() as u64) as u8
    }
}

#[test]
pub fn test_reference_tinyrand() {
    use tinyrand::{Rand, Seeded};
    for seed in [0, 1, 2, 0xffff, 0x1ffff_fff55] {
        let mut rcl_wyrand = WyRand::new(seed);
        let mut ref_wyrand = tinyrand::Wyrand::seed(seed);
        for _ in 0..10_000 {
            assert_eq!(rcl_wyrand.next_u64(), ref_wyrand.next_u64());
        }
    }
}
