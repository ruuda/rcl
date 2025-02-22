// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! A fuzzer to verify various properties of the `Decimal` implementation.

#![no_main]

use std::num::FpCategory;

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::{fuzz_target, Corpus};
use rcl::decimal::{Decimal, ParseResult};

#[derive(Debug)]
struct NormalF64(f64);

impl<'a> Arbitrary<'a> for NormalF64 {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let x: f64 = f64::arbitrary(u)?;
        match x.classify() {
            FpCategory::Normal => Ok(NormalF64(x)),
            FpCategory::Zero => Ok(NormalF64(0.0)),
            _ => Err(arbitrary::Error::IncorrectFormat),
        }
    }
}

#[derive(Arbitrary, Debug)]
enum Input {
    Compare {
        a: NormalF64,
        b: NormalF64,
    },
    Roundtrip {
        mantissa: i64,
        exponent: i16,
        decimals: u8,
    },
}

fuzz_target!(|input: Input| -> Corpus {
    match input {
        // Check that comparison of decimals is consistent with f64.
        Input::Compare { a, b } => {
            let f64_ord = match a.0.partial_cmp(&b.0) {
                None => return Corpus::Reject,
                Some(c) => c,
            };

            // The Display impl does not print using scientific notation, but
            // the Debug impl does. We need that otherwise we get enormous
            // strings where RCL's parser fails due to overflow.
            let a_str = format!("{:?}", a.0);
            let a_dec = match Decimal::parse_str(&a_str) {
                Some(ParseResult::Decimal(d)) => d,
                Some(ParseResult::Int(i)) => Decimal::from(i),
                _ => panic!("Failed to parse: {a_str}"),
            };

            let b_str = format!("{:?}", b.0);
            let b_dec = match Decimal::parse_str(&b_str) {
                Some(ParseResult::Decimal(d)) => d,
                Some(ParseResult::Int(i)) => Decimal::from(i),
                _ => panic!("Failed to parse: {b_str}"),
            };

            // First we test that comparison of the decimals matches the f64
            // comparison, both ways around.
            let decimal_ord = a_dec.cmp(&b_dec);
            assert_eq!(
                decimal_ord, f64_ord,
                "Compare {a_dec:?} vs. {b_dec:?} does not match f64 comparison.",
            );

            let rev_decimal_ord = b_dec.cmp(&a_dec);
            assert_eq!(
                rev_decimal_ord.reverse(),
                f64_ord,
                "Decimal::cmp should be antisymmetric.",
            );

            // Sanity check, a decimal is equal to itself.
            assert_eq!(a_dec, a_dec, "Decimals should be equal to themselves.");
            assert_eq!(b_dec, b_dec, "Decimals should be equal to themselves.");

            Corpus::Keep
        }

        // Check that parsing after formatting a decimal is the identity function.
        Input::Roundtrip {
            mantissa,
            exponent,
            decimals,
        } => {
            let a = Decimal {
                mantissa,
                exponent,
                decimals,
            };

            let a_str = a.format();
            let b = match Decimal::parse_str(&a_str) {
                Some(ParseResult::Decimal(d)) => d,
                Some(ParseResult::Int(i)) => Decimal::from(i),
                _ => panic!("Failed to parse output of Decimal::format: {a_str}"),
            };

            assert_eq!(
                a, b,
                "Decimal::parse after Decimal::format should be identity modulo equality.",
            );

            assert_eq!(
                b.mantissa, b.mantissa,
                "Decimal::parse after Decimal::format should be identity.",
            );
            assert_eq!(
                a.decimals, b.decimals,
                "Decimal::parse after Decimal::format should be identity.",
            );
            assert_eq!(
                a.exponent, b.exponent,
                "Decimal::parse after Decimal::format should be identity.",
            );

            Corpus::Keep
        }
    }
});
