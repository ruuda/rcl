// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Implementation of decimal and rational numbers for use in the interpreter.

use std::cmp::Ordering;

/// A rational number of the form `m × 10^(n - d)`.
#[derive(Copy, Clone, Debug)]
pub struct Decimal {
    /// The value `m`.
    pub mantissa: i64,
    /// The exponent `n`.
    pub exponent: i16,
    /// The exponent `d`, which is also the number of decimal digits.
    pub decimals: u8,
}

impl From<i64> for Decimal {
    fn from(x: i64) -> Decimal {
        Decimal {
            mantissa: x,
            decimals: 0,
            exponent: 0,
        }
    }
}

#[derive(Debug)]
pub enum ParseResult {
    Int(i64),
    Decimal(Decimal),
}

impl Decimal {
    /// Parse a json number into either a decimal or an integer.
    ///
    /// Assumes the string is already validated (by the lexer), panics on
    /// unknown characters. Returns `None` on overflow.
    ///
    /// To aid testability and use in other places than the parser, this
    /// function can handle inputs that start with a minus sign, even though
    /// the lexer considers the minus sign a separate token.
    pub fn parse_str(dec: &str) -> Option<ParseResult> {
        let mut n: i64 = 0;
        let mut decimals: u8 = 0;
        let mut exponent: i16 = 0;
        let mut exponent_offset: i16 = 0;

        let mut is_int = true;
        let mut is_exp = false;
        let mut is_precise = true;
        let mut exponent_sign: i16 = 1;
        let mut mantissa_sign: i64 = 1;

        for ch in dec.as_bytes() {
            match ch {
                b'0'..=b'9' if is_exp => {
                    // Note, we apply the sign at every step, rather than once
                    // at the end, otherwise we would be unable to parse i16::MIN,
                    // whose absolute value is one more than i16::MAX.
                    exponent = exponent
                        .checked_mul(10)?
                        .checked_add((ch - b'0') as i16 * exponent_sign)?;
                }
                b'0'..=b'9' if !is_precise => {
                    // If the mantissa is already saturated, but we keep getting
                    // more digits, we just drop them, but we do need to adjust
                    // the exponent if we are still parsing an int.
                    // TODO: This can in theory overflow with a 16 kB input
                    // string.
                    exponent_offset += if is_int { 1 } else { 0 };
                }
                b'0'..=b'9' => {
                    match n
                        .checked_mul(10)
                        .and_then(|n10| n10.checked_add((ch - b'0') as i64 * mantissa_sign))
                    {
                        // We added one digit to the mantissa and it still fits.
                        Some(m) => {
                            n = m;
                            // TODO: This can overflow on numbers that start with 0.0000,
                            // needs to be a checked add.
                            decimals += if is_int { 0 } else { 1 };
                        }
                        // The mantissa is saturated, we switch to dropping
                        // digits. If appropriate and when possible, round up
                        // the last digit we had.
                        None => {
                            let round_up = *ch >= b'5';
                            n = n.saturating_add(if round_up { 1 } else { 0 });
                            exponent_offset += if is_int { 1 } else { 0 };
                            is_precise = false;
                        }
                    }
                }
                b'.' => is_int = false,
                b'+' => {}
                b'-' if is_exp => exponent_sign = -1,
                b'-' => mantissa_sign = -1,
                b'e' | b'E' => {
                    is_int = false;
                    is_exp = true;
                }
                bad_byte => panic!("Invalid input byte for 'parse_str': 0x{bad_byte:x}"),
            }
        }

        exponent = exponent.checked_add(exponent_offset)?;

        // If we parsed something that was not an integer, but due to precision
        // limits we ended up having no decimals and exponent, then if we would
        // format it later, we would format it as int rather than float. Adjust
        // for that by bumping the decimals, and compensating in the exponent.
        // This means we parse this number's formatting as lossy, but avoiding
        // silently turning floats into ints is more important than preserving
        // the exact formatting of the number.
        if !is_int && decimals == 0 && exponent == 0 {
            decimals = 1;
            exponent = if n == 0 { 0 } else { 1 };
        }

        if is_int {
            // For integers, if the literal is too large to fit in an i64 and we
            // switched to imprecise, we don't want to implicitly turn that
            // integer literal into a float, so treat that as overflow.
            if is_precise {
                Some(ParseResult::Int(n))
            } else {
                None
            }
        } else {
            Some(ParseResult::Decimal(Decimal {
                mantissa: n,
                decimals,
                exponent,
            }))
        }
    }

    /// Format the number.
    ///
    /// The formatting is authentic to the representation, which means that for
    /// extreme values of `decimals`, we might get unreasonably large strings.
    /// To avoid that, normalize the number before formatting it. We don't
    /// normalize implicitly, so that the format of numbers in the source can
    /// be preserved exactly in the output.
    pub fn format(&self) -> String {
        let mut result = self.mantissa.to_string();

        // If the result starts with a minus sign, we need to leave it in place
        // when inserting the decimal point.
        let neg_off = if self.mantissa < 0 { 1 } else { 0 };

        if self.decimals > 0 {
            while self.decimals as usize + neg_off >= result.len() {
                result.insert(neg_off, '0');
            }
            result.insert(result.len() - self.decimals as usize, '.');
        }

        if self.exponent != 0 {
            result.push('e');
            result.push_str(&self.exponent.to_string());
        }

        result
    }

    /// Move all powers of 10 from the mantissa into the exponent.
    ///
    /// TODO: Update this to pick a reasonable formatting based on the number
    /// range.
    ///
    /// This might fail when the new exponent does not fit in 16 bits.
    pub fn normalize(&self) -> Option<Decimal> {
        let mut normed = *self;
        while normed.mantissa % 10 == 0 {
            normed.mantissa /= 10;
            normed.exponent = normed.exponent.checked_add(1)?;
        }
        Some(normed)
    }

    pub fn checked_neg(&self) -> Option<Decimal> {
        let result = Decimal {
            mantissa: self.mantissa.checked_neg()?,
            decimals: self.decimals,
            exponent: self.exponent,
        };
        Some(result)
    }

    /// Convert to a float. For many decimals this will be a lossy operation.
    pub fn to_f64_lossy(&self) -> f64 {
        let n = self.mantissa as f64;
        let exp = 10.0_f64.powi(self.exponent as i32 - self.decimals as i32);
        n * exp
    }

    /// Compare two decimals.
    ///
    /// This is used for equality testing as well as implementing the comparison
    /// operator. We mark it inline so that LLVM can specialize it for both.
    ///
    /// The central idea behind the comparison is to subtract the two decimals
    /// and then classify the result. In order to do that, we first need to
    /// make their exponents equal.
    #[inline(always)]
    fn cmp_impl(&self, other: &Decimal) -> Ordering {
        let m1 = self.mantissa;
        let m2 = other.mantissa;

        let e1 = self.exponent as i32 - self.decimals as i32;
        let e2 = other.exponent as i32 - other.decimals as i32;

        match e1 - e2 {
            // If both exponents are the same, we can just compare the mantissas.
            0 => m1.cmp(&m2),

            // If the numbers lie on different sides of 0 on the number line,
            // then we can just compare the mantissas, because scaling by the
            // exponent does not change which side the numbers lie on.
            _ if m1.signum() != m2.signum() => m1.cmp(&m2),

            // Case where the first number has the greater exponent. To compare
            // the two, we can look at the sign of their difference. The
            // difference can be written as:
            //
            //  m1 * 10^(e2 + d) - m2 * 10^e2 = 10^e2 * (m1 * 10^d - m2)
            //
            // The factor in front doesn't change the sign, so we can ignore it,
            // and compare the scaled m1 against m2.
            d if d > 0 => match 10_i64
                .checked_pow(d as u32)
                .and_then(|factor| m1.checked_mul(factor))
            {
                Some(m1_scaled) => m1_scaled.cmp(&m2),
                // We know m1 and m2 have the same sign, so in this case they
                // are both nonnegative. If m1 scaled to the same range as m2
                // overflows, then it's greater than m2, so the difference is
                // positive. TODO: There is an off by one, because the negative
                // range is 1 more than the positive range.
                None if m1 >= 0 => Ordering::Greater,
                // If m1 and m2 are both negative, the same logic applies, but
                // the sign of the comparison flips.
                None => Ordering::Less,
            },

            // Case where the second number has the greater exponent, symmetric
            // with the case above.
            d => {
                match 10_i64
                    .checked_pow(-d as u32)
                    .and_then(|factor| m2.checked_mul(factor))
                {
                    Some(m2_scaled) => m1.cmp(&m2_scaled),
                    None if m1 >= 0 => Ordering::Less,
                    None => Ordering::Greater,
                }
            }
        }
    }
}

impl PartialEq for Decimal {
    fn eq(&self, other: &Decimal) -> bool {
        self.cmp_impl(other) == Ordering::Equal
    }
}

impl Eq for Decimal {}

impl Ord for Decimal {
    fn cmp(&self, other: &Decimal) -> Ordering {
        self.cmp_impl(other)
    }
}

impl PartialOrd for Decimal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod test {
    use super::{Decimal, ParseResult};
    use std::cmp::Ordering;

    fn assert_parse_decimal(
        num: &str,
        expected_mantissa: i64,
        expected_decimals: u8,
        expected_exponent: i16,
    ) {
        let result = Decimal::parse_str(num);
        let is_ok = match result {
            Some(ParseResult::Decimal(d)) => {
                // Note, we explicitly check the mantissa and exponent here,
                // we don't want equivalence (mathematical equality) of the
                // decimals, we check the representation (structural equality).
                d.mantissa == expected_mantissa
                    && d.decimals == expected_decimals
                    && d.exponent == expected_exponent
            }
            _ => false,
        };

        assert!(
            is_ok,
            "Expected '{num}' to parse as {expected_mantissa}e\
            ({expected_exponent}-{expected_decimals}), \
            but got {result:#?}"
        );
    }

    fn assert_cmp(expr: &str) {
        let mut parts = expr.split(' ');
        let lhs_str = parts.next().unwrap();
        let cmp_str = parts.next().unwrap();
        let rhs_str = parts.next().unwrap();
        let lhs = match Decimal::parse_str(lhs_str) {
            Some(ParseResult::Decimal(d)) => d,
            _ => panic!("Expected a decimal as left-hand side, not '{lhs_str}'."),
        };
        let rhs = match Decimal::parse_str(rhs_str) {
            Some(ParseResult::Decimal(d)) => d,
            _ => panic!("Expected a decimal as right-hand side, not '{rhs_str}'."),
        };

        let expected_ord = match cmp_str {
            "<" => Ordering::Less,
            ">" => Ordering::Greater,
            "=" => Ordering::Equal,
            _ => panic!("Unexpected comparison '{cmp_str}', need one of <, >, =."),
        };
        assert_eq!(
            lhs.cmp(&rhs),
            expected_ord,
            "Unexpected comparison result for {lhs_str} ({lhs:?}) vs. {rhs_str} ({rhs:?}).",
        );
    }

    /// Shorthand to make tests briefer.
    ///
    /// But also less readable, we don't make this `Decimal::new`.
    fn decimal(mantissa: i64, decimals: u8, exponent: i16) -> Decimal {
        Decimal {
            mantissa,
            decimals,
            exponent,
        }
    }

    #[test]
    fn decimal_parse_str_parses_int() {
        assert!(matches!(Decimal::parse_str("0"), Some(ParseResult::Int(0))));
        assert!(matches!(Decimal::parse_str("1"), Some(ParseResult::Int(1))));
        assert!(matches!(
            Decimal::parse_str("10"),
            Some(ParseResult::Int(10))
        ));
        assert!(matches!(
            Decimal::parse_str(&i64::MAX.to_string()),
            Some(ParseResult::Int(i64::MAX))
        ));
        assert!(matches!(
            Decimal::parse_str(&(i64::MAX as u64 + 1).to_string()),
            None
        ));
    }

    #[test]
    fn decimal_parse_str_parses_decimal() {
        assert_parse_decimal("0.0", 0, 1, 0);
        assert_parse_decimal("0.", 0, 1, 0);
        assert_parse_decimal("10.0", 100, 1, 0);
        assert_parse_decimal("0.1", 1, 1, 0);
    }

    #[test]
    fn decimal_parse_str_parses_decimal_imprecise() {
        // This magic number is the overflow point.
        assert_eq!(i64::MAX, 9223372036854775807);

        // Without the switch to imprecise parsing, we'd overflow the i64
        // mantissa, and we would not be able to parse this, but with the switch
        // we can handle arbitrarily long decimals (but not in full precision).
        // In the case below the overflow happens before the decimal point. We
        // also test that rounding works as expected. Note that none of these
        // have both the exponent and decimals at 0, if they did we would format
        // as integer, but the inputs have a decimal point, so we should not
        // silently turn them into ints. (Dropping the decimal point is
        // acceptable in this weird case, but only because we add an exponent
        // which then sets the number apart from an integer.)
        assert_parse_decimal("9223372036854775807.0", 9223372036854775807, 1, 1);
        assert_parse_decimal("9223372036854775808.0", 922337203685477581, 0, 1);
        assert_parse_decimal("9223372036854775814.0", 922337203685477581, 0, 1);
        assert_parse_decimal("9223372036854775815.0", 922337203685477582, 0, 1);

        // And here it happens after the decimal point.
        assert_parse_decimal("0.9223372036854775807", 9223372036854775807, 19, 0);
        assert_parse_decimal("0.9223372036854775808", 922337203685477581, 18, 0);
        assert_parse_decimal("0.9223372036854775814", 922337203685477581, 18, 0);
        assert_parse_decimal("0.9223372036854775815", 922337203685477582, 18, 0);
    }

    #[test]
    fn decimal_parse_str_parses_exponent() {
        // The 0e0 acquires one decimal, because otherwise we can't tell it's
        // not an integer.
        assert_parse_decimal("0e0", 0, 1, 0);
        assert_parse_decimal("1.0e42", 10, 1, 42);
        assert_parse_decimal("1.0e+42", 10, 1, 42);
        assert_parse_decimal("1.0e-42", 10, 1, -42);
        assert_parse_decimal("0.00500e5", 500, 5, 5);
        assert_parse_decimal("300e-3", 300, 0, -3);

        // We should be able to reach the maximum exponents.
        assert_parse_decimal("1e32767", 1, 0, i16::MAX);
        assert_parse_decimal("1e-32768", 1, 0, i16::MIN);
    }

    #[test]
    fn decimal_format_works() {
        assert_eq!(decimal(0, 0, 0).format(), "0");
        assert_eq!(decimal(0, 1, 0).format(), "0.0");
        assert_eq!(decimal(1, 0, 0).format(), "1");
        assert_eq!(decimal(10, 1, 0).format(), "1.0");
        assert_eq!(decimal(1, 0, 1).format(), "1e1");
        assert_eq!(decimal(1, 1, 0).format(), "0.1");

        // Note, the mantissa matters for how many decimals we get!
        assert_eq!(decimal(10, 2, 0).format(), "0.10");
        assert_eq!(decimal(10, 1, 0).format(), "1.0");
        assert_eq!(decimal(1, 6, 0).format(), "0.000001");
        assert_eq!(decimal(1, 0, 6).format(), "1e6");
    }

    #[test]
    fn decimal_format_handles_negative_numbers() {
        assert_eq!(decimal(-1000, 1, 0).format(), "-100.0");
        assert_eq!(decimal(-1, 2, 0).format(), "-0.01");
        assert_eq!(decimal(-9000009, 6, -8).format(), "-9.000009e-8");
        assert_eq!(decimal(-7, 0, 25).format(), "-7e25");
        assert_eq!(decimal(-70, 1, 25).format(), "-7.0e25");
    }

    #[test]
    fn decimal_parse_after_format_is_identity() {
        // This test is only a quick verification; the same property will be
        // exercised more thoroughly by the fuzzer that verifies that evaluation
        // to json is idempotent. We don't test negative mantissas here because
        // in the full parser the - is a unary operator, not part of the number.
        for mantissa in (0..999).step_by(7) {
            for decimals in 0..20 {
                for exponent in (-999..999).step_by(11) {
                    let d1 = Decimal {
                        mantissa,
                        decimals,
                        exponent,
                    };
                    let s = d1.format();
                    match Decimal::parse_str(&s).unwrap() {
                        ParseResult::Int(..) => {
                            panic!("Formatting a decimal should parse as decimal.")
                        }
                        ParseResult::Decimal(d2) => {
                            assert_eq!(d1, d2);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn decimal_ord_works() {
        assert_cmp("1e0 < 2e0");
        assert_cmp("2e0 < 10e0");
        assert_cmp("10e0 < 11e0");

        assert_cmp("1e0 < 1e1");
        assert_cmp("2e0 < 1e1");
        assert_cmp("10e0 = 1e1");
        assert_cmp("11e0 > 1e1");
        assert_cmp("11e0 < 2e1");
        assert_cmp("11e0 < 10e1");

        assert_cmp("1e-1 < 1e0");
        assert_cmp("2e-1 < 1e0");
        assert_cmp("10e-1 = 1e0");
        assert_cmp("10e-1 < 1e1");
        assert_cmp("11e-1 > 1e0");
        assert_cmp("11e-1 < 2e0");

        assert_cmp("11e1 > 11e-1");
        assert_cmp("11e-1 < 11e1");

        assert_cmp("-1e0 > -2e0");
        assert_cmp("-2e0 > -10e0");
        assert_cmp("-10e0 > -11e0");

        assert_cmp("-1e0 < 1e0");
        assert_cmp("-1e0 < 1e1");
        assert_cmp("-11e0 < 1e1");
        assert_cmp("-11e0 < 10e0");

        assert_cmp("-1e0 < 1e-1");
        assert_cmp("-1e0 < 10e-1");

        assert_cmp("1e100 > 1e1");
        assert_cmp("1e100 > -1e0");
        assert_cmp("-1e100 < 1e100");
        assert_cmp("-1e100 < 1e1");
        assert_cmp("-1e100 < -1e0");

        // This one is a regression test, the fuzz_smith fuzzer found an
        // overflow when subtracting the exponents.
        assert_cmp("5.0e-6504 < 5.0e26505");
        assert_cmp("5.0e26505 > 5.0e-6504");

        // This is a regression test for a bug found by the fuzz_decimal fuzzer.
        assert_cmp("-0.1 < 0e0");
        assert_cmp("0.1 > 0e0");
    }
}
