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

impl From<ParseResult> for Decimal {
    fn from(r: ParseResult) -> Decimal {
        match r {
            ParseResult::Int(i) => Decimal::from(i),
            ParseResult::Decimal(d) => d,
        }
    }
}

impl Decimal {
    /// Parse a json number into either a decimal or an integer.
    ///
    /// Assumes the string is already validated (by the lexer), panics on
    /// unknown characters. Returns `None` on overflow.
    ///
    /// To aid testability and use in other places than the parser, this
    /// function can handle inputs that start with a minus sign, even though
    /// the lexer considers the minus sign a separate token. In fact, the minus
    /// can appear anywhere, which is not valid syntax, but we don't care about
    /// that here!
    pub fn parse_str(dec: &str) -> Option<ParseResult> {
        // When we count the number of decimals, and the exponent, we may
        // increment those counts for every digit we parse, so after 255
        // decimals we may overflow the `decimals` counter, and after 16 KiB of
        // zeros we may overflow the `exponent_offset` counter. We could do a
        // checked add for that, but if you have a 256-byte number literal, then
        // what are you even doing? We just disallow that.
        if dec.len() > 255 {
            return None;
        }

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
                    // the exponent if we are still parsing an int. This does
                    // not overflow because we validated the length of the input
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
                            // This add does not overflow because the input is
                            // at most 255 bytes long.
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
                b'e' | b'E' => is_exp = true,
                b'_' => {
                    // Numeric underscores are allowed, we just ignore them.
                    // This way we don't have to allocate a new string to filter
                    // them out before we parse the number.
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

        if is_int && !is_exp {
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

    pub fn checked_neg(&self) -> Option<Decimal> {
        let result = Decimal {
            mantissa: self.mantissa.checked_neg()?,
            decimals: self.decimals,
            exponent: self.exponent,
        };
        Some(result)
    }

    pub fn checked_add(&self, other: &Decimal) -> Option<Decimal> {
        let (mut d1, mut d2) = (*self, *other);

        // To add, we first make the number of decimals equal, then we make the
        // exponents equal, by moving powers of 10 out of decimals/exponent and
        // into the mantissa. This is a bit naive, there are cases where it
        // overflows that we could avoid, but it'll do for now.

        // The number of decimals in the result is the larger of the two.
        if d1.decimals < d2.decimals {
            std::mem::swap(&mut d1, &mut d2);
        }
        if d1.decimals > d2.decimals {
            let f = 10_i64.checked_pow((d1.decimals - d2.decimals) as u32)?;
            d2.mantissa = d2.mantissa.checked_mul(f)?;
            d2.decimals = d1.decimals;
        }

        // The exponent of the result is the smaller of the two.
        if d1.exponent > d2.exponent {
            std::mem::swap(&mut d1, &mut d2);
        }
        if d1.exponent < d2.exponent {
            // We know d2 > d1, so d2 - d1 is positive, but the difference may
            // not fit in an i16 when d2 is close to i16::MAX and d1 is close to
            // i16::MIN, so widen to i32 before subtracting.
            let f = 10_i64.checked_pow((d2.exponent as i32 - d1.exponent as i32) as u32)?;
            d2.mantissa = d2.mantissa.checked_mul(f)?;
            d2.exponent = d1.exponent;
        }

        let result = Decimal {
            mantissa: d1.mantissa.checked_add(d2.mantissa)?,
            decimals: d1.decimals,
            exponent: d1.exponent,
        };
        Some(result)
    }

    pub fn checked_sub(&self, other: &Decimal) -> Option<Decimal> {
        self.checked_add(&other.checked_neg()?)
    }

    pub fn checked_mul(&self, other: &Decimal) -> Option<Decimal> {
        // TODO: This treatment of the decimals is a bit naive; it uses the
        // maximum decimals needed, but possibly we can do it in less. E.g. this
        // turns "1.0 * 1.0" into "1.00", but we could just return "1.0". We
        // should try to eliminate powers of 10 until the result has as many
        // decimals as one of the inputs.
        let result = Decimal {
            mantissa: self.mantissa.checked_mul(other.mantissa)?,
            decimals: self.decimals.checked_add(other.decimals)?,
            exponent: self.exponent.checked_add(other.exponent)?,
        };
        Some(result)
    }

    /// Divide two numbers, but only if `self` is an exact multiple of `other`.
    pub fn checked_div_exact(&self, other: &Decimal) -> Option<Decimal> {
        debug_assert_ne!(other.mantissa, 0, "Division by zero is not allowed.");

        // TODO: This could be smarter, now it might overflow in cases where
        // that is not strictly needed.
        let y = other.mantissa;
        let f = 10_i64.checked_pow(other.decimals as u32)?;
        let mut x = self.mantissa.checked_mul(f)?;

        let mut q = x / y;
        let mut d = self.decimals;

        // If the division doesn't work, add more decimals and retry. Either we
        // find a case where it works, or x is missing other factors than 2 or 5
        // and it will never work, but then we'll hit overflow in at most 19
        // iterations.
        while q * y != x {
            x = x.checked_mul(10)?;
            d = d.checked_add(1)?;
            q = x / y;
        }

        let result = Decimal {
            mantissa: q,
            decimals: d,
            exponent: self.exponent.checked_sub(other.exponent)?,
        };

        Some(result)
    }

    /// Round the number to the nearest multiple of 10^{-n_decimals}.
    ///
    /// This moves the exponent into the mantissa, so the result always has the
    /// exponent set to 0. This means that round is not that useful beyond the
    /// range of an i64, but that's fine for now. Rounds away from 0.
    ///
    /// Returns `None` on overflow.
    pub fn round(&self, n_decimals: u8) -> Option<Decimal> {
        match n_decimals as i32 - self.decimals as i32 + self.exponent as i32 {
            // The mantissa is already correct for the requested number of decimals.
            0 => {
                let result = Decimal {
                    mantissa: self.mantissa,
                    exponent: 0,
                    decimals: n_decimals,
                };
                Some(result)
            }

            // We have to add additional decimals. The maximum power of 2 we can
            // represent is 2^18, beyond that we will certainly get overflow.
            d if d > 0 && d <= 18 => {
                let f = 10_i64.pow(d as u32);
                let result = Decimal {
                    mantissa: self.mantissa.checked_mul(f)?,
                    exponent: 0,
                    decimals: n_decimals,
                };
                Some(result)
            }

            // d is negative, we have to remove decimals.
            d if d < 0 && d >= -18 => {
                let f = 10_i64.pow((-d) as u32);
                // We want `round`, not `floor`, so add half the range.
                // Negate if the mantissa is negative so we round away from 0.
                let b = if self.mantissa > 0 { f / 2 } else { -f / 2 };
                let result = Decimal {
                    mantissa: self.mantissa.saturating_add(b) / f,
                    exponent: 0,
                    decimals: n_decimals,
                };
                Some(result)
            }

            // We have to remove so many decimals that none remain. In other
            // words, this factor f that we compute in the branch above, it
            // doesn't fit in an i64, which means that if we divide the mantissa
            // by it, we get zero.
            d if d < -18 => {
                let result = Decimal {
                    mantissa: 0,
                    exponent: 0,
                    decimals: n_decimals,
                };
                Some(result)
            }

            d => {
                // The factor f = 10^d does not fit in an i64, so we would
                // overflow the mantissa.
                debug_assert!(d > 18, "All other branches are covered.");
                None
            }
        }
    }

    /// Convert to a float. For many decimals this will be a lossy operation.
    pub fn to_f64_lossy(&self) -> f64 {
        let n = self.mantissa as f64;
        let exp = 10.0_f64.powi(self.exponent as i32 - self.decimals as i32);
        n * exp
    }

    /// Extract an integer, if the number is an integer.
    ///
    /// This cares only about numeric integers, not about formatting. For
    /// example, all of `1`, `0.1e1`, and `1.00` would return `Some(1)`.
    pub fn to_i64(&self) -> Option<i64> {
        match self.exponent as i32 - self.decimals as i32 {
            0 => Some(self.mantissa),
            exp if exp > 0 => {
                let f = 10_i64.checked_pow(exp as u32)?;
                self.mantissa.checked_mul(f)
            }
            exp => {
                // If we get to this branch, `exp < 0`, it's a case of e.g.
                // 1.00 == 100e-2, which is numerically an integer.
                let f = 10_i64.checked_pow((-exp) as u32)?;
                let n = self.mantissa / f;
                if n * f == self.mantissa {
                    Some(n)
                } else {
                    None
                }
            }
        }
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
                // are both non-negative. If m1 scaled to the same range as m2
                // overflows, then it's greater than m2, so the difference is
                // positive.
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
            // coverage:off -- This branch shouldn't be hit when the assertion is not.
            _ => false,
            // coverage:on
        };

        assert!(
            is_ok,
            // coverage:off
            "Expected '{num}' to parse as {expected_mantissa}e\
            ({expected_exponent}-{expected_decimals}), \
            but got {result:#?}",
            // coverage:on
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
            // coverage:off
            "Unexpected comparison result for {lhs_str} ({lhs:?}) vs. {rhs_str} ({rhs:?}).",
            // coverage:on
        );
    }

    fn assert_binop(expr: &str) {
        let mut parts = expr.split(' ');
        let lhs_str = parts.next().unwrap();
        let op_str = parts.next().unwrap();
        let rhs_str = parts.next().unwrap();
        let eq_str = parts.next().unwrap();
        let res_str = parts.next().unwrap();
        let lhs: Decimal = match Decimal::parse_str(lhs_str) {
            Some(r) => r.into(),
            _ => panic!("Expected a decimal as left-hand side, not '{lhs_str}'."),
        };
        let rhs: Decimal = match Decimal::parse_str(rhs_str) {
            Some(r) => r.into(),
            _ => panic!("Expected a decimal as right-hand side, not '{rhs_str}'."),
        };
        assert_eq!(eq_str, "==");

        let actual = match op_str {
            "+" => lhs.checked_add(&rhs),
            "-" => lhs.checked_sub(&rhs),
            "*" => lhs.checked_mul(&rhs),
            "/" => lhs.checked_div_exact(&rhs),
            _ => panic!("Unexpected operator '{op_str}'."),
        };

        // Note, we compare the formatted value, not the parsed result, because
        // we want the formatting to match, we don't want just a numeric match!
        assert_eq!(
            actual.map(|x| x.format()),
            Some(res_str.to_string()),
            // coverage:off
            "Unexpected result for {lhs_str} ({lhs:?}) {op_str} {rhs_str} ({rhs:?}):\
            got {actual:?}",
            // coverage:on
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
        assert_parse_decimal("0e0", 0, 0, 0);
        assert_parse_decimal("1.0e42", 10, 1, 42);
        assert_parse_decimal("1.0e+42", 10, 1, 42);
        assert_parse_decimal("1.0e-42", 10, 1, -42);
        assert_parse_decimal("0.00500e5", 500, 5, 5);
        assert_parse_decimal("300e-3", 300, 0, -3);

        // We should be able to reach the maximum exponents.
        assert_parse_decimal("1e32767", 1, 0, i16::MAX);
        assert_parse_decimal("1e-32768", 1, 0, i16::MIN);

        // This is a regression test, e0 we parse as exponent 0.
        assert_parse_decimal("1e0", 1, 0, 0);
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
        assert_cmp("-1e100 > -1e110");
        assert_cmp("-8577.55471122393 > -2.775282690723978e36");

        // This one is a regression test, the fuzz_smith fuzzer found an
        // overflow when subtracting the exponents.
        assert_cmp("5.0e-6504 < 5.0e26505");
        assert_cmp("5.0e26505 > 5.0e-6504");

        // This is a regression test for a bug found by the fuzz_decimal fuzzer.
        assert_cmp("-0.1 < 0e0");
        assert_cmp("0.1 > 0e0");
    }

    #[test]
    fn decimal_to_i64_works_for_floats() {
        assert_eq!(decimal(100, 2, 0).to_i64(), Some(1), "1.00 == 1");
        assert_eq!(decimal(10, 1, 0).to_i64(), Some(1), "1.0 == 1");
        assert_eq!(decimal(1, 0, 0).to_i64(), Some(1), "1 == 1");
        assert_eq!(decimal(10, 0, -1).to_i64(), Some(1), "10e-1 == 1");
        assert_eq!(decimal(100, 0, -1).to_i64(), Some(10), "100e-1 == 10");
        assert_eq!(decimal(1, 1, 0).to_i64(), None, "0.1 is not an int");
    }

    #[test]
    fn decimal_checked_add() {
        // Decimals is the most of the two inputs.
        assert_binop("1.0 + 1 == 2.0");
        assert_binop("1 + 1.0 == 2.0");
        assert_binop("1.000 + 0.01 == 1.010");
        assert_binop("-1.00 + 1.00 == 0.00");

        // Same for subtraction
        assert_binop("1.0 - 2 == -1.0");
        assert_binop("1 - 3.0 == -2.0");
        assert_binop("1.000 - 0.01 == 0.990");
        assert_binop("-1.00 - -1.00 == 0.00");

        // The exponent is the smaller of the two.
        assert_binop("1e2 + 0 == 100");
        assert_binop("1e2 + 0.1e5 == 101.0e2");

        // The number of decimals is the sum of the two.
        assert_binop("1.0 * 1.0 == 1.00");
        assert_binop("0.5 * 0.5 == 0.25");
        assert_binop("2 * 21 == 42");

        // The exponent is the sum of the exponents.
        assert_binop("1e1 * 1e20 == 1e21");
        assert_binop("1e100 * 1e-100 == 1");

        // Division preserves the number of decimals in the numerator if
        // possible, and extends it when needed.
        assert_binop("10 / 5 == 2");
        assert_binop("1 / 5 == 0.2");
        assert_binop("0.1 / 5 == 0.02");
        assert_binop("1.000 / 2 == 0.500");
        assert_binop("1.000 / 20 == 0.050");
    }

    #[test]
    fn decimal_to_i64() {
        assert_eq!(decimal(1, 0, 0).to_i64(), Some(1));
        assert_eq!(decimal(10, 0, 0).to_i64(), Some(10));
        assert_eq!(decimal(1, 0, 1).to_i64(), Some(10));
        assert_eq!(decimal(10, 1, 0).to_i64(), Some(1));
        assert_eq!(decimal(1, 1, 1).to_i64(), Some(1));
        assert_eq!(decimal(1, 1, 0).to_i64(), None);
        assert_eq!(decimal(1, 0, 20).to_i64(), None);
    }

    #[test]
    fn decimal_to_f64_lossy() {
        assert_eq!(decimal(1, 0, 0).to_f64_lossy(), 1.0);
        assert_eq!(decimal(10, 0, 0).to_f64_lossy(), 10.0);
        assert_eq!(decimal(1, 0, 1).to_f64_lossy(), 10.0);
        assert_eq!(decimal(1, 1, 1).to_f64_lossy(), 1.0);
        assert_eq!(decimal(1, 1, 0).to_f64_lossy(), 0.1);
        assert_eq!(decimal(1, 0, 20).to_f64_lossy(), 1e20);
        assert_eq!(decimal(-1, 5, 20).to_f64_lossy(), -0.00001e20);

        // This one is a neat example of where the loss occurs, if we parsed as
        // 1.2 directly then there is a float that is closer, but due to our
        // stepwise arithmetic involving powers of 10, some loss occurs.
        assert_eq!(decimal(12, 1, 0).to_f64_lossy(), 1.2000000000000002);
    }
}
