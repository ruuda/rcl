// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Implementation of decimal and rational numbers for use in the interpreter.

use std::cmp::Ordering;

/// A rational number of the form `a × 10^n`.
#[derive(Copy, Clone, Debug)]
pub struct Decimal {
    /// The value `a` (the "numerator").
    pub numer: i64,
    /// The exponent `n`.
    pub exponent: i16,
}

impl From<i64> for Decimal {
    fn from(x: i64) -> Decimal {
        Decimal {
            numer: x,
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
    pub fn parse_str(dec: &str) -> Option<ParseResult> {
        let mut n: i64 = 0;
        let mut exponent: i16 = 0;
        let mut dec_point: i16 = 0;
        let mut is_int = true;
        let mut is_exp = false;
        let mut exp_sign = 1;
        let mut is_precise = true;

        for ch in dec.as_bytes() {
            match ch {
                b'0'..=b'9' if is_exp => {
                    exponent = exponent.checked_mul(10)?.checked_add((ch - b'0') as i16)?;
                }
                b'0'..=b'9' if !is_precise => {
                    // If the mantissa is already saturated, but we keep getting
                    // more digits, then we drop those digits, but we do need to
                    // adjust the exponent if the digits are before the decimal
                    // point.
                    dec_point -= if is_int { 1 } else { 0 };
                }
                b'0'..=b'9' => {
                    match n
                        .checked_mul(10)
                        .and_then(|n10| n10.checked_add((ch - b'0') as i64))
                    {
                        // We added one digit to the mantissa and it still fits.
                        Some(m) => {
                            n = m;
                            dec_point += if is_int { 0 } else { 1 };
                        }
                        // The mantissa is saturated, we switch to dropping
                        // digits. If appropriate and when possible, round up
                        // the last digit we had.
                        None => {
                            let round_up = *ch >= b'5';
                            n = n.saturating_add(if round_up { 1 } else { 0 });
                            dec_point -= if is_int { 1 } else { 0 };
                            is_precise = false;
                        }
                    }
                }
                b'.' => {
                    is_int = false;
                }
                b'+' => {}
                b'-' => {
                    debug_assert!(is_exp, "Minus is only valid inside exponent.");
                    exp_sign = -1;
                }
                b'e' | b'E' => {
                    is_int = false;
                    is_exp = true;
                }
                bad_byte => panic!("Invalid input byte for 'parse_str': 0x{bad_byte:x}"),
            }
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
                numer: n,
                // The first multiply does not overflow because the negative
                // range of a signed int is one larger than the positive range.
                exponent: (exponent * exp_sign).checked_sub(dec_point)?,
            }))
        }
    }

    /// Format the number in scientific notation with exponent.
    ///
    /// Expects the numerator to be already converted to string.
    fn format_scientific(&self, numer_str: String) -> String {
        let mut result = numer_str;

        // We want to use the width of the string to determine the exponent,
        // but that fails for 0, so handle this special case now to simplify
        // the remaining code. We might return 0e0, but it would be dead code
        // as long as this method is not exposed to users, so for now panic.
        debug_assert_ne!(self.numer, 0, "format_scientific is not meant to format 0.");

        // We are going to put one digit before the decimal points and the
        // others after it, so we need to adjust the exponent for that.
        let sign_len = if self.numer < 0 { 1 } else { 0 };
        let implicit_exponent = result.len() as i16 - sign_len - 1;
        result.insert(1 + sign_len as usize, '.');

        // Ensure there is always something after the decimal point, even when
        // there are no significant digits there.
        if result.len() == 2 + sign_len as usize {
            result.push('0');
        }

        let true_exponent = self.exponent + implicit_exponent;
        result.push('e');
        result.push_str(&true_exponent.to_string());

        result
    }

    pub fn format(&self) -> String {
        // Zero is a special case, because the width of the string does not
        // match its 10-log, so we treat this here first to avoid having to deal
        // with this case below.
        if self.numer == 0 {
            return "0.0".to_string();
        }

        let mut result = self.numer.to_string();

        // The maximum value of an i64 is 9223372036854775808, which has 19
        // digits. Let's say that numbers with up to 12 digits before or after
        // the decimal point are okay to print in normal notation, but when we
        // get beyond that, we switch to scientific notation.
        let sign_len = if self.numer < 0 { 1 } else { 0 };
        let n = result.len() as i16 - sign_len;
        let digits_before_point = (n + self.exponent).max(0);
        let digits_after_point = (-self.exponent).max(0);

        if digits_before_point > 12 || digits_after_point > 12 {
            return self.format_scientific(result);
        }

        if digits_before_point >= n {
            for _ in 0..digits_before_point - n {
                result.push('0');
            }
            result.push_str(".0");
            return result;
        }

        if digits_after_point >= n {
            let mut final_result = String::with_capacity(digits_after_point as usize + 2);
            if self.numer < 0 {
                final_result.push_str("-0.");
            } else {
                final_result.push_str("0.");
            }
            for _ in 0..digits_after_point - n {
                final_result.push('0');
            }
            final_result.push_str(&result[sign_len as usize..]);
            return final_result;
        }

        // If the exponent was positive, then we would have been in the case
        // where the digits before the decimal point were more than the length.
        debug_assert!(self.exponent < 0);

        // If the exponent was thas that negative, we would have been in the
        // case where the entire number was behind the point.
        debug_assert!(-self.exponent < n);

        // So we are in the case where the decimal point is inside the string.
        result.insert(result.len() - digits_after_point as usize, '.');

        result
    }

    /// Move all powers of 10 from the numerator into the exponent.
    ///
    /// This might fail when the new exponent does not fit in 16 bits.
    pub fn normalize(&self) -> Option<Decimal> {
        let mut normed = *self;
        while normed.numer % 10 == 0 {
            normed.numer /= 10;
            normed.exponent = normed.exponent.checked_add(1)?;
        }
        Some(normed)
    }

    pub fn checked_neg(&self) -> Option<Decimal> {
        let result = Decimal {
            numer: self.numer.checked_neg()?,
            exponent: self.exponent,
        };
        Some(result)
    }

    /// Convert to a float. For many decimals this will be a lossy operation.
    pub fn to_f64(&self) -> f64 {
        let n = self.numer as f64;
        let exp = 10.0_f64.powi(self.exponent as i32);
        n * exp
    }
}

impl PartialEq for Decimal {
    fn eq(&self, other: &Decimal) -> bool {
        // The case below assumes that on factor overflow the two are not equal,
        // but this is not the case when the numerator is 0, so we need to check
        // for that first.
        if self.numer == 0 && other.numer == 0 {
            return true;
        }

        match self.exponent as i32 - other.exponent as i32 {
            0 => self.numer == other.numer,
            n if n > 0 => {
                // `self` has excess exponent, try to move that excess into the
                // numerator so we can compare.
                let (factor, did_overflow) = 10_i64.overflowing_pow(n as u32);
                if did_overflow {
                    return false;
                }
                let self_numer = match self.numer.checked_mul(factor) {
                    Some(m) => m,
                    None => return false,
                };
                self_numer == other.numer
            }
            n => {
                // Same as above, but now `other` has the excess exponent.
                let (factor, did_overflow) = 10_i64.overflowing_pow((-n) as u32);
                if did_overflow {
                    return false;
                }
                let other_numer = match other.numer.checked_mul(factor) {
                    Some(m) => m,
                    None => return false,
                };
                self.numer == other_numer
            }
        }
    }
}

impl Eq for Decimal {}

impl Ord for Decimal {
    fn cmp(&self, other: &Decimal) -> Ordering {
        if self.numer == 0 && other.numer == 0 {
            return Ordering::Equal;
        }

        if self.exponent == other.exponent {
            return self.numer.cmp(&other.numer);
        }

        if self.exponent <= other.exponent {
            // This is a case like 0.1 cmp 1.0. Try to rescale ourselves to be
            // in the same range as the other. That loses precision, but we can
            // get a lower and upper bound of where we are and that might be
            // sufficient to complete the comparison.
            let factor = match 10_i64.checked_pow((other.exponent - self.exponent) as u32) {
                Some(n) => n,
                None => {
                    // This number is so much smaller than the other one that we
                    // can't even put them on the same scale.
                    return match (self.numer.signum(), other.numer.signum()) {
                        (_, 1) => Ordering::Less,
                        (_, -1) => Ordering::Greater,
                        (-1, 0) => Ordering::Less,
                        (1, 0) => Ordering::Greater,
                        _ => unreachable!("We handled (0, 0) before and signum returns no others."),
                    };
                }
            };

            // Scale ourselves down to the same range of the other. We lose
            // precision, so we have an inclusive lower bound and exclusive
            // upper bound of where `self` is on this scale compared to the
            // other.
            // TODO: This fails for negative numbers.
            let self_lower = self.numer / factor;
            let self_upper = self_lower + 1;

            if self_lower > other.numer {
                return Ordering::Greater;
            }
            if self_upper <= other.numer {
                return Ordering::Less;
            }

            debug_assert_eq!(self_lower, other.numer);

            // If the scaled version is equal, if the scaled version is exact
            // then the two numbers are equal. If the scaled version lost some
            // digits, then it means `self` is in between `self_lower` and
            // `self_upper` so it's greater than the other number.
            if self.numer == self_lower * factor {
                Ordering::Equal
            } else {
                Ordering::Greater
            }
        } else {
            // The case where other's exponent is larger is symmetric, we can
            // use the same code but just reverse the outcome.
            other.cmp(self).reverse()
        }
    }
}

impl PartialOrd for Decimal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/* TODO: Restore the Rational type when we need it.

/// A rational number.
#[derive(Copy, Clone, Debug)]
struct Rational {
    /// The numerator.
    numer: i64,
    /// The denominator, which should not contain factors of 10.
    denom: u64,
}

impl From<i64> for Rational {
    fn from(x: i64) -> Rational {
        Rational { numer: x, denom: 1 }
    }
}

impl Rational {
    pub fn add(&self, other: Rational) -> Option<Rational> {
        let result = if self.denom == other.denom {
            Rational {
                numer: self.numer.checked_add(other.numer)?,
                denom: self.denom,
            }
        } else {
            let denom = self.denom.checked_mul(other.denom)?;
            let n1 = self.numer.checked_mul(other.denom.try_into().ok()?)?;
            let n2 = other.numer.checked_mul(self.denom.try_into().ok()?)?;
            // TODO: Simplify the representation if possible.
            Rational {
                numer: n1.checked_add(n2)?,
                denom,
            }
        };
        Some(result)
    }

    pub fn sub(&self, other: Rational) -> Option<Rational> {
        let result = if self.denom == other.denom {
            Rational {
                numer: self.numer.checked_sub(other.numer)?,
                denom: self.denom,
            }
        } else {
            let denom = self.denom.checked_mul(other.denom)?;
            let n1 = self.numer.checked_mul(other.denom.try_into().ok()?)?;
            let n2 = other.numer.checked_mul(self.denom.try_into().ok()?)?;
            // TODO: Simplify the representation if possible.
            Rational {
                numer: n1.checked_sub(n2)?,
                denom,
            }
        };
        Some(result)
    }
}

*/

#[cfg(test)]
mod test {
    use super::{Decimal, ParseResult};

    fn assert_parse_decimal(num: &str, expected_numer: i64, expected_exponent: i16) {
        let result = Decimal::parse_str(num);
        let is_ok = match result {
            Some(ParseResult::Decimal(d)) => {
                // Note, we explicitly check the numerator and exponent here,
                // we don't want equivalence (mathematical equality) of the
                // decimals, we check the representation (structural equality).
                d.numer == expected_numer && d.exponent == expected_exponent
            }
            _ => false,
        };

        assert!(
            is_ok,
            "Expected '{num}' to parse as {expected_numer}e{expected_exponent}, but got {result:#?}"
        );
    }

    /// Shorthand to make tests briefer.
    ///
    /// But also less readable, we don't make this `Decimal::new`.
    fn decimal(numer: i64, exponent: i16) -> Decimal {
        Decimal { numer, exponent }
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
        assert_parse_decimal("0.0", 0, -1);
        assert_parse_decimal("0.", 0, 0);
        assert_parse_decimal("10.0", 100, -1);
        assert_parse_decimal("0.1", 1, -1);
    }

    #[test]
    fn decimal_parse_str_parses_decimal_imprecise() {
        // This magic number is the overflow point.
        assert_eq!(i64::MAX, 9223372036854775807);

        // Without the switch to imprecise parsing, we'd overflow the i64
        // mantissa, and we would not be able to parse this, but with the switch
        // we can handle arbitrarily long decimals (but not in full precision).
        // In the case below the overflow happens before the decimal point. We
        // also test that rounding works as expected.
        assert_parse_decimal("9223372036854775807.0", 9223372036854775807, 0);
        assert_parse_decimal("9223372036854775808.0", 922337203685477581, 1);
        assert_parse_decimal("9223372036854775814.0", 922337203685477581, 1);
        assert_parse_decimal("9223372036854775815.0", 922337203685477582, 1);

        // And here it happens after the decimal point.
        assert_parse_decimal("0.9223372036854775807", 9223372036854775807, -19);
        assert_parse_decimal("0.9223372036854775808", 922337203685477581, -18);
        assert_parse_decimal("0.9223372036854775814", 922337203685477581, -18);
        assert_parse_decimal("0.9223372036854775815", 922337203685477582, -18);
    }

    #[test]
    fn decimal_parse_str_parses_exponent() {
        assert_parse_decimal("0e0", 0, 0);
        assert_parse_decimal("1.0e42", 10, 41);
        assert_parse_decimal("1.0e+42", 10, 41);
        assert_parse_decimal("1.0e-42", 10, -43);
        assert_parse_decimal("0.00500e5", 500, 0);
        assert_parse_decimal("300e-3", 300, -3);
    }

    #[test]
    fn decimal_format_works() {
        let x = Decimal {
            numer: 0,
            exponent: 0,
        };
        assert_eq!(&x.format(), "0.0");

        let x = Decimal {
            numer: 1,
            exponent: 0,
        };
        assert_eq!(&x.format(), "1.0");

        let x = Decimal {
            numer: 1,
            exponent: 1,
        };
        assert_eq!(&x.format(), "10.0");

        let x = Decimal {
            numer: 1,
            exponent: -1,
        };
        assert_eq!(&x.format(), "0.1");

        // Note, the numerator matters for how many decimals we get!
        let x = Decimal {
            numer: 10,
            exponent: -2,
        };
        assert_eq!(&x.format(), "0.10");

        let x = Decimal {
            numer: 10,
            exponent: -1,
        };
        assert_eq!(&x.format(), "1.0");

        let x = Decimal {
            numer: 1,
            exponent: -6,
        };
        assert_eq!(&x.format(), "0.000001");

        let x = Decimal {
            numer: 1,
            exponent: 6,
        };
        assert_eq!(&x.format(), "1000000.0");
    }

    #[test]
    fn decimal_format_switches_to_scientific() {
        let x = Decimal {
            numer: 1,
            exponent: 11,
        };
        assert_eq!(&x.format(), "100000000000.0");

        let x = Decimal {
            numer: 1,
            exponent: 12,
        };
        assert_eq!(&x.format(), "1.0e12");

        let x = Decimal {
            numer: 1,
            exponent: -12,
        };
        assert_eq!(&x.format(), "0.000000000001");

        let x = Decimal {
            numer: 1,
            exponent: -13,
        };
        assert_eq!(&x.format(), "1.0e-13");
    }

    #[test]
    fn decimal_format_handles_negative_numbers() {
        assert_eq!(&decimal(-1, 2).format(), "-100.0");
        assert_eq!(&decimal(-1, -2).format(), "-0.01");
        assert_eq!(&decimal(-9000009, -14).format(), "-9.000009e-8");
        assert_eq!(&decimal(-7, 25).format(), "-7.0e25");
    }

    #[test]
    fn decimal_parse_after_format_is_identity() {
        // This test is only a quick verification; the same property will be
        // exercised more thoroughly by the fuzzer that verifies that evaluation
        // to json is idempotent. We don't test negative numerators here because
        // in the full parser the - is a unary operator, not part of the number.
        for numer in (0..999).step_by(7) {
            for exponent in (-999..999).step_by(11) {
                let d1 = Decimal { numer, exponent };
                let s = d1.format();
                match Decimal::parse_str(&s).unwrap() {
                    ParseResult::Int(..) => panic!("Formatting a decimal should parse as decimal."),
                    ParseResult::Decimal(d2) => {
                        assert_eq!(d1, d2);
                    }
                }
            }
        }
    }

    #[test]
    fn decimal_ord_works() {
        // TODO: Instead of "decimal" shorthand, write as strings and parse
        // them, that would make the tests way more readable.
        let p1e0 = decimal(1, 0);
        let p2e0 = decimal(2, 0);
        let p10e0 = decimal(10, 0);
        let p11e0 = decimal(11, 0);

        let p1e1 = decimal(1, 1);
        let p2e1 = decimal(2, 1);
        let p10e1 = decimal(10, 1);
        let p11e1 = decimal(11, 1);

        let p1en1 = decimal(1, -1);
        let p2en1 = decimal(2, -1);
        let p10en1 = decimal(10, -1);
        let p11en1 = decimal(11, -1);

        let n1e0 = decimal(-1, 0);
        let n2e0 = decimal(-2, 0);
        let n10e0 = decimal(-10, 0);
        let n11e0 = decimal(-11, 0);

        assert!(p1e0 < p2e0);
        assert!(p2e0 < p10e0);
        assert!(p10e0 < p11e0);

        assert!(p1e0 < p1e1);
        assert!(p2e0 < p1e1);
        assert!(p10e0 == p1e1);
        assert!(p11e0 > p1e1);
        assert!(p11e0 < p2e1);
        assert!(p11e0 < p10e1);

        assert!(p1en1 < p1e0);
        assert!(p2en1 < p1e0);
        assert!(p10en1 == p1e0);
        assert!(p10en1 < p1e1);
        assert!(p11en1 > p1e0);
        assert!(p11en1 < p2e0);

        assert!(p11e1 > p11en1);
        assert!(p11en1 < p11e1);

        assert!(n1e0 > n2e0);
        assert!(n2e0 > n10e0);
        assert!(n10e0 > n11e0);

        assert!(n1e0 < p1e0);
        assert!(n1e0 < p1e1);
        assert!(n11e0 < p1e1);
        assert!(n11e0 < p10e0);

        assert!(n1e0 < p1en1);
        assert!(n1e0 < p10en1);

        let p1e100 = decimal(1, 100);
        let n1e100 = decimal(-1, 100);
        assert!(p1e100 > p1e1);
        assert!(p1e100 > n1e0);
        assert!(n1e100 < p1e100);
        assert!(n1e100 < p1e1);
        assert!(n1e100 < n1e0);
    }
}