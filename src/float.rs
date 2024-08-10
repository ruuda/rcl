// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Implementation of decimal and rational numbers for use in the interpreter.

/// A rational number of the form `a × 10^n`.
#[derive(Copy, Clone, Debug)]
struct Decimal {
    /// The value `a` (the "numerator").
    numer: i64,
    /// The exponent `n`.
    exponent: i16,
}

impl From<i64> for Decimal {
    fn from(x: i64) -> Decimal {
        Decimal {
            numer: x,
            exponent: 0,
        }
    }
}

impl Decimal {
    /// Format the number in scientific notation with exponent.
    ///
    /// Expects the numerator to be already converted to string.
    fn format_scientific(&self, numer_str: String) -> String {
        let mut result = numer_str;

        // We want to use the width of the string to determine the exponent,
        // but that fails for 0, so handle this special case now to simplify
        // the remaining code. We might return 0e0 but it would be dead code
        // as long as this method is not exposed to users, so for now panic.
        debug_assert_ne!(self.numer, 0, "format_scientific is not meant to format 0.");

        // We are going to put one digit before the decimal points and the
        // others after it, so we need to adjust the exponent for that.
        let implicit_exponent = result.len() as i16 - 1;
        result.insert(1, '.');

        // Ensure there is always something after the decimal point, even when
        // there are no significant digits there.
        if result.len() == 2 {
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
        let digits_before_point = (result.len() as i16 + self.exponent).max(0);
        let digits_after_point = (-self.exponent).max(0);

        if digits_before_point > 12 || digits_after_point > 12 {
            return self.format_scientific(result);
        }

        if digits_before_point >= result.len() as i16 {
            for _ in 0..digits_before_point - result.len() as i16 {
                result.push('0');
            }
            result.push_str(".0");
            return result;
        }

        if digits_after_point >= result.len() as i16 {
            let mut final_result = String::with_capacity(digits_after_point as usize + 2);
            final_result.push_str("0.");
            for _ in 0..digits_after_point - result.len() as i16 {
                final_result.push('0');
            }
            final_result.push_str(&result);
            return final_result;
        }

        // If the exponent was positive, then we would have been in the case
        // where the digits before the decimal point were more than the length.
        debug_assert!(self.exponent < 0);

        // If the exponent was thas that negative, we would have been in the
        // case where the entire number was behind the point.
        debug_assert!(-self.exponent < result.len() as i16);

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
}

impl PartialEq for Decimal {
    fn eq(&self, other: &Decimal) -> bool {
        match self.exponent as i32 - other.exponent as i32 {
            0 => self.numer == other.numer,
            n if n > 0 => {
                // TODO: Normalize, then compare.
                panic!("TODO: Normalize, then compare.");
            }
            _ => {
                panic!("TODO: Normalize, then compare.");
            }
        }
    }
}

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

#[cfg(test)]
mod test {
    use super::Decimal;

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
}
