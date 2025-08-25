// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The "über fuzzer" checks many invariants from one corpus of RCL programs.

#![no_main]

use arbitrary::{Arbitrary, Error, Unstructured};
use libfuzzer_sys::fuzz_target;

use rcl_fuzz::uber::{fuzz_main, Mode};

// Two helpers functions to make the code below a bit more concise.
fn memchr(haystack: &[u8], needle: u8) -> arbitrary::Result<usize> {
    haystack
        .iter()
        .position(|b| *b == needle)
        .ok_or(Error::IncorrectFormat)
}

fn utf8(buf: &[u8]) -> arbitrary::Result<String> {
    std::str::from_utf8(buf)
        .map_err(|_| Error::IncorrectFormat)
        .map(|x| x.to_owned())
}

/// Helper for `Arbitrary` to get a value in 0..=245, such that the byte is not a newline.
struct NonNewline(u8);

impl Arbitrary<'_> for NonNewline {
    fn arbitrary(u: &mut Unstructured) -> arbitrary::Result<NonNewline> {
        match u.bytes(1)? {
            [n] if *n > b'\n' => Ok(NonNewline(n - b'\n')),
            _ => Err(Error::IncorrectFormat),
        }
    }
}

#[derive(Debug)]
struct Input<'a> {
    mode: Mode,
    data: &'a str,
}

impl<'a> Arbitrary<'a> for Input<'a> {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Input<'a>> {
        // The fuzz inputs must start with a comment line. This should ensure
        // that we can evaluate the fuzz input as a regular file as well,
        // although in some cases the leading comment may cause a divergence.
        // In that case it's still easy to pipe through `tail -n +2`.
        if u.bytes(2)? != b"//" {
            return Err(Error::IncorrectFormat);
        }

        // The modes are characters on purpose, so the resulting fuzz input
        // remains human-readable. In the past we also had simpler fuzz modes,
        // the idea being that the fuzzer can explore faster if it doesn't have
        // to evaluate the full pipeline every time. But I'm not sure that was
        // working well, because it also amplifies the corpus with essentially
        // duplicates. So now we only have the essential checks. We
        // unconditionally have the width byte even for modes that don't use it,
        // to make samples more portable between modes.
        let mode_byte = u.bytes(1);
        let width = u.arbitrary::<NonNewline>()?.0 as u32;
        let mode = match mode_byte? {
            b"a" => Mode::Eval,
            b"j" => Mode::EvalJsonIdempotent { width },
            b"k" => Mode::EvalJsonCheck { width },
            b"t" => Mode::EvalTomlCheck { width },
            b"f" => Mode::EvalFormat { width },
            b"s" => Mode::EvalJsonSuperset,
            b"l" => Mode::EvalJsonLines,
            b"p" => {
                // For the patch mode, we need two additional inputs. We stuff
                // those on the same metadata line, with a space in between.
                // There are no length prefixes, and the only requirement is a
                // space, so this is relatively easy for the fuzzer to discover.
                // For aesthetic reasons, we also demand a space between the meta
                // bytes in the comment, and the path.
                let buf = u.peek_bytes(u.len()).expect("We peek the len, it fits.");
                if buf.first() != Some(&b' ') {
                    return Err(Error::IncorrectFormat);
                }
                let i = 1 + memchr(&buf[1..], b' ')?;
                let j = i + memchr(&buf[i..], b'\n')?;
                u.bytes(j).expect("We peeked before, it fits.");
                Mode::PatchIdempotent {
                    width,
                    path: utf8(&buf[1..i])?,
                    replacement: utf8(&buf[i..j])?,
                }
            }
            _ => return Err(Error::IncorrectFormat),
        };

        if u.bytes(1)? != b"\n" {
            return Err(Error::IncorrectFormat);
        }

        let data = match std::str::from_utf8(u.bytes(u.len())?) {
            Ok(s) => s,
            Err(_) => return Err(Error::IncorrectFormat),
        };

        let result = Input { mode, data };
        Ok(result)
    }
}

fuzz_target!(|input: Input| {
    fuzz_main(input.mode, input.data);
});
