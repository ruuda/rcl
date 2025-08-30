// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

// This CLI parser is adapted from the one in Squiller [1], which is in turn
// adapted from the one in Tako [2] which is copyrighted by Arian van Putten,
// Ruud van Asseldonk, and Tako Marks, and licensed Apache2.
// [1] https://github.com/ruuda/squiller
// [2] https://github.com/ruuda/tako

//! Utilities to aid parsing the command line.

use std::fmt;
use std::vec;

pub enum Arg<T> {
    Plain(T),
    Short(T),
    Long(T),
    /// An argument `-` that occurred before a bare `--`.
    StdInOut,
}

impl Arg<String> {
    pub fn as_ref(&self) -> Arg<&str> {
        match *self {
            Arg::Plain(ref x) => Arg::Plain(&x[..]),
            Arg::Short(ref x) => Arg::Short(&x[..]),
            Arg::Long(ref x) => Arg::Long(&x[..]),
            Arg::StdInOut => Arg::StdInOut,
        }
    }
}

impl fmt::Display for Arg<String> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Arg::Plain(ref x) => write!(f, "{}", x),
            Arg::Short(ref x) => write!(f, "-{}", x),
            Arg::Long(ref x) => write!(f, "--{}", x),
            Arg::StdInOut => write!(f, "-"),
        }
    }
}

pub struct ArgIter {
    /// Underlying args iterator.
    args: vec::IntoIter<String>,

    /// Whether we have observed a `--` argument.
    is_raw: bool,

    /// Leftover to return after an `--foo=bar` or `-fbar`-style argument.
    ///
    /// `--foo=bar` is returned as `Long(foo)` followed by `Plain(bar)`.
    /// `-fbar` is returned as `Short(f)` followed by `Plain(bar)`.
    leftover: Option<String>,
}

impl ArgIter {
    pub fn new(args: Vec<String>) -> ArgIter {
        ArgIter {
            args: args.into_iter(),
            is_raw: false,
            leftover: None,
        }
    }
}

impl Iterator for ArgIter {
    type Item = Arg<String>;

    fn next(&mut self) -> Option<Arg<String>> {
        if self.leftover.is_some() {
            return self.leftover.take().map(Arg::Plain);
        }

        let arg = self.args.next()?;

        if self.is_raw {
            return Some(Arg::Plain(arg));
        }

        if &arg == "--" {
            self.is_raw = true;
            return self.next();
        }

        if let Some(flag_slice) = arg.strip_prefix("--") {
            let mut flag = String::from(flag_slice);
            if let Some(i) = flag.find('=') {
                self.leftover = Some(flag.split_off(i + 1));
                flag.truncate(i);
            }
            return Some(Arg::Long(flag));
        }

        if arg == "-" {
            return Some(Arg::StdInOut);
        }

        if let Some(flag_slice) = arg.strip_prefix('-') {
            let mut flag = String::from(flag_slice);
            match flag.char_indices().nth(1) {
                Some((n, _)) if n < flag.len() => {
                    self.leftover = Some(flag.split_off(n));
                    flag.truncate(n);
                }
                _ => {}
            }
            return Some(Arg::Short(flag));
        }

        Some(Arg::Plain(arg))
    }
}

/// Helper macro to match option values and print help when there is no match.
macro_rules! match_option {
    {
        $args_iter:ident: $option:expr,
        $( $pat:literal => $val:expr , )+
    } => {{
        let mut err = vec![
            "Expected ".into(),
            Doc::from($option.to_string()).with_markup(Markup::Highlight),
            " to be followed by one of ".into(),
        ];
        $(
            err.push(Doc::from($pat).with_markup(Markup::Highlight));
            err.push(", ".into());
        )+
        err.pop();
        err.push(". See --help for usage.".into());
        let err = Doc::Concat(err);

        match $args_iter.next() {
            Some(arg) => match arg.as_ref() {
                $( Arg::Plain($pat) => $val, )+
                _ => return Error::new(err).err(),
            }
            None => return Error::new(err).err(),
        }
    }};
}
pub(crate) use match_option;

macro_rules! parse_option {
    {
        $args_iter:ident: $option:expr, $parser:expr
    } => {
        match $args_iter.next() {
            // Clippy is wrong here; probably it doesn't know that due to the
            // macro, the construction and call site of the closure are far away.
            #[allow(clippy::redundant_closure_call)]
            Some(Arg::Plain(value)) => match $parser(&value[..]) {
                Ok(r) => r,
                Err(..) => {
                    let err = concat! {
                        "'"
                        Doc::highlight(&value).into_owned()
                        "' is not valid for "
                        Doc::from($option.to_string()).with_markup(Markup::Highlight)
                        ". See --help for usage."
                    };
                    return Error::new(err).err();
                }
            }
            _ => {
                let err = concat! {
                    "Expected a value after "
                    Doc::from($option.to_string()).with_markup(Markup::Highlight)
                    ". See --help for usage."
                };
                return Error::new(err).err();
            }
        }
    };
}
pub(crate) use parse_option;
