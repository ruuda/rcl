// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Functions to parse the command line.

use std::str::FromStr;

use crate::cli::arg_iter::Arg;
use crate::cli::command::{FormatOptions, GlobalOptions, OutputFormat, OutputOptions};
use crate::error::{Error, Result};
use crate::markup::{Markup, MarkupMode};
use crate::pprint::Doc;

/// Helper macro to match option values and assign them to a field on match.
///
/// This also takes care of generating the errors for non-match cases.
macro_rules! parse_matches {
    {
        name = $opt_name:expr,
        args_iter = $args_iter:expr,
        $out:ident.$field:ident = arg { $( $pat:literal => $val:expr , )+ }
    } => {{
        let mut err = vec![
            "Expected ".into(),
            Doc::from($opt_name).with_markup(Markup::Highlight),
            " to be followed by one of ".into(),
        ];
        $(
            err.push(Doc::from($pat).with_markup(Markup::Highlight));
            err.push(", ".into());
        )+
        err.pop();
        err.push(".".into());
        let err = Doc::Concat(err);

        match $args_iter.next() {
            Some(arg) => match arg.as_ref() {
                $(
                    Arg::Plain($pat) => $out.$field = $val,
                )+
                _ => return Error::new(err).err(),
            }
            None => return Error::new(err).err(),
        }
    }};
    {
        name = $opt_name:expr,
        args_iter = $args_iter:expr,
        $out:ident.$field:ident = try $parser:expr
    } => {
        match $args_iter.next() {
            Some(Arg::Plain(value)) => match $parser(&value[..]) {
                Ok(r) => $out.$field = r,
                Err(..) => {
                    let err = vec![
                        "Invalid a value for ".into(),
                        Doc::from($opt_name).with_markup(Markup::Highlight),
                        ": ".into(),
                        Doc::from(value).with_markup(Markup::Highlight),
                        ".".into(),
                    ];
                    return Error::new(Doc::Concat(err)).err();
                }
            }
            _ => {
                let err = vec![
                    "Expected a value for ".into(),
                    Doc::from($opt_name).with_markup(Markup::Highlight),
                    ".".into(),
                ];
                return Error::new(Doc::Concat(err)).err();
            }
        }
    };
}

/// Generate boilerplate for parsing CLI options into a struct.
macro_rules! parse_struct {
    { $( $struct_:ty { $( $pat:pat => $field:ident = $op:ident $matches:tt , )+ } )+ } => { $(
        impl $struct_ {
            #[allow(clippy::redundant_closure_call)]
            pub fn parse(args: &mut Vec<Arg<String>>) -> Result<Self> {
                let mut to_consume = Vec::with_capacity(args.len());
                std::mem::swap(args, &mut to_consume);
                let mut args_iter = to_consume.into_iter();
                let mut result = <$struct_ as Default>::default();

                while let Some(arg) = args_iter.next() {
                    match arg.as_ref() {
                        $(
                            $pat => parse_matches! {
                                name = arg.to_string(),
                                args_iter = args_iter,
                                result.$field = $op $matches
                            },
                        ),+
                        _ => args.push(arg),
                    }
                }

                Ok(result)
            }
        }
    )+}
}

pub fn parse_global_options(args: &mut Vec<Arg<String>>) -> Result<GlobalOptions> {
    let mut to_consume = Vec::with_capacity(args.len());
    std::mem::swap(args, &mut to_consume);
    let mut args_iter = to_consume.into_iter();

    let mut result = GlobalOptions { markup: None };

    while let Some(arg) = args_iter.next() {
        match arg.as_ref() {
            Arg::Long("color") => match args_iter.next() {
                Some(arg) => match arg.as_ref() {
                    Arg::Plain("auto") => result.markup = None,
                    Arg::Plain("ansi") => result.markup = Some(MarkupMode::Ansi),
                    Arg::Plain("none") => result.markup = Some(MarkupMode::None),
                    _ => panic!("TODO: Error."),
                },
                None => panic!("TODO: Error."),
            },
            _ => args.push(arg),
        }
    }

    Ok(result)
}

parse_struct! {
    GlobalOptions {
        Arg::Long("color") => markup = arg {
            "auto" => None,
            "ansi" => Some(MarkupMode::Ansi),
            "none" => Some(MarkupMode::None),
        },
    }
    OutputOptions {
        Arg::Long("output") | Arg::Short("o") => format = arg {
            "json" => Some(OutputFormat::Json),
            "rcl" => Some(OutputFormat::Rcl),
        },
    }
    FormatOptions {
        Arg::Long("width") | Arg::Short("w") => width = try {
            |x| u32::from_str(x).map(Some)
        },
    }
}
