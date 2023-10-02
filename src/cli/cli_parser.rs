// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Functions to parse the command line.

use std::str::FromStr;

use crate::cli::arg_iter::{Arg, ArgIter};
use crate::cli::command::{Cmd, FormatOptions, GlobalOptions, OutputFormat, OutputOptions, Target};
use crate::error::{Error, Result};
use crate::markup::{Markup, MarkupMode};
use crate::pprint::{concat, Doc};

/// Helper macro to match option values and assign them to a field on match.
///
/// This also takes care of generating the errors for non-match cases.
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
        err.push(".".into());
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

macro_rules! parse_option {
    {
        $args_iter:ident: $option:expr, $parser:expr
    } => {
        match $args_iter.next() {
            Some(Arg::Plain(value)) => match $parser(&value[..]) {
                Ok(r) => r,
                Err(..) => {
                    let err = concat! {
                        "Invalid a value for "
                        Doc::from($option.to_string()).with_markup(Markup::Highlight)
                        ": "
                        Doc::from(value).with_markup(Markup::Error)
                        "."
                    };
                    return Error::new(err).err();
                }
            }
            _ => {
                let err = concat! {
                    "Expected a value for "
                    Doc::from($option.to_string()).with_markup(Markup::Highlight)
                    "."
                };
                return Error::new(err).err();
            }
        }
    };
}

fn parse(mut args: ArgIter) -> Result<Cmd> {
    let mut targets: Vec<Target> = Vec::new();
    let mut global_options = GlobalOptions::default();
    let mut output_options = OutputOptions::default();
    let mut format_options = FormatOptions::default();
    let mut in_place = false;
    let mut is_version = false;
    let mut cmd_help: Option<&'static str> = None;
    let mut cmd: Option<&'static str> = None;
    while let Some(arg) = args.next() {
        match arg.as_ref() {
            Arg::Long("color") => {
                global_options.markup = match_option! {
                    args: arg,
                    "auto" => None,
                    "ansi" => Some(MarkupMode::Ansi),
                    "none" => Some(MarkupMode::None),
                }
            }
            Arg::Long("output") | Arg::Short("o") => {
                output_options.format = match_option! {
                    args: arg,
                    "json" => Some(OutputFormat::Json),
                    "rcl" => Some(OutputFormat::Rcl),
                }
            }
            Arg::Long("width") | Arg::Short("w") => {
                format_options.width = Some(parse_option! { args: arg, u32::from_str });
            }
            Arg::Long("in-place") | Arg::Short("i") => {
                in_place = true;
            }
            Arg::Long("help") | Arg::Short("h") => {
                is_version = false;
                cmd_help = match cmd {
                    Some(c) => Some(c),
                    None => Some("main"),
                };
            }
            Arg::Long("version") => {
                is_version = true;
                cmd_help = None;
            }
            Arg::Plain("evaluate") | Arg::Plain("eval") | Arg::Plain("e") if cmd.is_none() => {
                cmd = Some("evaluate");
            }
            Arg::Plain("query") | Arg::Plain("q") if cmd.is_none() => {
                cmd = Some("query");
            }
            Arg::Plain("qj") if cmd.is_none() => {
                cmd = Some("query");
                output_options.format = Some(OutputFormat::Json);
            }
            Arg::Plain("format") | Arg::Plain("fmt") | Arg::Plain("f") if cmd.is_none() => {
                cmd = Some("format");
            }
            Arg::Plain("highlight") | Arg::Plain("h") if cmd.is_none() => {
                cmd = Some("highlight");
            }
            Arg::Plain(fname) if cmd.is_some() => {
                targets.push(Target::File(fname.to_string()));
            }
            Arg::StdInOut if cmd.is_some() => {
                targets.push(Target::Stdin);
            }
            Arg::Plain(fname) => {
                let err = concat! {
                    "Unknown command '"
                    Doc::from(arg.to_string()).with_markup(Markup::Highlight)
                    "'."
                };
                return Error::new(err).err();
            }
            _ => {
                let err = concat! {
                    "Unknown option '"
                    Doc::from(arg.to_string()).with_markup(Markup::Highlight)
                    "'."
                };
                return Error::new(err).err();
            }
        }
    }
    Ok(Cmd::Version)
}
