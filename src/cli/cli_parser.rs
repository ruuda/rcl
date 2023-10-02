// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Functions to parse the command line.

use std::str::FromStr;

use crate::cli::arg_iter::{Arg, ArgIter};
use crate::cli::command::{
    Cmd, FormatOptions, FormatTarget, GlobalOptions, OutputFormat, OutputOptions, Target,
};
use crate::error::{Error, Result};
use crate::markup::{Markup, MarkupMode};
use crate::pprint::{concat, Doc};

const USAGE_MAIN: &str = r#"
RCL -- A sane configuration language.

Usage:
  rcl [<options>] <command> <arguments>

Commands:
  evaluate     Evaluate a document to an output format.
  format       Auto-format an RCL document.
  highlight    Print a document with syntax highlighting.
  query        Evaluate an expression against an input document.

Command shorthands:
  e       Alias for 'evaluate'.
  eval    Alias for 'evaluate'.
  f       Alias for 'format'.
  fmt     Alias for 'format'.
  h       Alias for 'highlight'.
  qj      Alias for 'query --output=json'.
  q       Alias for 'query'.

Global options:
  --color <mode>  How and whether to add markup and color. Can be one of:
                    - 'ansi': Always color output using ANSI escape codes.
                    - 'auto': Use ANSI if the output file is a TTY and the
                      NO_COLOR environment variable is not set to a non-empty
                      string. This is the default.
                    - 'none': Do not color output at all.
  -h --help       Show this screen.
  --version       Show version.
"#;

const USAGE_EVAL_QUERY: &str = r#"
RCL -- A sane configuration language.

Usage:
  rcl [<options>] evaluate [<options>] <file>
  rcl [<options>] query    [<options>] <file> <query>

The 'evaluate' command evaluates the expression in the input file and prints it
to stdout. The 'query' command additionally evaluates an expression against the
result.

Arguments:
  <file>                The input file to process, or '-' for stdin.
  <query>               An RCL expression to evaluate. The result of evaluating
                        the input file is bound to the variable 'input'.

Options:
  -o --output <format>  Output format, can be one of 'json', 'rcl'.
                        Defaults to 'rcl'.
  --width <width>       Target width for pretty-printing, must be an integer.
                        Defaults to 80.

See also --help for global options.
"#;

const USAGE_FORMAT: &str = r#"
RCL -- A sane configuration language.

Usage:
  rcl [<options>] format [<options>] <file>...

The 'format' command formats one or more input documents in standard style.

Arguments:
  <file>...        The input files to process, or '-' for stdin. When --in-place
                   is used, there can be multiple input files.

Options:
  -i --in-place    Rewrite files in-place instead of writing to stdout.
                   By default the formatted result is written to stdout.
  --width <width>  Target width in number of columns, must be an integer.
                   Defaults to 80.

See also --help for global options.
"#;

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

macro_rules! parse_option {
    {
        $args_iter:ident: $option:expr, $parser:expr
    } => {
        match $args_iter.next() {
            Some(Arg::Plain(value)) => match $parser(&value[..]) {
                Ok(r) => r,
                Err(..) => {
                    let err = concat! {
                        "'"
                        Doc::from(value).with_markup(Markup::Highlight)
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

pub fn parse(args: Vec<String>) -> Result<(GlobalOptions, Cmd)> {
    let mut args = ArgIter::new(args);

    // Skip over the program name.
    args.next();

    let mut cmd: Option<&'static str> = None;
    let mut cmd_help: Option<&'static str> = None;
    let mut format_opts = FormatOptions::default();
    let mut global_opts = GlobalOptions::default();
    let mut output_opts = OutputOptions::default();
    let mut in_place = false;
    let mut is_version = false;
    let mut targets: Vec<Target> = Vec::new();

    while let Some(arg) = args.next() {
        match arg.as_ref() {
            Arg::Long("color") => {
                global_opts.markup = match_option! {
                    args: arg,
                    "auto" => None,
                    "ansi" => Some(MarkupMode::Ansi),
                    "none" => Some(MarkupMode::None),
                }
            }
            Arg::Long("output") | Arg::Short("o") => {
                output_opts.format = match_option! {
                    args: arg,
                    "json" => OutputFormat::Json,
                    "rcl" => OutputFormat::Rcl,
                }
            }
            Arg::Long("width") | Arg::Short("w") => {
                format_opts.width = Some(parse_option! { args: arg, u32::from_str });
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
                output_opts.format = OutputFormat::Json;
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
            Arg::Plain(..) => {
                let err = concat! {
                    "Unknown command '"
                    Doc::from(arg.to_string()).with_markup(Markup::Highlight)
                    "'. See --help for usage."
                };
                return Error::new(err).err();
            }
            _ => {
                let err = concat! {
                    "Unknown option '"
                    Doc::from(arg.to_string()).with_markup(Markup::Highlight)
                    "'. See --help for usage."
                };
                return Error::new(err).err();
            }
        }
    }

    if is_version {
        return Ok((global_opts, Cmd::Version));
    }

    let help_opt = match cmd_help {
        Some("evaluate") => Some(Cmd::Help {
            usage: USAGE_EVAL_QUERY,
        }),
        Some("format") => Some(Cmd::Help {
            usage: USAGE_FORMAT,
        }),
        // TODO: Add usage for highlight.
        Some("highlight") => Some(Cmd::Help { usage: USAGE_MAIN }),
        Some("main") => Some(Cmd::Help { usage: USAGE_MAIN }),
        Some("query") => Some(Cmd::Help {
            usage: USAGE_EVAL_QUERY,
        }),
        _ => None,
    };
    if let Some(help) = help_opt {
        return Ok((global_opts, help));
    }

    let result = match cmd {
        Some("evaluate") => Cmd::Evaluate {
            output_opts,
            format_opts,
            fname: get_unique_target(targets)?,
        },
        Some("query") => {
            let (fname, query) = match targets.len() {
                2 => (
                    targets.remove(0),
                    // Not a file, but a consequence of the CLI parsing that
                    // handles - being stdin/stdout.
                    match targets.remove(0) {
                        Target::File(query) => query,
                        Target::Stdin => "-".to_string(),
                    },
                ),
                _ => {
                    return Error::new(
                        "Expected an input file and a query. \
                        See --help for usage.",
                    )
                    .err()
                }
            };
            Cmd::Query {
                output_opts,
                format_opts,
                query,
                fname,
            }
        }
        Some("format") => Cmd::Format {
            format_opts,
            target: if in_place {
                FormatTarget::InPlace { fnames: targets }
            } else {
                FormatTarget::Stdout {
                    fname: get_unique_target(targets)?,
                }
            },
        },
        Some("highlight") => Cmd::Highlight {
            fname: get_unique_target(targets)?,
        },
        None => Cmd::Help { usage: USAGE_MAIN },
        _ => panic!("Should have returned an error before getting here."),
    };
    Ok((global_opts, result))
}

pub fn get_unique_target(mut targets: Vec<Target>) -> Result<Target> {
    match targets.pop() {
        None => Error::new("Expected an input file. See --help for usage.").err(),
        Some(_) if !targets.is_empty() => {
            Error::new("Too many input files. See --help for usage.").err()
        }
        Some(t) => Ok(t),
    }
}
