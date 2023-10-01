// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use crate::cli_parser::{Arg, ArgIter};
use crate::error::{Error, Result};
use crate::markup::Markup;
use crate::markup::MarkupMode;
use crate::pprint::{concat, Doc};

const USAGE: &str = r#"
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
  jq      Alias for 'query --output=json'.
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

/// The available output formats.
#[derive(Debug, Eq, PartialEq)]
pub enum Output {
    Json,
    Rcl,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Cmd {
    Evaluate {
        color: Option<MarkupMode>,
        width: Option<u32>,
        output: Output,
        fname: String,
    },
    Query {
        color: Option<MarkupMode>,
        width: Option<u32>,
        output: Output,
        fname: String,
        query: String,
    },
    Format {
        color: Option<MarkupMode>,
        width: Option<u32>,
        in_place: bool,
        fnames: Vec<String>,
    },
    Highlight {
        color: Option<MarkupMode>,
        fname: String,
    },
    Help {
        usage: &'static str,
    },
    Version,
}

pub fn parse(argv: Vec<String>) -> Result<Cmd> {
    let mut args = ArgIter::new(argv);

    // Skip executable name.
    args.next();
    let mut plain_args = Vec::new();
    let mut is_help = false;
    let mut is_version = false;
    let mut color = None;
    let mut in_place = false;
    let mut output = None;
    let mut width = None;

    while let Some(arg) = args.next() {
        match arg.as_ref() {
            Arg::Plain(..) => plain_args.push(arg.into_string()),
            Arg::Long("color") => match args.next() {
                Some(Arg::Plain(v)) if v == "ansi" => color = Some(MarkupMode::Ansi),
                Some(Arg::Plain(v)) if v == "auto" => color = None,
                Some(Arg::Plain(v)) if v == "none" => color = Some(MarkupMode::None),
                Some(Arg::Plain(invalid)) => {
                    let err = concat! {
                        "Invalid value for --color: '"
                        Doc::from(invalid).with_markup(Markup::Highlight)
                        "'. Possible values are 'ansi', 'auto', and 'none'."
                    };
                    return Error::new(err).err();
                }
                _ => return Error::new("Expected color mode after --color.").err(),
            },
            Arg::Short("o") | Arg::Long("output") => match args.next() {
                Some(Arg::Plain(v)) if v == "rcl" => output = Some(Output::Rcl),
                Some(Arg::Plain(v)) if v == "json" => output = Some(Output::Json),
                Some(Arg::Plain(invalid)) => {
                    let err = concat! {
                        "Invalid value for --output: '"
                        Doc::from(invalid).with_markup(Markup::Highlight)
                        "'. Possible formats are 'json' and 'rcl'."
                    };
                    return Error::new(err).err();
                }
                _ => return Error::new("Expected output format after --output.").err(),
            },
            Arg::Long("width") => match args.next() {
                Some(Arg::Plain(w_str)) => {
                    use std::str::FromStr;
                    match u32::from_str(&w_str) {
                        Ok(w) => width = Some(w),
                        Err(..) => {
                            let err = concat! {
                            "Invalid value for --width: '"
                            Doc::from(w_str).with_markup(Markup::Highlight)
                            "'. Must be an integer."
                                };
                            return Error::new(err).err();
                        }
                    }
                }
                _ => return Error::new("Expected width after --width.").err(),
            },
            Arg::Long("in-place") => in_place = true,
            Arg::Long("version") => {
                is_help = false;
                is_version = true;
            }
            Arg::Short("h") | Arg::Long("help") => {
                is_version = false;
                is_help = true;
            }
            _invalid => {
                let err = concat! {
                    "Unknown option: '"
                    Doc::from(arg.into_string()).with_markup(Markup::Highlight)
                    "'. See --help for usage."
                };
                return Error::new(err).err();
            }
        }
    }

    if is_version {
        return Ok(Cmd::Version);
    }

    let mut arg_iter = plain_args.into_iter();
    match arg_iter.next() {
        Some(cmd) if cmd == "evaluate" || cmd == "eval" || cmd == "e" => {
            if is_help {
                return Ok(Cmd::Help {
                    usage: USAGE_EVAL_QUERY,
                });
            };
            let fname = match arg_iter.next() {
                Some(fname) => fname,
                None => panic!("TODO: Write error."),
            };
            if arg_iter.next().is_some() {
                return Error::new("Did not expect additional argument for 'evaluate' command.")
                    .err();
            }
            let cmd = Cmd::Evaluate {
                color,
                width,
                fname,
                output: output.unwrap_or(Output::Rcl),
            };
            Ok(cmd)
        }
        Some(cmd) if cmd == "query" || cmd == "q" || cmd == "jq" => {
            if is_help {
                return Ok(Cmd::Help {
                    usage: USAGE_EVAL_QUERY.trim(),
                });
            };
            let fname = match arg_iter.next() {
                Some(fname) => fname,
                None => panic!("TODO: Write error."),
            };
            let query = match arg_iter.next() {
                Some(query) => query,
                None => panic!("TODO: Write error."),
            };
            if arg_iter.next().is_some() {
                return Error::new("Did not expect additional argument for 'query' command.").err();
            }
            let output = match output {
                None if cmd == "jq" => Output::Json,
                None => Output::Rcl,
                Some(_) if cmd == "jq" => {
                    return Error::new(
                        "Option --output is incompatible with 'jq' command \
                        because 'jq' implies --output=json. Use the 'q' command \
                        instead.",
                    )
                    .err();
                }
                Some(o) => o,
            };
            let cmd = Cmd::Query {
                color,
                width,
                output,
                fname,
                query,
            };
            Ok(cmd)
        }
        Some(cmd) if cmd == "format" || cmd == "fmt" || cmd == "f" => {
            if is_help {
                return Ok(Cmd::Help {
                    usage: USAGE_FORMAT.trim(),
                });
            };

            let fnames: Vec<String> = arg_iter.collect();
            if fnames.is_empty() {
                return Error::new("Expected at least one input file to format.").err();
            }
            if fnames.len() > 1 && !in_place {
                return Error::new(
                    "Multiple input files are only supported when --in-place is enabled.",
                )
                .err();
            }

            let cmd = Cmd::Format {
                color,
                width,
                in_place,
                fnames,
            };
            Ok(cmd)
        }
        Some(cmd) if cmd == "highlight" => {
            if is_help {
                todo!("Write usage for highlight command.");
            }
            let fname = match arg_iter.next() {
                Some(fname) => fname,
                None => panic!("TODO: Write error."),
            };
            if arg_iter.next().is_some() {
                return Error::new("Did not expect additional argument for 'highlight' command.")
                    .err();
            }
            let cmd = Cmd::Highlight { color, fname };
            Ok(cmd)
        }
        Some(invalid) => {
            let err = concat! {
                "Unknown subcommand: '"
                Doc::from(invalid).with_markup(Markup::Highlight)
                "'. See --help for usage."
            };
            Error::new(err).err()
        }
        None => Ok(Cmd::Help {
            usage: USAGE.trim(),
        }),
    }
}

#[cfg(test)]
mod test {
    use super::{parse, Cmd, Output};
    use crate::error::Result;

    fn parse_slice_result(args: &[&'static str]) -> Result<Cmd> {
        let argv = args.iter().map(|&s| s.into()).collect();
        parse(argv)
    }

    fn parse_slice(args: &[&'static str]) -> Cmd {
        parse_slice_result(args).unwrap()
    }

    #[test]
    fn parse_parses_help() {
        let expected = Cmd::Help {
            usage: super::USAGE.trim(),
        };
        assert_eq!(parse_slice(&["rcl", "-h"]), expected);
        assert_eq!(parse_slice(&["rcl", "--help"]), expected);
        assert_eq!(parse_slice(&["rcl", "--version", "--help"]), expected);
    }

    #[test]
    fn parse_parses_version() {
        let expected = Cmd::Version;
        assert_eq!(parse_slice(&["rcl", "--version"]), expected);
        assert_eq!(parse_slice(&["rcl", "--help", "--version"]), expected);
    }

    #[test]
    fn parse_parses_eval() {
        let expected = Cmd::Evaluate {
            color: None,
            width: None,
            output: Output::Rcl,
            fname: "f".to_string(),
        };
        assert_eq!(parse_slice(&["rcl", "evaluate", "f"]), expected);
        assert_eq!(parse_slice(&["rcl", "eval", "f"]), expected);
        assert_eq!(parse_slice(&["rcl", "e", "f"]), expected);

        let expected = Cmd::Evaluate {
            color: None,
            width: None,
            output: Output::Json,
            fname: "f".to_string(),
        };

        assert_eq!(
            parse_slice(&["rcl", "evaluate", "--output", "json", "f"]),
            expected,
        );
        assert_eq!(parse_slice(&["rcl", "e", "--output=json", "f"]), expected);
        assert_eq!(parse_slice(&["rcl", "-o", "json", "e", "f"]), expected);
        assert_eq!(parse_slice(&["rcl", "-ojson", "e", "f"]), expected);
        assert_eq!(
            parse_slice(&["rcl", "e", "-ojson", "-orcl", "-ojson", "f"]),
            expected,
        );
    }

    #[test]
    fn parse_handles_raw_args() {
        let expected = Cmd::Evaluate {
            color: None,
            width: None,
            output: Output::Rcl,
            fname: "--output".to_string(),
        };
        assert_eq!(parse_slice(&["rcl", "e", "--", "--output"]), expected);
    }

    #[test]
    fn parse_handles_dash_as_arg() {
        let expected = Cmd::Evaluate {
            color: None,
            width: None,
            output: Output::Rcl,
            fname: "-".to_string(),
        };
        assert_eq!(parse_slice(&["rcl", "e", "-"]), expected);
    }

    #[test]
    fn parse_returns_error_on_misuse() {
        // TODO: Write some tests for errors.
    }
}
