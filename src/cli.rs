// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Types that represent a parsed command line, and functions to parse it.

use std::str::FromStr;

use crate::cli_utils::{match_option, parse_option, Arg, ArgIter};
use crate::error::{Error, Result};
use crate::loader::SandboxMode;
use crate::markup::{Markup, MarkupMode};
use crate::pprint::{concat, Doc};

const USAGE_MAIN: &str = r#"
RCL -- A reasonable configuration language.

Usage:
  rcl [<options>] <command> <arguments>

Commands:
  evaluate     Evaluate a document to an output format.
  format       Auto-format an RCL document.
  highlight    Print a document with syntax highlighting.
  query        Evaluate an expression against an input document.

Command shorthands:
  e, eval      Alias for 'evaluate'.
  f, fmt       Alias for 'format'.
  h            Alias for 'highlight'.
  jq           Alias for 'query --output=json'.
  je           Alias for 'eval --output=json'.
  q            Alias for 'query'.

Global options:
  -h --help             Show this screen, or command-specific help.
  --version             Show version.
  --color <mode>        Set how output is colored, see modes below.
  -C --directory <dir>  Change the working directory.

Color modes:
  ansi    Always color output using ANSI escape codes.
  auto    Use ANSI if the output file is a TTY and the NO_COLOR environment
          variable is not set to a non-empty string. This is the default.
  none    Do not color output at all.
"#;

const USAGE_EVAL_QUERY: &str = r#"
RCL -- A reasonable configuration language.

Usage:
  rcl [<options>] evaluate [<options>] [<file>]
  rcl [<options>] query    [<options>] [<file>] <query>

The 'evaluate' command evaluates the expression in the input file and prints it
to stdout. The 'query' command additionally evaluates an expression against the
result.

Arguments:
  <file>     The input file to process, or '-' for stdin. When the file is not
             specified and stdin is not a TTY, stdin is used as default.
  <query>    An RCL expression to evaluate. The result of evaluating the input
             file is bound to the variable 'input'.

Options:
  -o --output <format>  Output format, can be one of 'json', 'rcl'.
                        Defaults to 'rcl'.
  -w --width <width>    Target width for pretty-printing, must be an integer.
                        Defaults to 80.
  --sandbox <mode>      Sandboxing mode, can be one of 'workdir' (the default)
                        and 'unrestricted'.

Sandboxing modes:
  workdir       Only allow importing files inside the working directory and
                subdirectories.
  unrestricted  Grant unrestricted filesystem access, allow importing any file.

See also --help for global options.
"#;

const USAGE_FORMAT: &str = r#"
RCL -- A reasonable configuration language.

Usage:
  rcl [<options>] format [<options>] [<file>...]

The 'format' command formats one or more input documents in standard style.

Arguments:
  <file>...        The input files to process, or '-' for stdin. When --in-place
                   is used, there can be multiple input files. When no file is
                   specified and stdin is not a TTY, stdin is used as default.

Options:
  -i --in-place       Rewrite files in-place instead of writing to stdout.
                      By default the formatted result is written to stdout.
  -w --width <width>  Target width in number of columns, must be an integer.
                      Defaults to 80.

See also --help for global options.
"#;

/// Whether stdin is a TTY or not.
#[derive(Copy, Clone)]
pub enum StdinCapabilities {
    Terminal,
    NonInteractive,
}

impl StdinCapabilities {
    pub fn from_stdin() -> Self {
        use std::io::IsTerminal;
        if std::io::stdin().is_terminal() {
            StdinCapabilities::Terminal
        } else {
            StdinCapabilities::NonInteractive
        }
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self, StdinCapabilities::Terminal)
    }
}

/// Options that apply to all subcommands.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct GlobalOptions {
    /// Whether and how to output color and other markup.
    ///
    /// We call it “markup” internally because it's more than just color, but
    /// we call it `--color` on the command line because that is what most tools
    /// call it.
    pub markup: Option<MarkupMode>,

    /// Alter the working directory for filesystem access.
    pub workdir: Option<String>,
}

/// The available output formats (JSON, RCL).
#[derive(Debug, Default, Eq, PartialEq)]
pub enum OutputFormat {
    Json,
    #[default]
    Rcl,
}

/// Options for commands that evaluate expressions.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct EvalOptions {
    /// The format to output in.
    pub format: OutputFormat,

    /// Policy for what files can be imported.
    pub sandbox: SandboxMode,
}

/// Options for commands that pretty-print their output.
#[derive(Debug, Eq, PartialEq)]
pub struct FormatOptions {
    /// Target width (number of columns) to try to not exceed.
    pub width: u32,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self { width: 80 }
    }
}

/// Input to act on.
#[derive(Debug, Eq, PartialEq)]
pub enum Target {
    File(String),
    Stdin,
}

/// For the `fmt` command, which documents to format, and in what mode.
#[derive(Debug, Eq, PartialEq)]
pub enum FormatTarget {
    Stdout { fname: Target },
    InPlace { fnames: Vec<Target> },
}

/// The different subcommands supported by the main program.
#[derive(Debug, Eq, PartialEq)]
pub enum Cmd {
    Evaluate {
        eval_opts: EvalOptions,
        format_opts: FormatOptions,
        fname: Target,
    },
    Query {
        eval_opts: EvalOptions,
        format_opts: FormatOptions,
        fname: Target,
        query: String,
    },
    Format {
        format_opts: FormatOptions,
        target: FormatTarget,
    },
    Highlight {
        fname: Target,
    },
    Help {
        usage: &'static str,
    },
    Version,
}

/// Parse the command line.
pub fn parse(args: Vec<String>, stdin: StdinCapabilities) -> Result<(GlobalOptions, Cmd)> {
    let mut args = ArgIter::new(args);

    // Skip over the program name.
    args.next();

    let mut cmd: Option<&'static str> = None;
    let mut cmd_help: Option<&'static str> = None;
    let mut format_opts = FormatOptions::default();
    let mut global_opts = GlobalOptions::default();
    let mut eval_opts = EvalOptions::default();
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
                eval_opts.format = match_option! {
                    args: arg,
                    "json" => OutputFormat::Json,
                    "rcl" => OutputFormat::Rcl,
                }
            }
            Arg::Long("sandbox") => {
                eval_opts.sandbox = match_option! {
                    args: arg,
                    "workdir" => SandboxMode::Workdir,
                    "unrestricted" => SandboxMode::Unrestricted,
                }
            }
            Arg::Long("directory") | Arg::Short("C") => {
                global_opts.workdir = parse_option! {
                    args: arg,
                    |x: &str| Ok::<_, std::convert::Infallible>(Some(x.to_string()))
                };
            }
            Arg::Long("width") | Arg::Short("w") => {
                format_opts.width = parse_option! { args: arg, u32::from_str };
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
            Arg::Plain("je") if cmd.is_none() => {
                cmd = Some("evaluate");
                eval_opts.format = OutputFormat::Json;
            }
            Arg::Plain("query") | Arg::Plain("q") if cmd.is_none() => {
                cmd = Some("query");
            }
            Arg::Plain("jq") if cmd.is_none() => {
                cmd = Some("query");
                eval_opts.format = OutputFormat::Json;
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
                    Doc::highlight(&arg.to_string()).into_owned()
                    "'. See --help for usage."
                };
                return Error::new(err).err();
            }
            _ => {
                let err = concat! {
                    "Unknown option '"
                    Doc::highlight(&arg.to_string()).into_owned()
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
            eval_opts,
            format_opts,
            fname: get_unique_target(targets, stdin)?,
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
                // If no file was specified, default to stdin, unless stdin is
                // a TTY, because that makes the application appear to hang and
                // it's likely not what the user intended; if it was intentional
                // they can specify '-' as input explicitly.
                1 if !stdin.is_terminal() => (
                    Target::Stdin,
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
                eval_opts,
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
                    fname: get_unique_target(targets, stdin)?,
                }
            },
        },
        Some("highlight") => Cmd::Highlight {
            fname: get_unique_target(targets, stdin)?,
        },
        None => Cmd::Help { usage: USAGE_MAIN },
        _ => panic!("Should have returned an error before getting here."),
    };
    Ok((global_opts, result))
}

fn get_unique_target(mut targets: Vec<Target>, stdin: StdinCapabilities) -> Result<Target> {
    match targets.pop() {
        None if stdin.is_terminal() => {
            Error::new("Expected an input file. See --help for usage.").err()
        }
        // If stdin is not a terminal, default to stdin. We don't
        // unconditionally default to stdin, because if you don't know that
        // the application is waiting for EOF, it looks like it hangs.
        None => Ok(Target::Stdin),
        Some(_) if !targets.is_empty() => {
            Error::new("Too many input files. See --help for usage.").err()
        }
        Some(t) => Ok(t),
    }
}

#[cfg(test)]
mod test {
    use crate::cli::{
        Cmd, EvalOptions, FormatOptions, FormatTarget, GlobalOptions, OutputFormat, SandboxMode,
        StdinCapabilities, Target,
    };
    use crate::markup::MarkupMode;
    use crate::pprint::Config;

    fn fail_parse_tty(args: &[&'static str]) -> String {
        fail_parse_with(args, StdinCapabilities::Terminal)
    }

    fn fail_parse_with(args: &[&'static str], stdin: StdinCapabilities) -> String {
        let args_vec: Vec<_> = args.iter().map(|a| a.to_string()).collect();
        let err = super::parse(args_vec, stdin).err().unwrap();
        let cfg = Config {
            width: 80,
            markup: MarkupMode::None,
        };
        err.report(&[]).println(&cfg)
    }

    fn parse_tty(args: &[&'static str]) -> (GlobalOptions, Cmd) {
        parse_with(args, StdinCapabilities::Terminal)
    }

    fn parse_not_tty(args: &[&'static str]) -> (GlobalOptions, Cmd) {
        parse_with(args, StdinCapabilities::NonInteractive)
    }

    fn parse_with(args: &[&'static str], stdin: StdinCapabilities) -> (GlobalOptions, Cmd) {
        let args_vec: Vec<_> = args.iter().map(|a| a.to_string()).collect();
        super::parse(args_vec, stdin).unwrap()
    }

    #[test]
    fn parse_cmd_eval() {
        let expected_opt = GlobalOptions {
            markup: None,
            workdir: None,
        };
        let expected_cmd = Cmd::Evaluate {
            eval_opts: EvalOptions::default(),
            format_opts: FormatOptions::default(),
            fname: Target::File("infile".into()),
        };
        let mut expected = (expected_opt, expected_cmd);

        // All of the aliases should behave the same.
        assert_eq!(parse_tty(&["rcl", "evaluate", "infile"]), expected);
        assert_eq!(parse_tty(&["rcl", "eval", "infile"]), expected);
        assert_eq!(parse_tty(&["rcl", "e", "infile"]), expected);

        // Test that --color works.
        assert_eq!(parse_tty(&["rcl", "--color=auto", "e", "infile"]), expected);
        expected.0.markup = Some(MarkupMode::None);
        assert_eq!(parse_tty(&["rcl", "--color=none", "e", "infile"]), expected);
        expected.0.markup = Some(MarkupMode::Ansi);
        assert_eq!(parse_tty(&["rcl", "--color=ansi", "e", "infile"]), expected);

        // We should be able to pass --color in any place.
        assert_eq!(parse_tty(&["rcl", "--color=ansi", "e", "infile"]), expected);
        assert_eq!(
            parse_tty(&["rcl", "--color", "ansi", "e", "infile"]),
            expected
        );
        assert_eq!(
            parse_tty(&["rcl", "eval", "--color", "ansi", "infile"]),
            expected
        );
        assert_eq!(
            parse_tty(&["rcl", "eval", "infile", "--color", "ansi"]),
            expected
        );

        // If we specify an option twice, the last one takes precedence.
        assert_eq!(
            parse_tty(&["rcl", "e", "infile", "--color=none", "--color=ansi"]),
            expected
        );

        // Test that --width works, in any location, last option wins.
        expected.0.markup = None;
        if let Cmd::Evaluate { format_opts, .. } = &mut expected.1 {
            format_opts.width = 42;
        }
        assert_eq!(parse_tty(&["rcl", "e", "--width=42", "infile"]), expected);
        assert_eq!(
            parse_tty(&["rcl", "e", "--width", "42", "infile"]),
            expected
        );
        assert_eq!(parse_tty(&["rcl", "e", "-w42", "infile"]), expected);
        assert_eq!(parse_tty(&["rcl", "e", "-w", "42", "infile"]), expected);
        assert_eq!(parse_tty(&["rcl", "e", "infile", "-w42"]), expected);
        assert_eq!(parse_tty(&["rcl", "-w42", "e", "infile"]), expected);
        assert_eq!(
            parse_tty(&["rcl", "-w100", "e", "--width=42", "infile"]),
            expected
        );

        // Test that --output works. We don't have to be as thorough, it's using
        // the same parser, if it works for the other options it should work here.
        if let Cmd::Evaluate {
            format_opts,
            eval_opts,
            ..
        } = &mut expected.1
        {
            format_opts.width = 80;
            eval_opts.format = OutputFormat::Json;
        }
        assert_eq!(parse_tty(&["rcl", "e", "infile", "-ojson"]), expected);
        assert_eq!(
            parse_tty(&["rcl", "e", "infile", "--output", "json"]),
            expected
        );
        assert_eq!(
            parse_tty(&["rcl", "e", "infile", "--output=json"]),
            expected
        );
        assert_eq!(parse_tty(&["rcl", "-ojson", "e", "infile"]), expected);
        assert_eq!(
            parse_tty(&["rcl", "-orcl", "-ojson", "e", "infile"]),
            expected
        );
        assert_eq!(parse_tty(&["rcl", "je", "infile"]), expected);

        // Test --sandbox.
        if let Cmd::Evaluate { eval_opts, .. } = &mut expected.1 {
            eval_opts.format = OutputFormat::Rcl;
            eval_opts.sandbox = SandboxMode::Unrestricted;
        }
        assert_eq!(
            parse_tty(&["rcl", "e", "infile", "--sandbox=unrestricted"]),
            expected
        );
        if let Cmd::Evaluate { eval_opts, .. } = &mut expected.1 {
            eval_opts.sandbox = SandboxMode::Workdir;
        }
        assert_eq!(
            parse_tty(&["rcl", "e", "infile", "--sandbox=workdir"]),
            expected
        );
        assert_eq!(parse_tty(&["rcl", "e", "infile"]), expected);

        // Test that defaulting to stdin works. If '-' should always work;
        // omitting the input file entirely should only work when stdin is
        // not a TTY.
        if let Cmd::Evaluate { fname, .. } = &mut expected.1 {
            *fname = Target::Stdin;
        }
        assert_eq!(parse_tty(&["rcl", "e", "-"]), expected);
        assert_eq!(parse_not_tty(&["rcl", "e", "-"]), expected);
        assert_eq!(parse_not_tty(&["rcl", "e"]), expected);
        assert_eq!(
            fail_parse_tty(&["rcl", "e"]),
            "Error: Expected an input file. See --help for usage.\n",
        );
    }

    #[test]
    fn parse_cmd_eval_fails_on_invalid_usage() {
        assert_eq!(
            fail_parse_tty(&["rcl", "eval"]),
            "Error: Expected an input file. See --help for usage.\n"
        );
        assert_eq!(
            fail_parse_tty(&["rcl", "eval", "infile", "--width=bobcat"]),
            "Error: 'bobcat' is not valid for --width. See --help for usage.\n"
        );
        assert_eq!(
            fail_parse_tty(&["rcl", "eval", "infile", "-wbobcat"]),
            "Error: 'bobcat' is not valid for -w. See --help for usage.\n"
        );
        assert_eq!(
            fail_parse_tty(&["rcl", "eval", "infile", "--output=yamr"]),
            "Error: Expected --output to be followed by one of json, rcl. See --help for usage.\n"
        );
        assert_eq!(
            fail_parse_tty(&["rcl", "frobnicate", "infile"]),
            "Error: Unknown command 'frobnicate'. See --help for usage.\n"
        );
        assert_eq!(
            fail_parse_tty(&["rcl", "eval", "--frobnicate", "infile"]),
            "Error: Unknown option '--frobnicate'. See --help for usage.\n"
        );
    }

    #[test]
    fn parse_cmd_fmt() {
        let expected_opt = GlobalOptions {
            markup: None,
            workdir: None,
        };
        let expected_cmd = Cmd::Format {
            format_opts: FormatOptions::default(),
            target: FormatTarget::Stdout {
                fname: Target::File("infile".into()),
            },
        };
        let mut expected = (expected_opt, expected_cmd);

        // All of the aliases should behave the same.
        assert_eq!(parse_tty(&["rcl", "format", "infile"]), expected);
        assert_eq!(parse_tty(&["rcl", "fmt", "infile"]), expected);
        assert_eq!(parse_tty(&["rcl", "f", "infile"]), expected);

        // Without --in-place, we can do only one arg.
        assert_eq!(
            fail_parse_tty(&["rcl", "f", "f1", "f2"]),
            "Error: Too many input files. See --help for usage.\n",
        );

        if let Cmd::Format { ref mut target, .. } = &mut expected.1 {
            *target = FormatTarget::InPlace {
                fnames: vec![Target::File("f1".into()), Target::File("f2".into())],
            };
        }
        assert_eq!(parse_tty(&["rcl", "f", "--in-place", "f1", "f2"]), expected);
        assert_eq!(parse_tty(&["rcl", "-i", "f", "f1", "f2"]), expected);
    }

    #[test]
    fn parse_cmd_help_version() {
        assert!(matches!(parse_tty(&["rcl", "--help"]).1, Cmd::Help { .. }));
        assert!(matches!(parse_tty(&["rcl", "--version"]).1, Cmd::Version));
        assert!(matches!(
            parse_tty(&["rcl", "eval", "-h"]).1,
            Cmd::Help { .. }
        ));
        assert!(matches!(
            parse_tty(&["rcl", "format", "-h"]).1,
            Cmd::Help { .. }
        ));
        assert!(matches!(
            parse_tty(&["rcl", "highlight", "-h"]).1,
            Cmd::Help { .. }
        ));
        assert!(matches!(
            parse_tty(&["rcl", "query", "-h"]).1,
            Cmd::Help { .. }
        ));
        // Missing subcommand also triggers help.
        assert!(matches!(parse_tty(&["rcl"]).1, Cmd::Help { .. }));
    }

    #[test]
    fn parse_cmd_highlight() {
        let expected_opt = GlobalOptions {
            markup: None,
            workdir: None,
        };
        let expected_cmd = Cmd::Highlight {
            fname: Target::File("infile".into()),
        };
        let expected = (expected_opt, expected_cmd);
        assert_eq!(parse_tty(&["rcl", "highlight", "infile"]), expected);
    }

    #[test]
    fn parse_cmd_query() {
        let expected_opt = GlobalOptions {
            markup: None,
            workdir: None,
        };
        let expected_cmd = Cmd::Query {
            eval_opts: EvalOptions::default(),
            format_opts: FormatOptions::default(),
            fname: Target::File("infile".into()),
            query: "input.name".to_string(),
        };
        let mut expected = (expected_opt, expected_cmd);
        assert_eq!(
            parse_tty(&["rcl", "query", "infile", "input.name"]),
            expected
        );
        assert_eq!(parse_tty(&["rcl", "q", "infile", "input.name"]), expected);

        if let Cmd::Query { eval_opts, .. } = &mut expected.1 {
            eval_opts.format = OutputFormat::Json
        };
        assert_eq!(parse_tty(&["rcl", "jq", "infile", "input.name"]), expected);

        assert_eq!(
            fail_parse_tty(&["rcl", "q", "infile"]),
            "Error: Expected an input file and a query. See --help for usage.\n",
        );
    }

    #[test]
    fn parse_cmd_handles_stdin_and_double_dash() {
        assert_eq!(
            parse_tty(&["rcl", "highlight", "infile"]).1,
            Cmd::Highlight {
                fname: Target::File("infile".into()),
            }
        );
        assert_eq!(
            parse_tty(&["rcl", "highlight", "--", "infile"]).1,
            Cmd::Highlight {
                fname: Target::File("infile".into()),
            }
        );
        assert_eq!(
            parse_tty(&["rcl", "highlight", "-"]).1,
            Cmd::Highlight {
                fname: Target::Stdin,
            }
        );
        assert_eq!(
            parse_tty(&["rcl", "highlight", "--", "-"]).1,
            Cmd::Highlight {
                fname: Target::File("-".into()),
            }
        );
    }
}
