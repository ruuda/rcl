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
  build        Write formatted evaluation results to files.
  evaluate     Evaluate a document to an output format.
  format       Auto-format an RCL document.
  highlight    Print a document with syntax highlighting.
  query        Evaluate an expression against an input document.

Command shorthands:
  e, eval      Alias for 'evaluate'.
  f, fmt       Alias for 'format'.
  h            Alias for 'highlight'.
  jq           Alias for 'query --format=json'.
  je           Alias for 'eval --format=json'.
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
  html    Output HTML tags in the same style as Pandoc.
  none    Do not color output at all.
"#;

const USAGE_BUILD: &str = r##"
RCL -- A reasonable configuration language.

Usage:
  rcl [<options>] build [<buildfile>]

The 'build' command writes formatted values to files. It can be used to update
many generated files in one command, similar to a build tool like Make or Ninja,
but with the build targets specified in RCL rather than a makefile. The build
file is an RCL document that should evaluate to a dict that maps output file
paths to targets. Targets are dicts with fields as described below.

Arguments:
  <buildfile>       The file with build targets to process, or '-' for stdin.
                    Defaults to 'build.rcl' when no file is specified.

Options:
  --sandbox <mode>  Sandboxing mode, see 'rcl evaluate --help` for an
                    explanation of the modes. Defaults to 'workdir'.

See also --help for global options.

Example build file:

  {
    "alice.toml": {
      contents = { name = "Alice", uid = 42 },
      format = "toml",
      banner = "# This file is generated from build.rcl.\n",
    },
    "bob.toml": {
      contents = { name = "Bob", uid = 43 },
      format = "toml",
    },
  }

Build target fields:

  banner: String    A string to prepend to the output file. For example, a
                    comment to clarify that the file is generated. Defaults
                    to an empty string. Corresponds to 'rcl evaluate --banner',
                    but unlike --banner, no implicit newline is added here.
  contents: Any     The value to format and write to the output file.
  format: String    The output format, must be one of the formats supported by
                    'rcl evaluate --format', see 'rcl evaluate --help'.
  width: Int        Target width for formatting, as for 'rcl evaluate --width'.
                    Optional, defaults to 80.
"##;

const USAGE_EVAL_QUERY: &str = r#"
RCL -- A reasonable configuration language.

Usage:
  rcl [<options>] evaluate [<options>] [<file>]
  rcl [<options>] query    [<options>] [<file>] <query>

The 'evaluate' command evaluates the expression in the input file and prints it
to stdout. The 'query' command additionally evaluates an expression against the
result.

Arguments:
  <file>     The input file to process, or '-' for stdin. Defaults to stdin when
             no file is specified.
  <query>    An RCL expression to evaluate. The result of evaluating the input
             file is bound to the variable 'input'.

Options:
  --banner <message>       Prepend the message to the output. This can be useful
                           to add headings or comments to generated files.
  -f --format <format>     Output format, see below for the available formats.
                           Defaults to 'rcl'.
  -o --output <outfile>    Write to the given file instead of stdout.
  --output-depfile <file>  Write all dependencies that were loaded during
                           evaluation to <file> in Makefile syntax. This can be
                           used by e.g. the Ninja build system.
  --sandbox <mode>         Sandboxing mode, see below. Defaults to 'workdir'.
  -w --width <width>       Target width for pretty-printing, must be an integer.
                           Defaults to 80.

Output format:
  json          Output pretty-printed JSON.
  raw           If the document is a string, output the string itself. If the
                document is a list or set of strings, output each string on its
                own line.
  rcl           Output pretty-printed RCL.
  toml          Output TOML.
  yaml-stream   If the document is a list, output every element as a JSON
                document, prefixed by the '---' YAML document separator.
                Top-level values other than lists are not valid for this format.

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
                   is used, there can be multiple input files. Defaults to stdin
                   when no file is specified.

Options:
  -i --in-place          Rewrite files in-place instead of writing to stdout.
                         By default the formatted result is written to stdout.
  -o --output <outfile>  Write to the given file instead of stdout. This is
                         incompatible with --in-place.
  -w --width <width>     Target width in number of columns, must be an integer.
                         Defaults to 80.

See also --help for global options.
"#;

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
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub enum OutputFormat {
    Json,
    Raw,
    #[default]
    Rcl,
    Toml,
    YamlStream,
}

/// Options for commands that evaluate expressions.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct EvalOptions {
    /// The format to output in.
    pub format: OutputFormat,

    /// Policy for what files can be imported.
    pub sandbox: SandboxMode,

    /// File to write dependencies to.
    ///
    /// See also the depfile documentation from the Ninja build system:
    /// <https://ninja-build.org/manual.html#_depfile>.
    pub output_depfile: Option<String>,

    /// A banner message to prepend to the output.
    pub banner: Option<String>,
}

/// Options for commands that pretty-print their output.
#[derive(Debug, Eq, PartialEq)]
pub struct StyleOptions {
    /// Target width (number of columns) to try to not exceed.
    pub width: u32,
}

impl Default for StyleOptions {
    fn default() -> Self {
        Self { width: 80 }
    }
}

/// Input to act on.
#[derive(Debug, Eq, PartialEq)]
pub enum Target {
    /// A file, selected explicitly.
    File(String),

    /// Stdin, selected explicitly.
    Stdin,

    /// Stdin, selected by default through absence of a file argument.
    ///
    /// We distinguish this from explicit `Stdin`, so we can print a
    /// note/warning when stdin is a TTY, because that case is likely
    /// unintentional, and it looks like the application hangs when the
    /// user doesn't realize it's waiting for input.
    StdinDefault,
}

/// For the `fmt` command, which documents to format, and in what mode.
#[derive(Debug, Eq, PartialEq)]
pub enum FormatTarget {
    Stdout { fname: Target },
    InPlace { fnames: Vec<Target> },
    Check { fnames: Vec<Target> },
}

/// An output file to write results to.
#[derive(Debug, Eq, PartialEq)]
pub enum OutputTarget {
    /// Write to the given file.
    File(String),
    /// Write to stdout.
    Stdout,
}

/// The different subcommands supported by the main program.
#[derive(Debug, Eq, PartialEq)]
pub enum Cmd {
    Build {
        eval_opts: EvalOptions,
        fname: Target,
    },
    Evaluate {
        eval_opts: EvalOptions,
        style_opts: StyleOptions,
        fname: Target,
        output: OutputTarget,
    },
    Query {
        eval_opts: EvalOptions,
        style_opts: StyleOptions,
        fname: Target,
        query: String,
        output: OutputTarget,
    },
    Format {
        style_opts: StyleOptions,
        target: FormatTarget,
        output: OutputTarget,
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
pub fn parse(args: Vec<String>) -> Result<(GlobalOptions, Cmd)> {
    let mut args = ArgIter::new(args);

    // Skip over the program name.
    args.next();

    let mut cmd: Option<&'static str> = None;
    let mut cmd_help: Option<&'static str> = None;
    let mut style_opts = StyleOptions::default();
    let mut global_opts = GlobalOptions::default();
    let mut eval_opts = EvalOptions::default();
    let mut in_place = false;
    let mut check = false;
    let mut is_version = false;
    let mut targets: Vec<Target> = Vec::new();
    let mut output = OutputTarget::Stdout;

    while let Some(arg) = args.next() {
        match arg.as_ref() {
            Arg::Long("banner") => {
                eval_opts.banner = parse_option! {
                    args: arg,
                    |x: &str| Ok::<_, std::convert::Infallible>(Some(x.to_string()))
                };
            }
            Arg::Long("check") => {
                check = true;
            }
            Arg::Long("color") => {
                global_opts.markup = match_option! {
                    args: arg,
                    "auto" => None,
                    "ansi" => Some(MarkupMode::Ansi),
                    "html" => Some(MarkupMode::HtmlPandoc),
                    "none" => Some(MarkupMode::None),
                }
            }
            Arg::Long("directory") | Arg::Short("C") => {
                global_opts.workdir = parse_option! {
                    args: arg,
                    |x: &str| Ok::<_, std::convert::Infallible>(Some(x.to_string()))
                };
            }
            Arg::Long("format") | Arg::Short("f") => {
                eval_opts.format = match_option! {
                    args: arg,
                    "json" => OutputFormat::Json,
                    "raw" => OutputFormat::Raw,
                    "rcl" => OutputFormat::Rcl,
                    "toml" => OutputFormat::Toml,
                    "yaml-stream" => OutputFormat::YamlStream,
                }
            }
            Arg::Long("output") | Arg::Short("o") => {
                output = parse_option! {
                    args: arg,
                    |x: &str| Ok::<_, std::convert::Infallible>(OutputTarget::File(x.to_string()))
                };
            }
            Arg::Long("output-depfile") => {
                eval_opts.output_depfile = parse_option! {
                    args: arg,
                    |x: &str| Ok::<_, std::convert::Infallible>(Some(x.to_string()))
                };
            }
            Arg::Long("sandbox") => {
                eval_opts.sandbox = match_option! {
                    args: arg,
                    "workdir" => SandboxMode::Workdir,
                    "unrestricted" => SandboxMode::Unrestricted,
                }
            }
            Arg::Long("width") | Arg::Short("w") => {
                style_opts.width = parse_option! { args: arg, u32::from_str };
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
            Arg::Plain("build") if cmd.is_none() => {
                cmd = Some("build");
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
        Some("build") => Some(Cmd::Help { usage: USAGE_BUILD }),
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
        Some("build") => {
            // Unlike other commands, for `rcl build` the input file defaults to
            // build.rcl instead of stdin.
            if targets.is_empty() {
                targets.push(Target::File("build.rcl".to_string()));
            }
            Cmd::Build {
                eval_opts,
                fname: get_unique_target(targets)?,
            }
        }
        Some("evaluate") => Cmd::Evaluate {
            eval_opts,
            style_opts,
            fname: get_unique_target(targets)?,
            output,
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
                        Target::StdinDefault => {
                            unreachable!("Only produced through absence of args.")
                        }
                    },
                ),
                1 => (
                    Target::StdinDefault,
                    match targets.remove(0) {
                        Target::File(query) => query,
                        Target::Stdin => "-".to_string(),
                        Target::StdinDefault => {
                            unreachable!("Only produced through absence of args.")
                        }
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
                style_opts,
                query,
                fname,
                output,
            }
        }
        Some("format") => Cmd::Format {
            style_opts,
            target: if in_place {
                FormatTarget::InPlace { fnames: targets }
            } else if check {
                FormatTarget::Check { fnames: targets }
            } else {
                FormatTarget::Stdout {
                    fname: get_unique_target(targets)?,
                }
            },
            output,
        },
        Some("highlight") => Cmd::Highlight {
            fname: get_unique_target(targets)?,
        },
        None => Cmd::Help { usage: USAGE_MAIN },
        _ => panic!("Should have returned an error before getting here."),
    };
    Ok((global_opts, result))
}

fn get_unique_target(mut targets: Vec<Target>) -> Result<Target> {
    match targets.pop() {
        None => Ok(Target::StdinDefault),
        Some(_) if !targets.is_empty() => {
            Error::new("Too many input files. See --help for usage.").err()
        }
        Some(t) => Ok(t),
    }
}

#[cfg(test)]
mod test {
    use crate::cli::{
        Cmd, EvalOptions, FormatTarget, GlobalOptions, OutputFormat, OutputTarget, SandboxMode,
        StyleOptions, Target,
    };
    use crate::markup::MarkupMode;
    use crate::pprint::Config;

    fn fail_parse(args: &[&'static str]) -> String {
        let args_vec: Vec<_> = args.iter().map(|a| a.to_string()).collect();
        let err = super::parse(args_vec).err().unwrap();
        let cfg = Config { width: 80 };
        err.report(&[]).println(&cfg).to_string_no_markup()
    }

    fn parse(args: &[&'static str]) -> (GlobalOptions, Cmd) {
        let args_vec: Vec<_> = args.iter().map(|a| a.to_string()).collect();
        super::parse(args_vec).unwrap()
    }

    #[test]
    fn parse_cmd_eval() {
        let expected_opt = GlobalOptions {
            markup: None,
            workdir: None,
        };
        let expected_cmd = Cmd::Evaluate {
            eval_opts: EvalOptions::default(),
            style_opts: StyleOptions::default(),
            fname: Target::File("infile".into()),
            output: OutputTarget::Stdout,
        };
        let mut expected = (expected_opt, expected_cmd);

        // All of the aliases should behave the same.
        assert_eq!(parse(&["rcl", "evaluate", "infile"]), expected);
        assert_eq!(parse(&["rcl", "eval", "infile"]), expected);
        assert_eq!(parse(&["rcl", "e", "infile"]), expected);

        // Test that --color works.
        assert_eq!(parse(&["rcl", "--color=auto", "e", "infile"]), expected);
        expected.0.markup = Some(MarkupMode::None);
        assert_eq!(parse(&["rcl", "--color=none", "e", "infile"]), expected);
        expected.0.markup = Some(MarkupMode::HtmlPandoc);
        assert_eq!(parse(&["rcl", "--color=html", "e", "infile"]), expected);
        expected.0.markup = Some(MarkupMode::Ansi);
        assert_eq!(parse(&["rcl", "--color=ansi", "e", "infile"]), expected);

        // We should be able to pass --color in any place.
        assert_eq!(parse(&["rcl", "--color=ansi", "e", "infile"]), expected);
        assert_eq!(parse(&["rcl", "--color", "ansi", "e", "infile"]), expected);
        assert_eq!(
            parse(&["rcl", "eval", "--color", "ansi", "infile"]),
            expected
        );
        assert_eq!(
            parse(&["rcl", "eval", "infile", "--color", "ansi"]),
            expected
        );

        // If we specify an option twice, the last one takes precedence.
        assert_eq!(
            parse(&["rcl", "e", "infile", "--color=none", "--color=ansi"]),
            expected
        );

        // Test that --width works, in any location, last option wins.
        expected.0.markup = None;
        if let Cmd::Evaluate { style_opts, .. } = &mut expected.1 {
            style_opts.width = 42;
        }
        assert_eq!(parse(&["rcl", "e", "--width=42", "infile"]), expected);
        assert_eq!(parse(&["rcl", "e", "--width", "42", "infile"]), expected);
        assert_eq!(parse(&["rcl", "e", "-w42", "infile"]), expected);
        assert_eq!(parse(&["rcl", "e", "-w", "42", "infile"]), expected);
        assert_eq!(parse(&["rcl", "e", "infile", "-w42"]), expected);
        assert_eq!(parse(&["rcl", "-w42", "e", "infile"]), expected);
        assert_eq!(
            parse(&["rcl", "-w100", "e", "--width=42", "infile"]),
            expected
        );

        // Test that --format works. We don't have to be as thorough, it's using
        // the same parser, if it works for the other options it should work here.
        if let Cmd::Evaluate {
            style_opts,
            eval_opts,
            ..
        } = &mut expected.1
        {
            style_opts.width = 80;
            eval_opts.format = OutputFormat::Json;
        }
        assert_eq!(parse(&["rcl", "e", "infile", "-fjson"]), expected);
        assert_eq!(parse(&["rcl", "e", "infile", "--format", "json"]), expected);
        assert_eq!(parse(&["rcl", "e", "infile", "--format=json"]), expected);
        assert_eq!(parse(&["rcl", "-fjson", "e", "infile"]), expected);
        assert_eq!(parse(&["rcl", "-frcl", "-fjson", "e", "infile"]), expected);
        assert_eq!(parse(&["rcl", "je", "infile"]), expected);

        if let Cmd::Evaluate { eval_opts, .. } = &mut expected.1 {
            eval_opts.format = OutputFormat::Raw;
        }
        assert_eq!(parse(&["rcl", "e", "infile", "-fraw"]), expected);

        // Test --sandbox.
        if let Cmd::Evaluate { eval_opts, .. } = &mut expected.1 {
            eval_opts.format = OutputFormat::Rcl;
            eval_opts.sandbox = SandboxMode::Unrestricted;
        }
        assert_eq!(
            parse(&["rcl", "e", "infile", "--sandbox=unrestricted"]),
            expected
        );
        if let Cmd::Evaluate { eval_opts, .. } = &mut expected.1 {
            eval_opts.sandbox = SandboxMode::Workdir;
        }
        assert_eq!(
            parse(&["rcl", "e", "infile", "--sandbox=workdir"]),
            expected
        );
        assert_eq!(parse(&["rcl", "e", "infile"]), expected);

        // Test --banner
        if let Cmd::Evaluate { eval_opts, .. } = &mut expected.1 {
            eval_opts.banner = Some("prefix".to_string());
        }
        assert_eq!(parse(&["rcl", "e", "infile", "--banner=prefix"]), expected);

        // Test that defaulting to stdin works. If '-' is there we get it
        // explicitly, if it's not, we get it implicitly.
        if let Cmd::Evaluate {
            fname, eval_opts, ..
        } = &mut expected.1
        {
            eval_opts.banner = None;
            *fname = Target::Stdin;
        }
        assert_eq!(parse(&["rcl", "e", "-"]), expected);
        if let Cmd::Evaluate { fname, .. } = &mut expected.1 {
            *fname = Target::StdinDefault;
        }
        assert_eq!(parse(&["rcl", "e"]), expected);

        // Test the --output flag.
        if let Cmd::Evaluate { output, .. } = &mut expected.1 {
            *output = OutputTarget::File("outfile".to_string());
        }
        assert_eq!(parse(&["rcl", "e", "--output=outfile"]), expected);
        assert_eq!(parse(&["rcl", "-ooutfile", "evaluate"]), expected);
    }

    #[test]
    fn parse_cmd_eval_fails_on_invalid_usage() {
        assert_eq!(
            fail_parse(&["rcl", "eval", "infile", "--width=bobcat"]),
            "Error: 'bobcat' is not valid for --width. See --help for usage.\n"
        );
        assert_eq!(
            fail_parse(&["rcl", "eval", "infile", "-wbobcat"]),
            "Error: 'bobcat' is not valid for -w. See --help for usage.\n"
        );
        assert_eq!(
            fail_parse(&["rcl", "eval", "infile", "--format=yamr"]),
            "Error: Expected --format to be followed by one of json, raw, rcl, toml, yaml-stream. See --help for usage.\n"
        );
        assert_eq!(
            fail_parse(&["rcl", "frobnicate", "infile"]),
            "Error: Unknown command 'frobnicate'. See --help for usage.\n"
        );
        assert_eq!(
            fail_parse(&["rcl", "eval", "--frobnicate", "infile"]),
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
            style_opts: StyleOptions::default(),
            target: FormatTarget::Stdout {
                fname: Target::File("infile".into()),
            },
            output: OutputTarget::Stdout,
        };
        let mut expected = (expected_opt, expected_cmd);

        // All of the aliases should behave the same.
        assert_eq!(parse(&["rcl", "format", "infile"]), expected);
        assert_eq!(parse(&["rcl", "fmt", "infile"]), expected);
        assert_eq!(parse(&["rcl", "f", "infile"]), expected);

        // Without --in-place, we can do only one arg.
        assert_eq!(
            fail_parse(&["rcl", "f", "f1", "f2"]),
            "Error: Too many input files. See --help for usage.\n",
        );

        if let Cmd::Format { ref mut target, .. } = &mut expected.1 {
            *target = FormatTarget::InPlace {
                fnames: vec![Target::File("f1".into()), Target::File("f2".into())],
            };
        }
        assert_eq!(parse(&["rcl", "f", "--in-place", "f1", "f2"]), expected);
        assert_eq!(parse(&["rcl", "-i", "f", "f1", "f2"]), expected);

        if let Cmd::Format { ref mut target, .. } = &mut expected.1 {
            *target = FormatTarget::Check {
                fnames: vec![Target::File("f1".into()), Target::File("f2".into())],
            };
        }
        assert_eq!(parse(&["rcl", "f", "--check", "f1", "f2"]), expected);
    }

    #[test]
    fn parse_cmd_help_version() {
        assert!(matches!(parse(&["rcl", "--help"]).1, Cmd::Help { .. }));
        assert!(matches!(parse(&["rcl", "--version"]).1, Cmd::Version));
        assert!(matches!(parse(&["rcl", "eval", "-h"]).1, Cmd::Help { .. }));
        assert!(matches!(
            parse(&["rcl", "format", "-h"]).1,
            Cmd::Help { .. }
        ));
        assert!(matches!(
            parse(&["rcl", "highlight", "-h"]).1,
            Cmd::Help { .. }
        ));
        assert!(matches!(parse(&["rcl", "query", "-h"]).1, Cmd::Help { .. }));
        // Missing subcommand also triggers help.
        assert!(matches!(parse(&["rcl"]).1, Cmd::Help { .. }));
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
        assert_eq!(parse(&["rcl", "highlight", "infile"]), expected);
    }

    #[test]
    fn parse_cmd_query() {
        let expected_opt = GlobalOptions {
            markup: None,
            workdir: None,
        };
        let expected_cmd = Cmd::Query {
            eval_opts: EvalOptions::default(),
            style_opts: StyleOptions::default(),
            fname: Target::File("infile".into()),
            query: "input.name".to_string(),
            output: OutputTarget::Stdout,
        };
        let mut expected = (expected_opt, expected_cmd);
        assert_eq!(parse(&["rcl", "query", "infile", "input.name"]), expected);
        assert_eq!(parse(&["rcl", "q", "infile", "input.name"]), expected);

        if let Cmd::Query { eval_opts, .. } = &mut expected.1 {
            eval_opts.format = OutputFormat::Json
        };
        assert_eq!(parse(&["rcl", "jq", "infile", "input.name"]), expected);

        if let Cmd::Query {
            eval_opts,
            fname,
            query,
            ..
        } = &mut expected.1
        {
            eval_opts.format = OutputFormat::Rcl;
            *fname = Target::StdinDefault;
            *query = "infile".to_string();
        };
        assert_eq!(parse(&["rcl", "q", "infile"]), expected);
    }

    #[test]
    fn parse_cmd_handles_stdin_and_double_dash() {
        assert_eq!(
            parse(&["rcl", "highlight", "infile"]).1,
            Cmd::Highlight {
                fname: Target::File("infile".into()),
            }
        );
        assert_eq!(
            parse(&["rcl", "highlight", "--", "infile"]).1,
            Cmd::Highlight {
                fname: Target::File("infile".into()),
            }
        );
        assert_eq!(
            parse(&["rcl", "highlight", "-"]).1,
            Cmd::Highlight {
                fname: Target::Stdin,
            }
        );
        assert_eq!(
            parse(&["rcl", "highlight", "--", "-"]).1,
            Cmd::Highlight {
                fname: Target::File("-".into()),
            }
        );
    }
}
