// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Types that represent a parsed command line, and functions to parse it.

use std::str::FromStr;

use crate::cli_utils::{match_option, parse_option, Arg, ArgIter};
use crate::cmd_build::BuildMode;
use crate::error::{Error, Result};
use crate::loader::SandboxMode;
use crate::markup::{Markup, MarkupMode};
use crate::pprint::{concat, Doc};

// The help text for the main program is split up into two parts, so we can show
// a less cluttered version when no help is explicitly requested.

const USAGE_MAIN_INTRO: &str = r#"
RCL -- A reasonable configuration language.

Usage:
  rcl [<options>] <command> <arguments>

Commands:
  build        Write formatted evaluation results to files.
  evaluate     Evaluate a document to an output format.
  format       Auto-format an RCL document.
  highlight    Print a document with syntax highlighting.
  patch        Replace a value inside an RCL document.
  query        Evaluate an expression against an input document.
"#;

const USAGE_MAIN_EXTENDED: &str = r#"
Command shorthands:
  e, eval      Alias for 'evaluate'.
  f, fmt       Alias for 'format'.
  h            Alias for 'highlight'.
  je, jq       Alias for 'eval' and 'query' respectively with '--format=json'.
  re, rq       Alias for 'eval' and 'query' respectively with '--format=raw'.
  q            Alias for 'query'.

Global options:
  --about               Print license and dependency information.
  --color <mode>        Set how output is colored, see modes below.
  -C --directory <dir>  Change the working directory.
  -h --help             Show this screen, or command-specific help.
  --version             Show version.

Color modes:
  ansi    Always color output using ANSI escape codes.
  auto    Use ANSI if the output file is a TTY and the NO_COLOR environment
          variable is not set to a non-empty string. This is the default.
  html    Output HTML tags in the same style as Pandoc.
  none    Do not color output at all.

For more comprehensive documentation about the language itself, see also
<https://docs.ruuda.nl/rcl/>.
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
  --check           Report whether files would be created or rewritten. If so,
                    exit with exit code 1. When all target files are already up
                    to date, exit with exit code 0.
  --dry-run         Print what files we would write to stdout, instead of
                    writing to the file system, which would overwrite existing
                    files.
  --sandbox <mode>  Sandboxing mode, see 'rcl evaluate --help' for an
                    explanation of the modes. Defaults to 'workdir'.

See also --help for global options.

Example build file:

  {
    "alice.toml": {
      contents = { name = "Alice", uid = 42 },
      format = "toml",
      banner = "# This file is generated from build.rcl.",
    },
    "bob.toml": {
      contents = { name = "Bob", uid = 43 },
      format = "toml",
    },
  }

Build target fields:

  banner: Union[    A string to prepend to the output file. For example, a
    String,         comment to clarify that the file is generated. Defaults to
    Null,           null, which means no banner. Corresponds to 'rcl evaluate
  ]                 --banner'.

  contents: Any     The value to format and write to the output file.

  format: String    The output format, must be one of the formats supported by
                    'rcl evaluate --format', see 'rcl evaluate --help'.

  width: Number     Target width for formatting, as for 'rcl evaluate --width'.
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
  json-lines    If the document is a list, output every element as a JSON value
                on its own line. Top-level values other than lists are not valid
                for this format.
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
                   or --check are used, there can be multiple input files.
                   Defaults to stdin when no file is specified.

Options:
  --check                Report whether files would be reformatted. If so, exit
                         with exit code 1. When all files are already formatted
                         correctly, exit with exit code 0.
  -i --in-place          Rewrite files in-place instead of writing to stdout.
                         By default the formatted result is written to stdout.
  -o --output <outfile>  Write to the given file instead of stdout. This is
                         incompatible with --in-place.
  -w --width <width>     Target width in number of columns, must be an integer.
                         Defaults to 80.

See also --help for global options.
"#;

const USAGE_PATCH: &str = r#"
RCL -- A reasonable configuration language.

Usage:
  rcl [<options>] patch [<options>] [<file>] <path> <replacement>

The 'patch' command edits an RCL document, to replace the expression identified
by <path> with the given <replacement>. This can be used to make automation
edit a configuration that is otherwise written by hand. This command formats
the new document in standard style.

As an example, consider the file 'example.rcl':

  let widget = { id = 1337 };
  widget

The command 'rcl patch --in-place example.rcl widget.id 42' would rewrite it to:

  let widget = { id = 42 };
  widget

Arguments:
  <file>         The input file to process, or '-' for stdin. Defaults to stdin
                 when no file is specified.
  <path>         The path inside the document that identifies the syntax node to
                 replace. The path consists of identifiers separated by dots,
                 where each identifier matches either a let-binding, or a key in
                 a dict. Keys only match in record notation.
  <replacement>  The expression to replace the previous value with.

Options:
  --check                Report whether the file would be altered. If so, exit
                         with exit code 1. When the file already contains the
                         desired contents, exit with exit code 0. Formatting
                         changes unrelated to the patch will also cause an exit
                         code of 1.
  -i --in-place          Rewrite file in-place instead of writing to stdout.
                         By default the patched result is written to stdout.
  -o --output <outfile>  Write to the given file instead of stdout. This is
                         incompatible with --in-place.
  -w --width <width>     Target width in number of columns, must be an integer.
                         Defaults to 80.

See also --help for global options.
"#;

const USAGE_ABOUT: &str = r#"
RCL -- A reasonable configuration language.
Copyright 2025 Ruud van Asseldonk and contributors
Licensed under the Apache 2.0 license <https://www.apache.org/licenses/>.

This program incorporates the following Apache 2.0-licensed library:
  unicode-width <https://github.com/unicode-rs/unicode-width>
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
    JsonLines,
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
        build_mode: BuildMode,
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
    Patch {
        style_opts: StyleOptions,
        target: FormatTarget,
        output: OutputTarget,
        path: String,
        replacement: String,
    },
    Highlight {
        fname: Target,
    },
    Help {
        usage: &'static [&'static str],
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
    let mut build_mode = BuildMode::WriteFilesystem;

    while let Some(arg) = args.next() {
        match arg.as_ref() {
            Arg::Long("about") => cmd_help = Some("about"),
            Arg::Long("banner") => {
                eval_opts.banner = parse_option! {
                    args: arg,
                    |x: &str| Ok::<_, std::convert::Infallible>(Some(x.to_string()))
                };
            }
            Arg::Long("check") => {
                check = true;
                build_mode = BuildMode::Check;
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
            Arg::Long("dry-run") => {
                build_mode = BuildMode::DryRun;
            }
            Arg::Long("format") | Arg::Short("f") => {
                eval_opts.format = match_option! {
                    args: arg,
                    "json" => OutputFormat::Json,
                    "json-lines" => OutputFormat::JsonLines,
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
            Arg::Plain("re") if cmd.is_none() => {
                cmd = Some("evaluate");
                eval_opts.format = OutputFormat::Raw;
            }
            Arg::Plain("query") | Arg::Plain("q") if cmd.is_none() => {
                cmd = Some("query");
            }
            Arg::Plain("jq") if cmd.is_none() => {
                cmd = Some("query");
                eval_opts.format = OutputFormat::Json;
            }
            Arg::Plain("rq") if cmd.is_none() => {
                cmd = Some("query");
                eval_opts.format = OutputFormat::Raw;
            }
            Arg::Plain("format") | Arg::Plain("fmt") | Arg::Plain("f") if cmd.is_none() => {
                cmd = Some("format");
            }
            Arg::Plain("patch") if cmd.is_none() => {
                cmd = Some("patch");
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
        Some("about") => Some(Cmd::Help {
            usage: &[USAGE_ABOUT],
        }),
        Some("build") => Some(Cmd::Help {
            usage: &[USAGE_BUILD],
        }),
        Some("evaluate") => Some(Cmd::Help {
            usage: &[USAGE_EVAL_QUERY],
        }),
        Some("format") => Some(Cmd::Help {
            usage: &[USAGE_FORMAT],
        }),
        // TODO: Add usage for highlight.
        Some("highlight") => Some(Cmd::Help {
            usage: &[USAGE_MAIN_INTRO, USAGE_MAIN_EXTENDED],
        }),
        Some("main") => Some(Cmd::Help {
            usage: &[USAGE_MAIN_INTRO, USAGE_MAIN_EXTENDED],
        }),
        Some("patch") => Some(Cmd::Help {
            usage: &[USAGE_PATCH],
        }),
        Some("query") => Some(Cmd::Help {
            usage: &[USAGE_EVAL_QUERY],
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
                build_mode,
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
            target: get_format_target(in_place, check, targets)?,
            output,
        },
        Some("patch") => {
            let mut pop_arg = || match targets.pop() {
                Some(Target::File(expr)) => Ok(expr),
                Some(Target::Stdin) => Ok("-".to_string()),
                Some(Target::StdinDefault) => unreachable!("Produced only through absence of arg."),
                None => Error::new("Expected a path and replacement. See --help for usage.").err(),
            };
            let replacement = pop_arg()?;
            let path = pop_arg()?;
            Cmd::Patch {
                style_opts,
                target: get_format_target(in_place, check, targets)?,
                path,
                replacement,
                output,
            }
        }
        Some("highlight") => Cmd::Highlight {
            fname: get_unique_target(targets)?,
        },
        None => Cmd::Help {
            usage: &[USAGE_MAIN_INTRO, "\nSee --help for more info."],
        },
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

fn get_format_target(in_place: bool, check: bool, targets: Vec<Target>) -> Result<FormatTarget> {
    let result = if in_place {
        FormatTarget::InPlace { fnames: targets }
    } else if check {
        FormatTarget::Check { fnames: targets }
    } else {
        FormatTarget::Stdout {
            fname: get_unique_target(targets)?,
        }
    };
    Ok(result)
}

#[cfg(test)]
mod test {
    use crate::cli::{
        Cmd, EvalOptions, FormatTarget, GlobalOptions, OutputFormat, OutputTarget, SandboxMode,
        StyleOptions, Target,
    };
    use crate::cmd_build::BuildMode;
    use crate::markup::MarkupMode;
    use crate::pprint::Config;

    fn fail_parse(args: &[&'static str]) -> String {
        let args_vec: Vec<_> = args.iter().map(|a| a.to_string()).collect();
        let err = super::parse(args_vec).err().unwrap();
        let cfg = Config { width: Some(80) };
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
        assert_eq!(parse(&["rcl", "re", "infile"]), expected);

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

        // Test --output-depfile
        if let Cmd::Evaluate { eval_opts, .. } = &mut expected.1 {
            eval_opts.banner = None;
            eval_opts.output_depfile = Some("deps".to_string());
        }
        assert_eq!(
            parse(&["rcl", "e", "infile", "--output-depfile=deps"]),
            expected
        );

        // Test that defaulting to stdin works. If '-' is there we get it
        // explicitly, if it's not, we get it implicitly.
        if let Cmd::Evaluate {
            fname, eval_opts, ..
        } = &mut expected.1
        {
            eval_opts.output_depfile = None;
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
            "Error: Expected --format to be followed by one of json, json-lines, raw, rcl, toml, yaml-stream. See --help for usage.\n"
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
    fn parse_cmd_help_about_version() {
        assert!(matches!(parse(&["rcl", "--help"]).1, Cmd::Help { .. }));
        assert!(matches!(parse(&["rcl", "--version"]).1, Cmd::Version));
        assert!(matches!(parse(&["rcl", "--about"]).1, Cmd::Help { .. }));
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

        if let Cmd::Query { eval_opts, .. } = &mut expected.1 {
            eval_opts.format = OutputFormat::Raw
        };
        assert_eq!(parse(&["rcl", "rq", "infile", "input.name"]), expected);

        // With one argument, the arg is the query, not the file.
        if let Cmd::Query {
            eval_opts,
            fname,
            query,
            ..
        } = &mut expected.1
        {
            eval_opts.format = OutputFormat::Rcl;
            *fname = Target::StdinDefault;
            *query = "query".to_string();
        };
        assert_eq!(parse(&["rcl", "q", "query"]), expected);

        // A dash in query position is a query expression, not a file.
        if let Cmd::Query { query, .. } = &mut expected.1 {
            *query = "-".to_string();
        };
        assert_eq!(parse(&["rcl", "q", "-"]), expected);

        if let Cmd::Query { fname, .. } = &mut expected.1 {
            *fname = Target::File("infile".to_string());
        };
        assert_eq!(parse(&["rcl", "q", "infile", "-"]), expected);

        // If we omit the query, that's an error.
        assert_eq!(
            fail_parse(&["rcl", "query"]),
            "Error: Expected an input file and a query. See --help for usage.\n"
        );
    }

    #[test]
    fn parse_cmd_build() {
        let expected_opt = GlobalOptions {
            markup: None,
            workdir: None,
        };
        let expected_cmd = Cmd::Build {
            eval_opts: EvalOptions::default(),
            build_mode: BuildMode::WriteFilesystem,
            fname: Target::File("build.rcl".to_string()),
        };
        let mut expected = (expected_opt, expected_cmd);
        assert_eq!(parse(&["rcl", "build"]), expected);
        assert_eq!(parse(&["rcl", "build", "build.rcl"]), expected);

        if let Cmd::Build { fname, .. } = &mut expected.1 {
            *fname = Target::File("other.rcl".to_string());
        };
        assert_eq!(parse(&["rcl", "build", "other.rcl"]), expected);

        if let Cmd::Build { build_mode, .. } = &mut expected.1 {
            *build_mode = BuildMode::DryRun;
        };
        assert_eq!(parse(&["rcl", "build", "--dry-run", "other.rcl"]), expected);

        if let Cmd::Build { build_mode, .. } = &mut expected.1 {
            *build_mode = BuildMode::Check;
        };
        // The later --check takes precedence.
        assert_eq!(
            parse(&["rcl", "build", "--dry-run", "--check", "other.rcl"]),
            expected
        );
        assert_eq!(parse(&["rcl", "build", "--check", "other.rcl"]), expected);
    }

    #[test]
    fn parse_cmd_patch() {
        let expected_opt = GlobalOptions {
            markup: None,
            workdir: None,
        };
        let expected_cmd = Cmd::Patch {
            style_opts: StyleOptions::default(),
            target: FormatTarget::Stdout {
                fname: Target::File("infile".into()),
            },
            output: OutputTarget::Stdout,
            path: "path".to_string(),
            replacement: "replacement".to_string(),
        };
        let mut expected = (expected_opt, expected_cmd);
        assert_eq!(
            parse(&["rcl", "patch", "infile", "path", "replacement"]),
            expected
        );

        if let Cmd::Patch { output, .. } = &mut expected.1 {
            *output = OutputTarget::File("outfile".to_string());
        };
        // The --outfile can go anywhere; input, path, and replacement are positional.
        assert_eq!(
            parse(&[
                "rcl",
                "patch",
                "infile",
                "--output",
                "outfile",
                "path",
                "replacement"
            ]),
            expected
        );
        assert_eq!(
            parse(&[
                "rcl",
                "patch",
                "--output=outfile",
                "infile",
                "path",
                "replacement"
            ]),
            expected
        );
        assert_eq!(
            parse(&[
                "rcl",
                "patch",
                "infile",
                "path",
                "replacement",
                "--output",
                "outfile"
            ]),
            expected
        );

        if let Cmd::Patch { target, output, .. } = &mut expected.1 {
            *output = OutputTarget::Stdout;
            *target = FormatTarget::Stdout {
                fname: Target::Stdin,
            };
        };
        assert_eq!(
            parse(&["rcl", "patch", "-", "path", "replacement"]),
            expected
        );

        // A dash is a replacement expression, not a filename.
        if let Cmd::Patch { replacement, .. } = &mut expected.1 {
            *replacement = "-".to_string();
        };
        assert_eq!(parse(&["rcl", "patch", "-", "path", "-"]), expected);

        // If we omit one of the 3 positionals, the input file defaults to stdin.
        if let Cmd::Patch {
            target,
            replacement,
            ..
        } = &mut expected.1
        {
            *replacement = "replacement".to_string();
            *target = FormatTarget::Stdout {
                fname: Target::StdinDefault,
            };
        };
        assert_eq!(parse(&["rcl", "patch", "path", "replacement"]), expected);

        // If we omit more than one, that's an error.
        assert_eq!(
            fail_parse(&["rcl", "patch", "path"]),
            "Error: Expected a path and replacement. See --help for usage.\n"
        );
        assert_eq!(
            fail_parse(&["rcl", "patch"]),
            "Error: Expected a path and replacement. See --help for usage.\n"
        );

        // With --check and --in-place we control the target. In those cases,
        // we do not default to stdin. Technically it would work for --check,
        // but for --in-place it makes no sense.
        if let Cmd::Patch { target, .. } = &mut expected.1 {
            *target = FormatTarget::Check {
                fnames: vec![Target::Stdin],
            };
        };
        assert_eq!(
            parse(&["rcl", "patch", "--check", "-", "path", "replacement"]),
            expected
        );

        if let Cmd::Patch { target, .. } = &mut expected.1 {
            *target = FormatTarget::Check {
                fnames: vec![Target::File("infile".to_string())],
            };
        };
        assert_eq!(
            parse(&["rcl", "patch", "--check", "infile", "path", "replacement"]),
            expected
        );

        if let Cmd::Patch { target, .. } = &mut expected.1 {
            *target = FormatTarget::InPlace {
                fnames: vec![Target::File("infile".to_string())],
            };
        };
        assert_eq!(
            parse(&["rcl", "patch", "-i", "infile", "path", "replacement"]),
            expected
        );
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
