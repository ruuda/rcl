// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Types that represent a parsed command line.

use crate::error::Result;
use crate::loader::Loader;
use crate::markup::MarkupMode;
use crate::pprint;
use crate::source::DocId;

/// How to format output of the
#[derive(Debug, Default, Eq, PartialEq)]
pub struct GlobalOptions {
    /// Whether and how to output color and other markup.
    ///
    /// We call it “markup” internally because it's more than just color, but
    /// we call it `--color` on the command line because that is what most tools
    /// call it.
    pub markup: Option<MarkupMode>,
}

impl GlobalOptions {
    /// Overwrite the relevant settings on the pprint config.
    pub fn apply(&self, cfg: &mut pprint::Config) {
        cfg.markup = self.markup.unwrap_or(cfg.markup);
    }
}

/// The available output formats (JSON, RCL).
#[derive(Debug, Eq, PartialEq)]
pub enum OutputFormat {
    Json,
    Rcl,
}

/// Options for commands that output values.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct OutputOptions {
    /// The format to output in.
    pub format: Option<OutputFormat>,
}

/// Options for commands that pretty-print their output.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct FormatOptions {
    /// Target width (number of columns) to try to not exceed.
    pub width: Option<u32>,
}

impl FormatOptions {
    /// Overwrite the relevant settings on the pprint config.
    pub fn apply(&self, cfg: &mut pprint::Config) {
        cfg.width = self.width.unwrap_or(cfg.width);
    }
}

/// Input to act on.
pub enum Target {
    File(String),
    Stdin,
}

impl Target {
    /// Load the input into the loader.
    pub fn load(&self, loader: &mut Loader) -> Result<DocId> {
        match self {
            Target::File(path) => loader.load_file(path),
            Target::Stdin => loader.load_stdin(),
        }
    }
}

/// For the `fmt` command, which documents to format, and in what mode.
pub enum FormatTarget {
    Stdout { fname: Target },
    InPlace { fnames: Vec<Target> },
}

/// The different subcommands supported by the main program.
pub enum Cmd {
    Evaluate {
        output_opts: OutputOptions,
        format_opts: FormatOptions,
        fname: Target,
    },
    Query {
        output_opts: OutputOptions,
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
