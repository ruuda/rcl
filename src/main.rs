// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::io::Write;
use std::path::Path;

use rcl::cli::{
    self, Cmd, EvalOptions, FormatTarget, GlobalOptions, OutputFormat, OutputTarget, StyleOptions,
};
use rcl::error::{Error, Result};
use rcl::loader::{Loader, SandboxMode};
use rcl::markup::{MarkupMode, MarkupString};
use rcl::pprint::{self, Doc};
use rcl::runtime::{self, Value};
use rcl::source::{DocId, Span};
use rcl::tracer::StderrTracer;
use rcl::typecheck;

struct App {
    loader: Loader,
    opts: GlobalOptions,
}

impl App {
    fn print_string(&self, mode: MarkupMode, data: MarkupString, out: &mut dyn Write) {
        let res = data.write_bytes(mode, out);
        if res.is_err() {
            // If we fail to print to stdout/stderr, there is no point in
            // printing an error, just exit then.
            std::process::exit(1);
        }
    }

    /// Write a string to a file.
    fn print_to_file_impl(
        &self,
        mode: MarkupMode,
        data: MarkupString,
        out_path: &Path,
    ) -> std::io::Result<()> {
        let f = std::fs::File::open(out_path)?;
        let mut w = std::io::BufWriter::new(f);
        data.write_bytes(mode, &mut w)
    }

    /// Write a string to a file.
    fn print_to_file(&self, mode: MarkupMode, data: MarkupString, out_path: &str) -> Result<()> {
        let out_path = self.loader.resolve_cli_output_path(out_path);

        self.print_to_file_impl(mode, data, out_path.as_ref())
            .map_err(|err| {
                // The concat! macro is not exported, we'll make do with a vec here.
                let parts = vec![
                    "Failed to write to file '".into(),
                    Doc::highlight(out_path.to_string_lossy().as_ref()).into_owned(),
                    "': ".into(),
                    err.to_string().into(),
                ];
                Error::new(Doc::Concat(parts)).into()
            })
    }

    fn print_doc_target(
        &self,
        output: OutputTarget,
        style_opts: &StyleOptions,
        doc: Doc,
    ) -> Result<()> {
        let stdout = std::io::stdout();
        let markup = match output {
            OutputTarget::Stdout => self
                .opts
                .markup
                .unwrap_or_else(|| MarkupMode::default_for_fd(&stdout)),
            // When the output is a file, we don't want to put ANSI escape codes
            // in the file; --output is unaffected by --color.
            OutputTarget::File(..) => MarkupMode::None,
        };
        let cfg = pprint::Config {
            width: style_opts.width,
        };
        let result = doc.println(&cfg);
        match output {
            OutputTarget::Stdout => {
                let mut out = stdout.lock();
                self.print_string(markup, result, &mut out);
            }
            OutputTarget::File(fname) => {
                self.print_to_file(markup, result, &fname)?;
            }
        };
        Ok(())
    }

    fn print_doc_stderr(&self, doc: Doc) {
        let stderr = std::io::stderr();
        let markup = self
            .opts
            .markup
            .unwrap_or_else(|| MarkupMode::default_for_fd(&stderr));
        let cfg = pprint::Config { width: 80 };
        let result = doc.println(&cfg);
        let mut out = stderr.lock();
        self.print_string(markup, result, &mut out);
    }

    pub fn print_value(
        &self,
        eval_opts: &EvalOptions,
        style_opts: &StyleOptions,
        output: OutputTarget,
        value_span: Span,
        value: &Value,
    ) -> Result<()> {
        let out_doc = match eval_opts.format {
            OutputFormat::Json => rcl::fmt_json::format_json(value_span, value)?,
            OutputFormat::Raw => rcl::fmt_raw::format_raw(value_span, value)?,
            OutputFormat::Rcl => rcl::fmt_rcl::format_rcl(value),
            OutputFormat::Toml => rcl::fmt_toml::format_toml(value_span, value)?,
            OutputFormat::YamlStream => {
                rcl::fmt_yaml_stream::format_yaml_stream(value_span, value)?
            }
        };
        self.print_doc_target(output, style_opts, out_doc)
    }

    fn print_fatal_error(&self, err: Error) -> ! {
        let inputs = self.loader.as_inputs();
        let err_doc = err.report(&inputs);
        self.print_doc_stderr(err_doc);
        // Regardless of whether printing to stderr failed or not, the error was
        // fatal, so we exit with code 1.
        std::process::exit(1);
    }

    fn get_tracer(&self) -> StderrTracer {
        StderrTracer::new(self.opts.markup)
    }

    fn main_fmt(&self, output: OutputTarget, style_opts: &StyleOptions, doc: DocId) -> Result<()> {
        let data = self.loader.get_doc(doc).data;
        let cst = self.loader.get_cst(doc)?;
        let res = rcl::fmt_cst::format_expr(data, &cst);
        self.print_doc_target(output, style_opts, res)
    }

    fn main(&mut self) -> Result<()> {
        let (opts, cmd) = cli::parse(std::env::args().collect())?;
        self.opts = opts;

        match cmd {
            Cmd::Help { usage } => {
                println!("{}", usage.trim());
                std::process::exit(0)
            }

            Cmd::Evaluate {
                eval_opts,
                style_opts,
                fname,
                output,
            } => {
                self.loader
                    .initialize_filesystem(eval_opts.sandbox, self.opts.workdir.as_deref())?;

                let mut tracer = self.get_tracer();
                let mut type_env = typecheck::prelude();
                let mut value_env = runtime::prelude();
                let doc = self.loader.load_cli_target(fname)?;
                let val = self
                    .loader
                    .evaluate(&mut type_env, &mut value_env, doc, &mut tracer)?;

                if let Some(depfile_path) = eval_opts.output_depfile.as_ref() {
                    self.loader.write_depfile(&output, depfile_path)?;
                }
                // TODO: Need to get last inner span.
                let full_span = self.loader.get_span(doc);
                self.print_value(&eval_opts, &style_opts, output, full_span, &val)
            }

            Cmd::Query {
                eval_opts,
                style_opts,
                fname,
                query: expr,
                output,
            } => {
                self.loader
                    .initialize_filesystem(eval_opts.sandbox, self.opts.workdir.as_deref())?;

                let input = self.loader.load_cli_target(fname)?;
                let query = self.loader.load_string(expr);

                // First we evaluate the input document.
                let mut tracer = self.get_tracer();
                let mut type_env = typecheck::prelude();
                let mut value_env = runtime::prelude();
                let val_input =
                    self.loader
                        .evaluate(&mut type_env, &mut value_env, input, &mut tracer)?;

                // Then we bind that to the variable `input`, and in that context,
                // we evaluate the query expression. The environments should be
                // clean at this point, so we can reuse them.
                type_env.push("input".into(), typecheck::type_any().clone());
                value_env.push("input".into(), val_input);
                let val_result =
                    self.loader
                        .evaluate(&mut type_env, &mut value_env, query, &mut tracer)?;

                if let Some(depfile_path) = eval_opts.output_depfile.as_ref() {
                    self.loader.write_depfile(&output, depfile_path)?;
                }

                let full_span = self.loader.get_span(query);
                self.print_value(&eval_opts, &style_opts, output, full_span, &val_result)
            }

            Cmd::Format {
                style_opts,
                target,
                output,
            } => match target {
                FormatTarget::InPlace { fnames: _ } => {
                    todo!("TODO: --in-place formatting is not yet implemented.");
                }
                FormatTarget::Stdout { fname } => {
                    self.loader.initialize_filesystem(
                        SandboxMode::Unrestricted,
                        self.opts.workdir.as_deref(),
                    )?;
                    let doc = self.loader.load_cli_target(fname)?;
                    self.main_fmt(output, &style_opts, doc)
                }
            },

            Cmd::Highlight { fname } => {
                self.loader.initialize_filesystem(
                    SandboxMode::Unrestricted,
                    self.opts.workdir.as_deref(),
                )?;
                let doc = self.loader.load_cli_target(fname)?;
                let tokens = self.loader.get_tokens(doc)?;
                let data = self.loader.get_doc(doc).data;
                let mut stdout = std::io::stdout().lock();
                // TODO: Make this based on the pretty-printer.
                let res = rcl::highlight::highlight(&mut stdout, &tokens, data);
                if res.is_err() {
                    // If we fail to print to stdout, there is no point in printing
                    // an error, just exit then.
                    std::process::exit(1);
                }
                Ok(())
            }

            Cmd::Version => {
                println!("RCL version {}", env!("CARGO_PKG_VERSION"));
                Ok(())
            }
        }
    }
}

fn main() {
    let mut app = App {
        opts: GlobalOptions::default(),
        loader: Loader::new(),
    };

    if let Err(err) = app.main() {
        app.print_fatal_error(*err);
    }
}
