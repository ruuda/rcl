// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::io::Write;
use std::rc::Rc;

use rcl::cli::{self, Cmd, EvalOptions, FormatOptions, FormatTarget, GlobalOptions, OutputFormat};
use rcl::error::{Error, Result};
use rcl::loader::{Loader, SandboxMode};
use rcl::markup::MarkupMode;
use rcl::pprint;
use rcl::runtime::Env;
use rcl::runtime::Value;
use rcl::source::{DocId, Span};
use rcl::tracer::StderrTracer;

struct App {
    loader: Loader,
    opts: GlobalOptions,
}

impl App {
    fn print_string(&self, out: &mut dyn Write, data: String) {
        let res = out.write_all(data.as_bytes());
        if res.is_err() {
            // If we fail to print to stdout/stderr, there is no point in
            // printing an error, just exit then.
            std::process::exit(1);
        }
    }

    fn print_doc_stdout(&self, format_opts: &FormatOptions, doc: pprint::Doc) {
        let stdout = std::io::stdout();
        let cfg = pprint::Config {
            markup: self
                .opts
                .markup
                .unwrap_or_else(|| MarkupMode::default_for_fd(&stdout)),
            width: format_opts.width,
        };
        let result = doc.println(&cfg);
        let mut out = stdout.lock();
        self.print_string(&mut out, result);
    }

    fn print_doc_stderr(&self, doc: pprint::Doc) {
        let stderr = std::io::stderr();
        let cfg = pprint::Config {
            markup: self
                .opts
                .markup
                .unwrap_or_else(|| MarkupMode::default_for_fd(&stderr)),
            width: 80,
        };
        let result = doc.println(&cfg);
        let mut out = stderr.lock();
        self.print_string(&mut out, result);
    }

    pub fn print_value(
        &self,
        eval_opts: &EvalOptions,
        format_opts: &FormatOptions,
        value_span: Span,
        value: Rc<Value>,
    ) -> Result<()> {
        let out_doc = match eval_opts.format {
            OutputFormat::Rcl => rcl::fmt_rcl::format_rcl(value.as_ref()),
            OutputFormat::Json => rcl::fmt_json::format_json(value_span, value.as_ref())?,
        };
        self.print_doc_stdout(format_opts, out_doc);
        Ok(())
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
        StderrTracer::new(&self.opts)
    }

    fn main_fmt(&self, format_opts: &FormatOptions, doc: DocId) -> Result<()> {
        let data = self.loader.get_doc(doc).data;
        let cst = self.loader.get_cst(doc)?;
        let res = rcl::fmt_cst::format_expr(data, &cst);
        self.print_doc_stdout(format_opts, res);
        Ok(())
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
                format_opts,
                fname,
            } => {
                self.loader.initialize_filesystem(eval_opts.sandbox)?;

                let mut tracer = self.get_tracer();
                let mut env = Env::new();
                let doc = self.loader.load_cli_target(fname)?;
                let val = self.loader.evaluate(doc, &mut env, &mut tracer)?;
                // TODO: Need to get last inner span.
                let full_span = self.loader.get_span(doc);
                self.print_value(&eval_opts, &format_opts, full_span, val)
            }

            Cmd::Query {
                eval_opts,
                format_opts,
                fname,
                query: expr,
            } => {
                self.loader.initialize_filesystem(eval_opts.sandbox)?;

                let input = self.loader.load_cli_target(fname)?;
                let query = self.loader.load_string(expr);

                // First we evaluate the input document.
                let mut tracer = self.get_tracer();
                let mut env = Env::new();
                let val_input = self.loader.evaluate(input, &mut env, &mut tracer)?;

                // Then we bind that to the variable `input`, and in that context, we
                // evaluate the query expression.
                let mut env = Env::new();
                env.push("input".into(), val_input);
                let val_result = self.loader.evaluate(query, &mut env, &mut tracer)?;

                let full_span = self.loader.get_span(query);
                self.print_value(&eval_opts, &format_opts, full_span, val_result)
            }

            Cmd::Format {
                format_opts,
                target,
            } => match target {
                FormatTarget::InPlace { fnames: _ } => {
                    todo!("TODO: --in-place formatting is not yet implemented.");
                }
                FormatTarget::Stdout { fname } => {
                    self.loader
                        .initialize_filesystem(SandboxMode::Unrestricted)?;
                    let doc = self.loader.load_cli_target(fname)?;
                    self.main_fmt(&format_opts, doc)
                }
            },

            Cmd::Highlight { fname } => {
                self.loader
                    .initialize_filesystem(SandboxMode::Unrestricted)?;
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
