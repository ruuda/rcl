// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::io::{Stdout, Write};
use std::path::Path;

use rcl::cli::{
    self, Cmd, EvalOptions, FormatTarget, GlobalOptions, OutputTarget, StyleOptions, Target,
};
use rcl::error::{Error, Result};
use rcl::loader::{Loader, SandboxMode};
use rcl::markup::{MarkupMode, MarkupString};
use rcl::pprint::{self, Doc};
use rcl::runtime::{self, Value};
use rcl::source::{DocId, Inputs, Span};
use rcl::tracer::StderrTracer;
use rcl::typecheck;

/// The result of [`App::process_format_targets`].
struct FormatResult {
    /// Whether we printed to stdout.
    stdout: bool,
    /// Whether we updated files in place.
    in_place: bool,
    /// Number of files changed, or that would change.
    n_changed: u32,
    /// Number of files inspected.
    n_loaded: u32,
}

struct App {
    loader: Loader,
    opts: GlobalOptions,
}

impl App {
    fn initialize_filesystem(&mut self, sandbox_mode: SandboxMode) -> Result<()> {
        self.loader
            .initialize_filesystem(sandbox_mode, self.opts.workdir.as_deref())
    }

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
        let f = std::fs::File::create(out_path)?;
        let mut w = std::io::BufWriter::new(f);
        data.write_bytes(mode, &mut w)?;
        // Flush to force errors to materialize. Without this, flush would
        // happen on drop, which has no opportunity to report errors.
        w.flush()
    }

    /// Write a string to a file.
    fn print_to_file(&self, mode: MarkupMode, data: MarkupString, out_path: &str) -> Result<()> {
        let out_path = self.loader.resolve_cli_output_path(out_path);

        self.print_to_file_impl(mode, data, out_path.as_ref())
            .map_err(|err| {
                // The concat! macro is not exported, we'll make do with a vec here.
                let parts = vec![
                    "Failed to write to file '".into(),
                    Doc::path(out_path),
                    "': ".into(),
                    err.to_string().into(),
                ];
                Error::new(Doc::Concat(parts)).into()
            })
    }

    fn get_markup_for_target(&self, target: &OutputTarget, stdout: &Stdout) -> MarkupMode {
        match target {
            OutputTarget::Stdout => self
                .opts
                .markup
                .unwrap_or_else(|| MarkupMode::default_for_fd(stdout)),
            // When the output is a file, we don't want to put ANSI escape codes
            // in the file; --output is unaffected by --color.
            OutputTarget::File(..) => MarkupMode::None,
        }
    }

    fn print_doc_target(
        &self,
        output: OutputTarget,
        style_opts: &StyleOptions,
        doc: Doc,
    ) -> Result<()> {
        let stdout = std::io::stdout();
        let markup = self.get_markup_for_target(&output, &stdout);
        let cfg = pprint::Config {
            width: Some(style_opts.width),
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
        let cfg = pprint::Config { width: Some(80) };
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
        let out_doc = rcl::cmd_eval::format_value(eval_opts.format, value_span, value)?;

        // Prepend the banner if the user specified one.
        let out_doc = match eval_opts.banner.as_ref() {
            Some(banner) => Doc::lines(banner) + Doc::HardBreak + out_doc,
            None => out_doc,
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

    /// Load all targets, apply `apply_one` to each, and write each output if applicable.
    fn process_format_targets<F>(
        &mut self,
        output: OutputTarget,
        style_opts: StyleOptions,
        targets: FormatTarget,
        mut apply_one: F,
    ) -> Result<FormatResult>
    where
        F: for<'a> FnMut(&'a Inputs, DocId, &'a mut rcl::cst::Expr) -> Result<Doc<'a>>,
    {
        let cfg = pprint::Config {
            width: Some(style_opts.width),
        };

        let mut result = FormatResult {
            stdout: false,
            in_place: false,
            n_changed: 0,
            n_loaded: 0,
        };

        let fnames = match targets {
            FormatTarget::Stdout { fname } => {
                let doc = self.loader.load_cli_target(&fname)?;
                let mut cst = self.loader.get_cst(doc)?;
                let inputs = self.loader.as_inputs();
                let res = apply_one(&inputs, doc, &mut cst)?;
                self.print_doc_target(output, &style_opts, res)?;
                result.stdout = true;
                return Ok(result);
            }
            FormatTarget::InPlace { fnames } => {
                result.in_place = true;
                fnames
            }
            FormatTarget::Check { mut fnames } => {
                // For in-place formatting we really need files, but for checking,
                // we can check stdin if the user did not specify any files.
                if fnames.is_empty() {
                    fnames.push(Target::StdinDefault);
                }
                fnames
            }
        };

        for target in fnames {
            result.n_loaded += 1;
            let doc = self.loader.load_cli_target(&target)?;
            let mut cst = self.loader.get_cst(doc)?;
            let inputs = self.loader.as_inputs();
            let fmt_doc = apply_one(&inputs, doc, &mut cst)?;
            let res = fmt_doc.println(&cfg);
            let formatted = res.to_string_no_markup();
            let did_change = self.loader.get_doc(doc).data != &formatted[..];

            if result.in_place {
                let fname = match target {
                    Target::File(fname) => fname,
                    Target::Stdin => {
                        let msg =
                            "Rewriting in-place is only possible for named files, not for stdin.";
                        return Error::new(msg).err();
                    }
                    Target::StdinDefault => {
                        unreachable!("In-place default is empty list, not stdin.")
                    }
                };
                // We only write to the file if we changed anything. This ensures
                // that we don't cause rebuilds for build systems that look at mtimes,
                // that we don't waste space on CoW filesystems, and that we don't
                // unnecessarily burn through SSDs in general.
                if did_change {
                    result.n_changed += 1;
                    self.print_to_file(MarkupMode::None, res, &fname)?;
                }
            } else {
                // We are in the --check case, not the --in-place case.
                if did_change {
                    result.n_changed += 1;
                    println!("Would modify {}", self.loader.get_doc(doc).name);
                }
            }
        }

        Ok(result)
    }

    fn main(&mut self) -> Result<()> {
        let (opts, cmd) = cli::parse(std::env::args().collect())?;
        self.opts = opts;

        match cmd {
            Cmd::Help { usage } => {
                println!("{}", usage.concat().trim());
                std::process::exit(0)
            }

            Cmd::Build {
                eval_opts,
                build_mode,
                fname,
            } => {
                // Evaluation options support a depfile, but this is not implemented
                // for builds, we'd have to put multiple output filenames in there
                // and that is not supported right now.
                if eval_opts.output_depfile.is_some() {
                    return Error::new("Generating depfiles is not supported for 'rcl build'.")
                        .err();
                }

                self.initialize_filesystem(eval_opts.sandbox)?;

                // TODO: We can make these members, then we can share a lot of code between commands!
                let mut tracer = self.get_tracer();
                let mut type_env = typecheck::prelude();
                let mut value_env = runtime::prelude();
                let doc = self.loader.load_cli_target(&fname)?;

                // TODO: Would be nice to be able to feed in an expected type.
                let val = self
                    .loader
                    .evaluate(&mut type_env, &mut value_env, doc, &mut tracer)?;

                let full_span = self.loader.get_span(doc);

                rcl::cmd_build::execute_build(&self.loader, build_mode, doc, full_span, val)
            }

            Cmd::Evaluate {
                eval_opts,
                style_opts,
                fname,
                output,
            } => {
                self.initialize_filesystem(eval_opts.sandbox)?;

                let mut tracer = self.get_tracer();
                let mut type_env = typecheck::prelude();
                let mut value_env = runtime::prelude();
                let doc = self.loader.load_cli_target(&fname)?;
                let val = self
                    .loader
                    .evaluate(&mut type_env, &mut value_env, doc, &mut tracer)?;

                if let Some(depfile_path) = eval_opts.output_depfile.as_ref() {
                    self.loader.write_depfile(&output, depfile_path)?;
                }

                let body_span = self.loader.get_span(doc);
                self.print_value(&eval_opts, &style_opts, output, body_span, &val)
            }

            Cmd::Query {
                eval_opts,
                style_opts,
                fname,
                query: expr,
                output,
            } => {
                self.initialize_filesystem(eval_opts.sandbox)?;

                let input = self.loader.load_cli_target(&fname)?;
                let query = self.loader.load_string("query", expr);

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

                let body_span = self.loader.get_span(query);
                self.print_value(&eval_opts, &style_opts, output, body_span, &val_result)
            }

            Cmd::Format {
                style_opts,
                target,
                output,
            } => {
                // Unrestricted is safe, because `format` does not evaluate documents.
                self.initialize_filesystem(SandboxMode::Unrestricted)?;
                let stats = self.process_format_targets(
                    output,
                    style_opts,
                    target,
                    |inputs, _doc, cst| Ok(rcl::fmt_cst::format_expr(inputs, cst)),
                )?;
                match (stats.n_changed, stats.n_loaded) {
                    (_, _) if stats.stdout => { /* No stats to print, we printed the doc. */ }
                    (k, n) if stats.in_place => println!("Reformatted {k} of {n} files."),
                    (0, 1) => println!("The file is formatted correctly."),
                    (0, n) => println!("All {n} files are formatted correctly."),
                    _ => {
                        let parts = vec![
                            stats.n_changed.to_string().into(),
                            Doc::str(" of "),
                            stats.n_loaded.to_string().into(),
                            Doc::str(" files would be reformatted."),
                        ];
                        return Error::new(Doc::Concat(parts)).err();
                    }
                }
                Ok(())
            }

            Cmd::Patch {
                style_opts,
                target,
                output,
                path,
                replacement,
            } => {
                // Unrestricted is safe, because `patch` does not evaluate documents.
                self.initialize_filesystem(SandboxMode::Unrestricted)?;

                let mut patcher = Some(rcl::patch::Patcher::new(
                    &mut self.loader,
                    &path,
                    replacement,
                )?);

                let stats = self.process_format_targets(
                    output,
                    style_opts,
                    target,
                    |inputs, doc, input_cst| {
                        let input_doc = &inputs[doc];
                        let patcher = patcher.take().ok_or_else(|| {
                            Error::new("'rcl patch' handles only one target file.")
                        })?;
                        patcher.apply(input_doc.data, input_doc.span, input_cst)?;
                        Ok(rcl::fmt_cst::format_expr(inputs, input_cst))
                    },
                )?;
                match (stats.n_changed, stats.n_loaded) {
                    (_, _) if stats.stdout => { /* No stats to print, we printed the doc. */ }
                    (1, 1) if stats.in_place => println!("Patch applied successfully."),
                    (0, 1) => println!("The patch is a no-op."),
                    (1, 1) => return Error::new("File would be patched or reformatted.").err(),
                    _ => unreachable!("Patch patches at most 1 document."),
                }
                Ok(())
            }

            Cmd::Highlight { fname } => {
                // Unrestricted is safe, because `highlight` does not evaluate documents.
                self.initialize_filesystem(SandboxMode::Unrestricted)?;
                let doc = self.loader.load_cli_target(&fname)?;
                let tokens = self.loader.get_tokens(doc)?;
                let data = self.loader.get_doc(doc).data;
                let result = rcl::highlight::highlight(&tokens, data);
                let out = std::io::stdout();
                let markup = self.get_markup_for_target(&OutputTarget::Stdout, &out);
                self.print_string(markup, result, &mut out.lock());
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
