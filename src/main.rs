// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::io::Stdout;

use rcl::cli::{self, Cmd, FormatTarget, GlobalOptions, OutputFormat, OutputOptions};
use rcl::error::Result;
use rcl::loader::Loader;
use rcl::pprint;
use rcl::runtime::Env;
use rcl::source::DocId;

/// Pretty-print a document to stdout.
fn pprint_stdout(stdout: Stdout, cfg: &pprint::Config, doc: &pprint::Doc) {
    use std::io::Write;
    let result = doc.println(cfg);
    let mut out = stdout.lock();
    let res = out.write_all(result.as_bytes());
    if res.is_err() {
        // If we fail to print to stdout, there is no point in printing
        // an error, just exit then.
        std::process::exit(1);
    }
}

fn main_eval(
    loader: &Loader,
    global_opts: &GlobalOptions,
    doc: DocId,
    opts: OutputOptions,
) -> Result<()> {
    let mut env = Env::new();
    let val = loader.evaluate(doc, &mut env)?;

    let full_span = loader.get_span(doc);
    let out_doc = match opts.format {
        OutputFormat::Rcl => rcl::fmt_rcl::format_rcl(val.as_ref()),
        OutputFormat::Json => rcl::fmt_json::format_json(full_span, val.as_ref())?,
    };
    let stdout = std::io::stdout();
    let mut cfg = pprint::Config::default_for_fd(&stdout);
    global_opts.apply(&mut cfg);
    pprint_stdout(stdout, &cfg, &out_doc);
    Ok(())
}

fn main_query(loader: &Loader, input: DocId, query: DocId) -> Result<()> {
    // First we evaluate the input document.
    let mut env = Env::new();
    let val_input = loader.evaluate(input, &mut env)?;

    // Then we bind that to the variable `input`, and in that context, we
    // evaluate the query expression.
    let mut env = Env::new();
    env.push("input".into(), val_input);
    let val_result = loader.evaluate(query, &mut env)?;

    let full_span = loader.get_span(query);
    let json = rcl::fmt_json::format_json(full_span, val_result.as_ref())?;
    let stdout = std::io::stdout();
    let cfg = pprint::Config::default_for_fd(&stdout);
    pprint_stdout(stdout, &cfg, &json);
    Ok(())
}

fn main_fmt(loader: &Loader, doc: DocId) -> Result<()> {
    let data = loader.get_doc(doc).data;
    let cst = loader.get_cst(doc)?;
    let res = rcl::fmt_cst::format_expr(data, &cst);
    let stdout = std::io::stdout();
    let cfg = pprint::Config::default_for_fd(&stdout);
    pprint_stdout(stdout, &cfg, &res);
    Ok(())
}

fn main_highlight(loader: &Loader, doc: DocId) -> Result<()> {
    let tokens = loader.get_tokens(doc)?;
    let data = loader.get_doc(doc).data;
    let mut stdout = std::io::stdout().lock();
    let res = rcl::highlight::highlight(&mut stdout, &tokens, data);
    if res.is_err() {
        // If we fail to print to stdout, there is no point in printing
        // an error, just exit then.
        std::process::exit(1);
    }
    Ok(())
}

fn main_with_loader(loader: &mut Loader, global_opts: &mut GlobalOptions) -> Result<()> {
    let (opts, cmd) = cli::parse(std::env::args().collect())?;
    *global_opts = opts;

    match cmd {
        Cmd::Help { usage } => {
            println!("{}", usage.trim());
            std::process::exit(0)
        }
        Cmd::Evaluate {
            fname, output_opts, ..
        } => {
            let doc = fname.load(loader)?;
            main_eval(loader, global_opts, doc, output_opts)
        }
        Cmd::Query {
            fname, query: expr, ..
        } => {
            let input = fname.load(loader)?;
            let query = loader.load_string(expr);
            main_query(loader, input, query)
        }
        Cmd::Format { target, .. } => match target {
            FormatTarget::InPlace { fnames: _ } => {
                todo!("TODO: Handle --in-place.");
            }
            FormatTarget::Stdout { fname } => {
                let doc = fname.load(loader)?;
                main_fmt(loader, doc)
            }
        },
        Cmd::Highlight { fname } => {
            let doc = fname.load(loader)?;
            main_highlight(loader, doc)
        }
        Cmd::Version => {
            todo!("Add version metadata.");
        }
    }
}

fn main() {
    use std::io::Write;
    let mut opts = GlobalOptions::default();
    let mut loader = Loader::new();

    if let Err(err) = main_with_loader(&mut loader, &mut opts) {
        let inputs = loader.as_inputs();
        let err_doc = err.report(&inputs);
        let stderr = std::io::stderr();
        let mut cfg = pprint::Config::default_for_fd(&stderr);
        opts.apply(&mut cfg);
        let err_string = err_doc.println(&cfg);
        let res = stderr.lock().write_all(err_string.as_bytes());
        // Regardless of whether printing to stderr failed or not, we were going
        // to exist with exit code 1 anyway, so ignore any stderr IO errors.
        let _ = res;
        std::process::exit(1);
    }
}
