// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::io::Stdout;

use rcl::cli_command::{self, Cmd};
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

fn main_eval(loader: &Loader, doc: DocId) -> Result<()> {
    let mut env = Env::new();
    let val = loader.evaluate(doc, &mut env)?;

    let full_span = loader.get_span(doc);
    let json = rcl::fmt_json::format_json(full_span, val.as_ref())?;
    let stdout = std::io::stdout();
    let cfg = pprint::Config::default_for_fd(&stdout);
    pprint_stdout(stdout, &cfg, &json);
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

fn main_with_loader(loader: &mut Loader) -> Result<()> {
    let cmd = cli_command::parse(std::env::args().collect())?;

    match cmd {
        Cmd::Help { usage } => {
            println!("{}", usage);
            std::process::exit(0)
        }
        Cmd::Evaluate { fname, .. } => {
            let doc = loader.load_from_cli_fname(&fname)?;
            main_eval(loader, doc)
        }
        Cmd::Query {
            fname, query: expr, ..
        } => {
            let input = loader.load_from_cli_fname(&fname)?;
            let query = loader.load_string(expr);
            main_query(loader, input, query)
        }
        Cmd::Format { fnames, .. } => {
            // TODO: Handle --in-place and multiple fnames.
            let doc = loader.load_from_cli_fname(&fnames[0])?;
            main_fmt(loader, doc)
        }
        Cmd::Highlight { fname, .. } => {
            let doc = loader.load_from_cli_fname(&fname)?;
            main_highlight(loader, doc)
        }
        Cmd::Version => {
            todo!("Add version metadata.");
        }
    }
}

fn main() {
    use std::io::Write;
    let mut loader = Loader::new();

    if let Err(err) = main_with_loader(&mut loader) {
        let inputs = loader.as_inputs();
        let err_doc = err.report(&inputs);
        let stderr = std::io::stderr();
        let cfg = pprint::Config::default_for_fd(&stderr);
        let err_string = err_doc.println(&cfg);
        let res = stderr.lock().write_all(err_string.as_bytes());
        // Regardless of whether printing to stderr failed or not, we were going
        // to exist with exit code 1 anyway, so ignore any stderr IO errors.
        let _ = res;
        std::process::exit(1);
    }
}
