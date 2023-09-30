// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::io::Stdout;

use rcl::error::Result;
use rcl::loader::Loader;
use rcl::pprint;
use rcl::runtime::Env;
use rcl::source::DocId;

const USAGE: &str = r#"
RCL -- Ruud's Configuration Language.

Usage:
  rcl evaluate <file>
  rcl format <file>
  rcl highlight <file>
  rcl query <file> <expr>
  rcl repl
  rcl -h | --help

Arguments:
  <file>      The input file to process, or '-' for stdin.
  <expr>      An RCL expression to evaluate against the input document.

Options:
  -h --help   Show this screen.

See the manual for a more elaborate usage guide.
"#;

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
    let json = rcl::json::format_json(full_span, val.as_ref())?;
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
    let json = rcl::json::format_json(full_span, val_result.as_ref())?;
    let stdout = std::io::stdout();
    let cfg = pprint::Config::default_for_fd(&stdout);
    pprint_stdout(stdout, &cfg, &json);
    Ok(())
}

fn main_fmt(loader: &Loader, doc: DocId) -> Result<()> {
    let data = loader.get_doc(doc).data;
    let cst = loader.get_cst(doc)?;
    let res = rcl::fmt::format_expr(data, &cst);
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
    let args: Vec<String> = std::env::args().skip(1).collect();
    let args_refs: Vec<&str> = args.iter().map(|a| &a[..]).collect();

    match &args_refs[..] {
        ["-h"] | ["--help"] => {
            println!("{}", USAGE.trim());
            std::process::exit(0)
        }
        ["e", fname] | ["eval", fname] | ["evaluate", fname] => {
            let doc = loader.load_from_cli_fname(fname)?;
            main_eval(loader, doc)
        }
        ["f", fname] | ["fmt", fname] | ["format", fname] => {
            let doc = loader.load_from_cli_fname(fname)?;
            main_fmt(loader, doc)
        }
        ["highlight", fname] => {
            let doc = loader.load_from_cli_fname(fname)?;
            main_highlight(loader, doc)
        }
        ["repl"] => {
            unimplemented!("TODO: Implement repl.");
        }
        ["q", fname, expr] | ["query", fname, expr] => {
            let input = loader.load_from_cli_fname(fname)?;
            let query = loader.load_string(expr.to_string());
            main_query(loader, input, query)
        }
        _ => {
            eprintln!("Failed to parse command line. Run with --help for usage.");
            std::process::exit(1);
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
