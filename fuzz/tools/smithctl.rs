// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Smithctl is a utility for working with the smith fuzz corpus.

use std::io;

use rcl_fuzz::smith::SynthesizedProgram;

const USAGE: &str = "Usage:
    smithctl <command> <file...>

Commands:
    print         Pretty-print a fuzz input.
    eval-corpus   Evaluate every element in the given corpus directory.
                  When compiled with coverage instrumentation, this is useful
                  for measuring coverage.
";

fn main() {
    let mut args = std::env::args();
    // Skip the program name.
    args.next();
    let args: Vec<String> = args.collect();

    match args.first().map(|a| a.as_ref()) {
        Some("print") => cmd_print(&args[1..]).unwrap(),
        Some("eval-corpus") => cmd_eval_corpus(&args[1]).unwrap(),
        _ => print!("{USAGE}"),
    }
}

fn cmd_print(fnames: &[String]) -> io::Result<()> {
    for fname in fnames {
        println!("{fname}:");
        let bytecode = std::fs::read(fname)?;
        let program = SynthesizedProgram::new(&bytecode);
        println!("{program:?}");
    }
    Ok(())
}

fn cmd_eval_corpus(dir: &str) -> io::Result<()> {
    use std::io::Write;
    let mut path: std::path::PathBuf = dir.into();

    let mut stderr = io::stderr().lock();
    writeln!(stderr, "Running all samples in {path:?} ...")?;
    let mut print_status = |n_exec, n_skip| {
        write!(
            stderr,
            "\r{n_exec} samples executed, {n_skip} samples skipped."
        )
        .and_then(|()| stderr.flush())
    };

    let mut n_skip = 0_u32;
    let mut n_exec = 0_u32;

    for (i, entry_opt) in std::fs::read_dir(dir)?.enumerate() {
        let entry = entry_opt?;
        let is_file = entry.file_type()?.is_file();
        if is_file {
            path.push(entry.file_name());
            let bytecode = std::fs::read(&path)?;
            let program = SynthesizedProgram::new(&bytecode);
            if program.is_minimal {
                rcl_fuzz::uber::fuzz_main(program.mode, &program.program);
                n_exec += 1;
            } else {
                n_skip += 1;
            }
            path.pop();
            if i & 128 == 0 {
                print_status(n_exec, n_skip)?;
            }
        }
    }
    print_status(n_exec, n_skip)?;
    writeln!(stderr)
}
