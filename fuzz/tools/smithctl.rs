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
    print    Pretty-print a fuzz input.
";

fn main() {
    let mut args = std::env::args();
    // Skip the program name.
    args.next();
    let args: Vec<String> = args.collect();

    match args.first().map(|a| a.as_ref()) {
        Some("print") => cmd_print(&args[1..]).unwrap(),
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
