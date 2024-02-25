// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Implementation of the `rcl build` subcommand.

use crate::error::Result;
use crate::runtime::Value;

/// Take a build specification and write the outputs to files.
pub fn execute_build(_targets: Value) -> Result<()> {
    unimplemented!("TODO");
    Ok(())
}
