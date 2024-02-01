// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use pyo3::prelude::*;

/// Formats the sum of two numbers as string.
#[pyfunction]
fn loads(src: String) -> PyResult<String> {
    // TODO: Evaluate.
    Ok(src)
}

#[pymodule]
fn rcl(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(loads, m)?)?;
    Ok(())
}
