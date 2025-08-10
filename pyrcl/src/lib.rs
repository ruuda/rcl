// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use pyo3::prelude::*;
use rcl::cli::Target;
use rcl::error::Result;
use rcl::loader::{Loader, SandboxMode};
use rcl::runtime::{self, Value};
use rcl::source::DocId;
use rcl::tracer::StderrTracer;
use rcl::typecheck;

fn evaluate<F: FnOnce(&mut Loader) -> Result<DocId>>(load: F) -> Result<Value> {
    let mut loader = Loader::new();
    loader.initialize_filesystem(SandboxMode::Workdir, None)?;
    let doc = load(&mut loader)?;
    let mut tracer = StderrTracer::new(None);
    let mut type_env = typecheck::prelude();
    let mut value_env = runtime::prelude();
    loader.evaluate(&mut type_env, &mut value_env, doc, &mut tracer)
}

fn runtime_error(message: &'static str) -> PyErr {
    PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(message)
}

fn build_python_value(py: Python, v: &Value) -> PyResult<PyObject> {
    use pyo3::types::{PyDict, PyList, PyNone, PySet};
    let result = match v {
        Value::Null => PyNone::get(py).into(),
        Value::Bool(b) => b.to_object(py),
        Value::Number(d) => {
            // Like Python's json module, try to preserve integers as an int
            // object.
            if d.decimals == 0 && d.exponent == 0 {
                d.mantissa.to_object(py)
            } else {
                d.to_f64_lossy().to_object(py)
            }
        }
        Value::String(s) => s.to_object(py),
        Value::List(xs) => {
            let values = xs
                .iter()
                .map(|x| build_python_value(py, x))
                .collect::<PyResult<Vec<_>>>()?;
            PyList::new(py, values).into()
        }
        Value::Set(xs) => {
            let set = PySet::empty(py)?;
            for x in xs.iter() {
                set.add(build_python_value(py, x)?)?;
            }
            set.into()
        }
        Value::Dict(xs) => {
            let dict = PyDict::new(py);
            for (k, v) in xs.iter() {
                dict.set_item(build_python_value(py, k)?, build_python_value(py, v)?)?;
            }
            dict.into()
        }
        Value::Function(..) | Value::BuiltinFunction(..) | Value::BuiltinMethod { .. } => {
            return Err(runtime_error("Functions cannot be exported to Python."))
        }
    };
    Ok(result)
}

/// Load an RCL expression from the file at the given path.
#[pyfunction]
fn load_file(py: Python, path: String) -> PyResult<PyObject> {
    // Behavior of the file paths for this function is the same as on the
    // command line; it's *not* the same as for import expressions.
    match evaluate(|loader| loader.load_cli_target(&Target::File(path))) {
        Ok(v) => build_python_value(py, &v),
        Err(..) => Err(runtime_error("Evaluation failed.")),
    }
}

/// Evaluate an RCL expression.
#[pyfunction]
fn loads(py: Python, src: String) -> PyResult<PyObject> {
    match evaluate(|loader| Ok(loader.load_string("input", src))) {
        Ok(v) => build_python_value(py, &v),
        Err(..) => Err(runtime_error("Evaluation failed.")),
    }
}

// The module exposed to Python. We name the function `pyrcl` to avoid clashing
// with the name `rcl` of the imported Rust crate.
#[pymodule]
#[pyo3(name = "rcl")]
fn pyrcl(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(load_file, m)?)?;
    m.add_function(wrap_pyfunction!(loads, m)?)?;
    Ok(())
}
