// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

use std::rc::Rc;

use pyo3::prelude::*;
use rcl::error::Result;
use rcl::loader::{Loader, SandboxMode};
use rcl::runtime::{Env, Value};
use rcl::tracer::StderrTracer;

fn evaluate(src: String) -> Result<Rc<Value>> {
    let mut loader = Loader::new();
    let mut tracer = StderrTracer::new(None);
    loader.initialize_filesystem(SandboxMode::Workdir, None)?;
    let mut env = Env::with_prelude();
    let doc = loader.load_string(src);
    loader.evaluate(doc, &mut env, &mut tracer)
}

fn runtime_error(message: &'static str) -> PyErr {
    PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(message)
}

fn build_python_value(py: Python, v: &Value) -> PyResult<PyObject> {
    use pyo3::types::{PyDict, PyList, PyNone, PySet};
    let result = match v {
        Value::Null => PyNone::get(py).into(),
        Value::Bool(b) => b.to_object(py),
        Value::Int(i) => i.to_object(py),
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
            for x in xs {
                set.add(build_python_value(py, x)?)?;
            }
            set.into()
        }
        Value::Dict(xs) => {
            let dict = PyDict::new(py);
            for (k, v) in xs {
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

#[pyfunction]
fn loads(py: Python, src: String) -> PyResult<PyObject> {
    match evaluate(src) {
        Ok(v) => build_python_value(py, v.as_ref()),
        Err(..) => Err(runtime_error("Evaluation failed.")),
    }
}

// The module exposed to Python. We name the function `pyrcl` to avoid clashing
// with the name `rcl` of the imported Rust crate.
#[pymodule]
#[pyo3(name = "rcl")]
fn pyrcl(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(loads, m)?)?;
    Ok(())
}
