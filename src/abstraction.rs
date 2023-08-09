// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Abstraction converts a Concrete Syntax Tree into an Abstract Syntax Tree.
//!
//! It is responsible for a few things:
//!
//! * Forgetting about non-code such as blank lines and comments.
//! * Converting literals in the source code into values in the runtime.
//! * Removing syntactical differences (e.g. converting `k = v;` into `"k": v`).

use crate::ast::Expr as AExpr;
use crate::cst::Expr as CExpr;

pub fn abstract_expr(expr: &CExpr) -> AExpr {
    match expr {
        CExpr::Let { ident, value, body } => {
            // TODO: Now we need to start plucking the strings out of the source
            // and into the runtime ...
            unimplemented!("TODO");
        }
        _ => {
            unimplemented!("TODO");
        }
    }
}
