// RCL -- A reasonable configuration language.
// Copyright 2025 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The implementation of `rcl patch`.

use crate::cst::{Expr, Stmt, Yield};
use crate::error::{Error, IntoError, Result};
use crate::pprint::{concat, Doc};
use crate::source::{self, Span};
use crate::string::is_identifier;

/// Parse a document path expression.
///
/// A path is a sequence of identifiers separated by dots, e.g. `foo.bar.baz`.
///
/// We take a `Doc`, rather than just a string, so we can highlight errors in
/// the exact span where there was a problem, in case the path is not valid.
pub fn parse_path_expr<'a>(path: &source::Doc<'a>) -> Result<Vec<&'a str>> {
    let mut result = Vec::new();
    let mut start = 0;
    loop {
        let (has_more, end) = match path.data.bytes().skip(start).position(|b| b == b'.') {
            Some(i) => (true, start + i),
            None => (false, path.data.len()),
        };
        let ident = &path.data[start..end];

        if is_identifier(ident) {
            result.push(ident);
            start = end + 1;
        } else {
            let err_span = Span::new(path.span.doc(), start, end);
            return err_span
                .error("This path segment is not a valid identifier.")
                .with_help(
                    "A document path can only contain identifiers, \
                    not list indexes or arbitrary keys.",
                )
                .err();
        }
        if !has_more {
            return Ok(result);
        }
    }
}

/// The result of attempting to patch the CST.
///
/// This is like `Result`, except there is an additional `NotFound` case,
/// because failing to match is not immediately fatal.
pub enum PatchResult {
    Ok,
    NotFound,
    Err(Box<Error>),
}

impl From<Result<()>> for PatchResult {
    fn from(result: Result<()>) -> PatchResult {
        match result {
            Ok(()) => PatchResult::Ok,
            Err(e) => PatchResult::Err(e),
        }
    }
}

/// Splice in the replacement into the source CST at the given path.
///
/// If a match was found, `replacement` will contain the replaced node. That is,
/// we swap `replacement` with the target node.
///
/// We do not attempt to fix up the span information in the source CST, which
/// means that it's bogus after patching. We could try to do that, and we could
/// even abstract, typecheck and evaluate the new CST afterwards, but error
/// reporting in such a spliced CST will be tricky: although spans of individual
/// expressions are correct, when we highlight them in an error message, it will
/// highlight the original document, which can then be confusing. Imagine we
/// have this document:
/// ```rcl
/// let x: Number = 42;
/// ```
/// and we patch path `x` with new value `"foobar"`. This creates a type error,
/// which we could report at document `cmdline`, and highlight the `"foobar"`
/// itself. But we also point to the expected type, and then we'd print out the
/// original 42, and it would be confusing. Better to just save the patched
/// document first and evaluate it afterwards.
pub fn patch_expr(
    input: &str,
    path: &[&str],
    source: &mut Expr,
    source_span: Span,
    replacement: &mut Expr,
) -> PatchResult {
    let (target, suffix) = match path.split_first() {
        Some(tf) => tf,
        None => {
            // If the path is empty, then we have arrived at the final target,
            // and we need to replace the source node itself.
            std::mem::swap(source, replacement);
            return PatchResult::Ok;
        }
    };

    // We construct the error lazily, in the happy case we don't need it.
    let fatal_not_found = || {
        source_span
            .error(concat! {
                "Could not find '"
                Doc::highlight(target).into_owned()
                "' in this expression."
            })
            .err()
            .into()
    };

    match source {
        Expr::BraceLit { elements, .. } => {
            for element in elements.elements.iter_mut() {
                match &mut element.body.inner {
                    Yield::AssocIdent {
                        field,
                        value,
                        value_span,
                        ..
                    } if field.resolve(input) == *target => {
                        match patch_expr(input, suffix, value, *value_span, replacement) {
                            PatchResult::Ok => return PatchResult::Ok,
                            PatchResult::NotFound => continue,
                            PatchResult::Err(err) => return PatchResult::Err(err),
                        }
                    }
                    // TODO: Handle nested Stmt, probably by extracting a
                    // function that traverses Seq. Maybe I need to refactor the
                    // CST to unnest the statements first though.
                    // Update: This refactor has now happened, but let me rebase
                    // all this code before addressing this todo.
                    _ => continue,
                }
            }
            fatal_not_found()
        }
        Expr::Statements {
            stmts,
            body_span,
            body,
        } => {
            // First, we try to match any let bindings among the statements
            // against the target.
            for (_span, stmt) in stmts.iter_mut() {
                match patch_stmt(input, path, &mut stmt.inner, replacement) {
                    PatchResult::Ok => return PatchResult::Ok,
                    PatchResult::NotFound => continue,
                    PatchResult::Err(err) => return PatchResult::Err(err),
                }
            }
            // If that did not match, then we descend further into the body, and
            // try to match there.
            patch_expr(input, path, &mut body.inner, *body_span, replacement)
        }
        _ => fatal_not_found(),
    }
}

/// Splice in the replacement into the source CST at the given path.
///
/// Returns the replaced node. The path must not be empty, because statements
/// themselves cannot be the target of a replacement, only the right-hand side
/// of let bindings can.
pub fn patch_stmt(
    input: &str,
    path: &[&str],
    source: &mut Stmt,
    replacement: &mut Expr,
) -> PatchResult {
    let (target, suffix) = path
        .split_first()
        .expect("Should not call `patch_stmt` with empty path.");

    match source {
        Stmt::Let {
            ident,
            value,
            value_span,
            ..
        } if ident.resolve(input) == *target => {
            patch_expr(input, suffix, &mut *value, *value_span, replacement)
        }
        _ => PatchResult::NotFound,
    }
}
