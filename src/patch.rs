// RCL -- A reasonable configuration language.
// Copyright 2025 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The implementation of `rcl patch`.

use crate::cst::{Expr, Stmt, Yield};
use crate::error::{IntoError, Result};
use crate::pprint::{concat, Doc};
use crate::source::{DocId, Span};
use crate::string::is_identifier;

/// Parse a document path expression.
///
/// A path is a sequence of identifiers separated by dots, e.g. `foo.bar.baz`.
///
/// We take a `DocId`, to be able to report a span on error, so we can highlight
/// the exact span of the problem.
pub fn parse_path_expr(path: &str, doc_id: DocId) -> Result<Vec<&str>> {
    let mut result = Vec::new();
    let mut start = 0;
    loop {
        let (has_more, end) = match path.bytes().skip(start).position(|b| b == b'.') {
            Some(i) => (true, start + i),
            None => (false, path.len()),
        };
        let ident = &path[start..end];

        if is_identifier(ident) {
            result.push(ident);
            start = end + 1;
        } else {
            let err_span = Span::new(doc_id, start, end);
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
) -> Result<()> {
    let (target, suffix) = match path.split_first() {
        Some(tf) => tf,
        None => {
            // If the path is empty, then we have arrived at the final target,
            // and we need to replace the source node itself.
            std::mem::swap(source, replacement);
            return Ok(());
        }
    };

    // Construct the error lazily: there are multiple cases below where we want
    // to report *this* error, with the entire source span as the search space,
    // but we don't want to allocate the `Error` in the success case.
    let make_err = || {
        source_span
            .error(concat! {
                "Could not find '"
                Doc::highlight(target).into_owned()
                "' in this expression."
            })
            .err()
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
                        // We matched one segment of the path, now it's up to
                        // the inner expression to match. If that fails and
                        // returns Err, we do *not* continue the search to see
                        // if anything else might match, we greedily follow the
                        // path by first matches.
                        return patch_expr(input, suffix, value, *value_span, replacement);
                    }
                    // TODO: Handle nested Stmt, probably by extracting a
                    // function that traverses Seq. Maybe I need to refactor the
                    // CST to unnest the statements first though.
                    // Update: This refactor has now happened, but let me rebase
                    // all this code before addressing this todo.
                    _ => continue,
                }
            }
            make_err()
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
                    Some(result) => return result,
                    None => continue,
                }
            }
            // If that did not match, then we descend further into the body, and
            // try to match there. We have a choice for error reporting here: if
            // we fail to match, we could blame it on the entire `Statements`
            // expression, or only on the body. The correct thing to do would
            // be to map the error here and blame it on the entire expression.
            // However, because we cite only the first line of the error span,
            // that will highlight the first statement, which may not even be
            // a let-binding. I expect that keeping the blame on the body is
            // slightly clearer.
            patch_expr(input, path, &mut body.inner, *body_span, replacement)
        }
        _ => make_err(),
    }
}

/// Splice in the replacement into the source CST at the given path.
///
/// If the path matches this statement, either the substitution succeeds, and
/// we return `Some(Ok)`, or it fails somewhere inside, and we return `Some(Err)`.
/// If the path does not match this statement, we return `None`. The path must
/// not be empty, because statements themselves cannot be the target of a
/// replacement, only the right-hand side of let bindings can.
pub fn patch_stmt(
    input: &str,
    path: &[&str],
    source: &mut Stmt,
    replacement: &mut Expr,
) -> Option<Result<()>> {
    let (target, suffix) = path
        .split_first()
        .expect("Should not call `patch_stmt` with empty path.");

    match source {
        Stmt::Let {
            ident,
            value,
            value_span,
            ..
        } if ident.resolve(input) == *target => Some(patch_expr(
            input,
            suffix,
            &mut *value,
            *value_span,
            replacement,
        )),
        _ => None,
    }
}
