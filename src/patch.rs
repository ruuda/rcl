// RCL -- A reasonable configuration language.
// Copyright 2025 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The implementation of `rcl patch`.

use crate::cst::{Expr, Seq, SeqControl, Stmt, Yield};
use crate::error::{IntoError, Result};
use crate::loader::Loader;
use crate::pprint::{concat, Doc};
use crate::source::{DocId, Span};
use crate::string::is_identifier;

pub struct Patcher<'a> {
    path: Vec<&'a str>,
    replacement: Expr,
}

impl<'a> Patcher<'a> {
    /// Prepare a patcher by parsing the path and replacement.
    pub fn new(loader: &mut Loader, path: &'a str, replacement: String) -> Result<Self> {
        let path_id = loader.load_string("path", path.to_string());
        let replacement_id = loader.load_string("replacement", replacement);
        let result = Patcher {
            path: parse_path_expr(path, path_id)?,
            replacement: loader.get_cst(replacement_id)?,
        };
        Ok(result)
    }

    /// Apply the patch to the `source_cst`.
    ///
    /// Because this swaps the replacement CST into place, the patcher cannot be
    /// reused afterwards.
    pub fn apply(mut self, input: &str, source_span: Span, source_cst: &mut Expr) -> Result<()> {
        patch_expr(
            input,
            &self.path,
            source_cst,
            source_span,
            &mut self.replacement,
        )
    }
}

/// Parse a document path expression.
///
/// A path is a sequence of identifiers separated by dots, e.g. `foo.bar.baz`.
///
/// We take a `DocId`, to be able to report a span on error, so we can highlight
/// the exact span of the problem.
fn parse_path_expr(path: &str, doc_id: DocId) -> Result<Vec<&str>> {
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

/// Splice the replacement into the source CST at the given path.
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
fn patch_expr(
    input: &str,
    path: &[&str],
    source: &mut Expr,
    source_span: Span,
    replacement: &mut Expr,
) -> Result<()> {
    let target = match path.first() {
        Some(tf) => tf,
        None => {
            // If the path is empty, then we have arrived at the final target,
            // and we need to replace the source node itself.
            std::mem::swap(source, replacement);
            return Ok(());
        }
    };

    // There are multiple cases below where we want to report this error defined
    // here, with the entire source span as the search space, but we don't want
    // to allocate the `Error` in the success case, so we construct it lazily.
    let make_err = || {
        let msg = concat! {
            "Could not find '"
            Doc::highlight(target).into_owned()
            "' in this expression."
        };
        source_span.error(msg).err()
    };

    match source {
        Expr::BraceLit { elements, .. } | Expr::BracketLit { elements, .. } => {
            for element in elements.elements.iter_mut() {
                match patch_seq(input, path, element, replacement) {
                    Some(result) => return result,
                    None => continue,
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

/// Splice the replacement into the source CST at the given path, if the path matches.
///
/// The path must not be empty, because seq elements themselves cannot be the
/// target of a replacement, only the right-hand side of bindings can.
///
/// Returns `Ok` if the substitution was applied, `Err` if it failed inside, and
/// `None` if the path did not match anywhere.
fn patch_seq(
    input: &str,
    path: &[&str],
    source: &mut Seq,
    replacement: &mut Expr,
) -> Option<Result<()>> {
    // First we check for any let bindings before the yield.
    for control in source.control.iter_mut() {
        match &mut control.inner {
            SeqControl::Stmt { stmt } => match patch_stmt(input, path, stmt, replacement) {
                Some(result) => return Some(result),
                None => continue,
            },
            _ => continue,
        }
    }

    let (target, suffix) = path
        .split_first()
        .expect("Should not call `patch_seq` with empty path.");

    // If there is no match yet, we try to match the yield itself, if it has
    // record form (`ident = expr`).
    match &mut source.body.inner {
        Yield::AssocIdent {
            field,
            value,
            value_span,
            ..
        } if field.resolve(input) == *target => {
            // We matched one segment of the path, now it's up to the inner
            // expression to match. If that fails and returns Err, we do *not*
            // continue the search to see if anything else might match, we
            // greedily follow the path by first matches only.
            Some(patch_expr(input, suffix, value, *value_span, replacement))
        }
        _ => None,
    }
}

/// Splice the replacement into the source CST at the given path.
///
/// If the path matches this statement, either the substitution succeeds, and
/// we return `Some(Ok)`, or it fails somewhere inside, and we return `Some(Err)`.
/// If the path does not match this statement, we return `None`.
///
/// The path must not be empty, because statements themselves cannot be the
/// target of a replacement, only the right-hand side of let bindings can.
fn patch_stmt(
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
