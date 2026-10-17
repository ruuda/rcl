// RCL -- A reasonable configuration language.
// Copyright 2025 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! The implementation of `rcl patch`.

use crate::cst::{Expr, Seq, SeqControl, Stmt, Yield};
use crate::error::{Error, IntoError, Result};
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
    /// reused afterward.
    pub fn apply(self, input: &str, source_span: Span, source_cst: &mut Expr) -> Result<()> {
        let mut patcher = PatcherImpl::new(input, self.replacement);
        patcher.patch_expr(&self.path, source_cst, source_span);

        match patcher.sites_matched.len() {
            0 => patcher.report_error_no_match(&self.path).err(),
            1 => Ok(()),
            _ => patcher.report_error_no_unique_match(&self.path).err(),
        }
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

struct PatcherImpl<'a> {
    input: &'a str,

    /// The node to swap in when there is a match.
    ///
    /// If the match site is unique then we swap this in only once, and this
    /// variable holds what was previously at the matched position. If there is
    /// a further match then this means we keep moving nodes in a nonsensical
    /// way, but that's okay because if the match is not unique, then in the end
    /// we report an error.
    replacement: Expr,

    /// Sites at which the patch path matched, and we replaced a node.
    sites_matched: Vec<Span>,

    /// Sites where we checked, but we failed to match the full path.
    ///
    /// These are tuples of `(path length, span searched)`, where the path
    /// length is the length of the remaining path to match. Lower numbers
    /// therefore mean that the match matched more components.
    sites_checked: Vec<(usize, Span)>,
}

impl<'a> PatcherImpl<'a> {
    pub fn new(input: &'a str, replacement: Expr) -> Self {
        Self {
            input,
            replacement,
            sites_matched: Vec::new(),
            sites_checked: Vec::new(),
        }
    }

    /// Report the most specific place where we failed to match.
    pub fn report_error_no_match(&self, path: &[&str]) -> Error {
        let (depth, span) = self
            .sites_checked
            .iter()
            .min_by_key(|(len, span)| (len, std::cmp::Reverse(span.start())))
            .expect("We always checked *somewhere* if we didn't match.");

        let target = path[path.len() - depth];
        let msg = concat! {
            "Could not find '"
            Doc::highlight(target).into_owned()
            "' in this expression."
        };
        span.error(msg)
    }

    pub fn report_error_no_unique_match(&self, path: &[&str]) -> Error {
        let mut parts = Vec::new();
        parts.push("The path '".into());
        for (i, ident) in path.iter().enumerate() {
            if i > 0 {
                parts.push(".".into());
            }
            parts.push(Doc::highlight(ident).into_owned());
        }
        parts.push("' is ambiguous, it matches in multiple places.".into());
        let mut error = Error::new(Doc::Concat(parts));

        for site in &self.sites_matched {
            error = error.with_note(*site, "Path matches here.");
        }

        error
    }

    /// Splice the replacement into the source CST at the given path.
    ///
    /// If a match was found, `replacement` will contain the replaced node. That
    /// is, we swap `replacement` with the target node.
    ///
    /// Patching finds all sites that match, and applies everywhere. We report
    /// those in `sites_applied`. If at the end there is no unique site, we
    /// append an error, if there is no earlier error for the current depth.
    ///
    /// We do not attempt to fix up the span information in the source CST,
    /// which means that it's bogus after patching. We could try to do that, and
    /// we could even abstract, typecheck and evaluate the new CST afterward,
    /// but error reporting in such a spliced CST will be tricky: although spans
    /// of individual expressions are correct, when we highlight them in an
    /// error message, it will highlight the original document, which can then
    /// be confusing. Imagine we have this document:
    /// ```rcl
    /// let x: Number = 42;
    /// ```
    /// and we patch path `x` with new value `"foobar"`. This creates a type
    /// error, which we could report at document `cmdline`, and highlight the
    /// `"foobar"` itself. But we also point to the expected type, and then we'd
    /// print out the original 42, and it would be confusing. Better to just
    /// save the patched document first and evaluate it afterward.
    fn patch_expr(&mut self, path: &[&str], source: &mut Expr, source_span: Span) {
        if path.is_empty() {
            // If the path is empty, then we have arrived at the final
            // target, and we need to replace the source node itself.
            self.sites_matched.push(source_span);
            std::mem::swap(source, &mut self.replacement);
            return;
        }

        match source {
            Expr::BraceLit { elements, .. } | Expr::BracketLit { elements, .. } => {
                for element in elements.elements.iter_mut() {
                    self.patch_seq(path, element);
                }
                self.sites_checked.push((path.len(), source_span));
            }
            Expr::Statements {
                stmts,
                body_span,
                body,
            } => {
                // First, we try to match any let bindings among the statements
                // against the target.
                for (_span, stmt) in stmts.iter_mut() {
                    self.patch_stmt(path, &mut stmt.inner);
                }
                // If that did not match, then we descend further into the body,
                // and try to match there.
                self.patch_expr(path, &mut body.inner, *body_span)
            }
            _ => self.sites_checked.push((path.len(), source_span)),
        }
    }

    /// Splice the replacement into the source CST at the given path, if the path matches.
    ///
    /// The path must not be empty, because seq elements themselves cannot be
    /// the target of a replacement, only the right-hand side of bindings can.
    fn patch_seq(&mut self, path: &[&str], source: &mut Seq) {
        // First we check for any let bindings before the yield.
        for control in source.control.iter_mut() {
            match &mut control.inner {
                SeqControl::Stmt { stmt } => self.patch_stmt(path, stmt),
                _ => continue,
            }
        }

        // If there is no match yet, we try to match the yield itself, if it has
        // record form (`ident = expr`, or nested, `a.b = expr`). The key path
        // of the entry must be a prefix of the path we are looking for.
        match &mut source.body.inner {
            Yield::AssocIdent {
                path_head,
                path_tail,
                value,
                value_span,
                ..
            } => {
                // If the path does not match the target, this is not the right node.
                if path_head.resolve(self.input) != path[0] {
                    return;
                }

                // Match the entire tail, failing to match also when the target
                // is shorter than the path we have.
                for (i, ident) in path_tail.iter().enumerate() {
                    if Some(ident.resolve(self.input)) != path[1..].get(i).cloned() {
                        return;
                    }
                }

                // We matched the segments of the key path, now it's up to
                // the inner expression to match the remainder
                let suffix = &path[1 + path_tail.len()..];
                self.patch_expr(suffix, value, *value_span)
            }
            _ => { /* No match. */ }
        }
    }

    /// Splice the replacement into the source CST at the given path.
    ///
    /// The path must not be empty, because statements themselves cannot be the
    /// target of a replacement, only the right-hand side of let bindings can.
    fn patch_stmt(&mut self, path: &[&str], source: &mut Stmt) {
        let (target, suffix) = path
            .split_first()
            .expect("Should not call `patch_stmt` with empty path.");

        match source {
            Stmt::Let {
                ident,
                value,
                value_span,
                ..
            } if ident.resolve(self.input) == *target => {
                self.patch_expr(suffix, &mut *value, *value_span);
            }
            _ => { /* No match. */ }
        }
    }
}
