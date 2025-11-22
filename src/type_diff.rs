// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! A type diff is the result of a subtype check.
//!
//! This module contains the definitions, and machinery for printing type diffs.
use crate::error::{IntoError, Result};
use crate::pprint::{concat, indent, Doc};
use crate::source::Span;
use crate::types::{AsTypeName, FunctionArg, Side, SourcedType, Type};

/// A component in a subtype check `T ≤ U` where `U` is expected and `T` encountered.
#[derive(Debug)]
pub enum Mismatch {
    /// The type error cannot be broken down further. Here are `T` and `U`.
    Atom {
        expected: SourcedType,
        actual: SourcedType,
    },

    /// Both sides are a list, but the element type has an issue.
    List(Box<TypeDiff<SourcedType>>),

    /// Both sides are a set, but the element type has an issue.
    Set(Box<TypeDiff<SourcedType>>),

    /// Both sides are a dict, but the key or value (or both) have issues.
    Dict(Box<TypeDiff<SourcedType>>, Box<TypeDiff<SourcedType>>),

    /// Both sides are functions of the same arity, but args or result have issues.
    Function(Vec<TypeDiff<FunctionArg>>, Box<TypeDiff<SourcedType>>),
}

/// The result of a subtype check `T ≤ U` where `U` is expected and `T` encountered.
///
/// The result is a tree, to be able to pinpoint where the check fails, enabling
/// more helpful errors.
///
/// See also [`SourcedType::is_subtype_of`].
#[derive(Debug)]
pub enum TypeDiff<T> {
    /// Yes, `T ≤ U`, and here is `T`.
    Ok(T),

    /// For `t: T`, we *might* have `t: U`. Here is `V` such that `T ≤ V ≤ U`.
    Defer(T),

    /// For all `t: T`, we have that `t` is not a value of `U`.
    ///
    /// Or, in some cases this is not strictly true, but we want to rule out
    /// that case because it makes more sense. For example, we say that
    /// `List[Number]` and `List[String]` are incompatible, even though `[]`
    /// inhabits both.
    Error(Mismatch),
}

/// The result of a static typecheck.
pub enum Typed<T> {
    /// The type is known statically, and this is the most specific type we infer.
    Type(T),

    /// We can't check this statically, a runtime check is needed.
    ///
    /// If the runtime check passes, then the value fits the returned type.
    Defer(T),
}

impl<T> TypeDiff<T> {
    pub fn check(self, at: Span) -> Result<Typed<T>> {
        self.check_with_context(at, "")
    }

    pub fn check_unpack_scalar(self, at: Span) -> Result<Typed<T>> {
        self.check_with_context(at, " in unpacked element")
    }

    /// Report the diff as a type error, or extract its result.
    fn check_with_context(self, at: Span, location_context: &'static str) -> Result<Typed<T>> {
        match self {
            TypeDiff::Ok(t) => Ok(Typed::Type(t)),
            TypeDiff::Defer(t) => Ok(Typed::Defer(t)),
            TypeDiff::Error(Mismatch::Atom { actual, expected }) => {
                let mut error = if let Type::Void = expected.type_ {
                    at.error(concat! {
                        "Expected a value of type "
                        "Void".format_type()
                        location_context
                        ", but no such values exist."
                    })
                } else {
                    // Any can never be the top level-cause of a type error.
                    // As a supertype, any value is fine, and as the actual type,
                    // it should result in a runtime check rather than an error.
                    debug_assert_ne!(actual.type_, Type::Any, "Any should not cause errors.");
                    debug_assert_ne!(expected.type_, Type::Any, "Any should not cause errors.");

                    // A top-level type error, we can report with a simple message.
                    at.error(concat! {
                        "Type mismatch"
                        location_context
                        "."
                    })
                    .with_body(report_type_mismatch(&expected, &actual))
                };

                // If we have it, explain why the expected type is expected.
                expected.explain_error(Side::Expected, &mut error);

                // If the actual type doesn't come from the span that we are
                // attributing the error to, then also include a note about that.
                let should_report_source = match actual.source.span() {
                    Some(src_span) => at != src_span,
                    None => true,
                };
                if should_report_source {
                    actual.explain_error(Side::Actual, &mut error);
                }
                error.err()
            }
            TypeDiff::Error(diff) => {
                // If the error is nested somewhere inside a type, then we
                // resort to a more complex format where we first print the
                // type itself, with the error part replaced with a placeholder,
                // and then we add a secondary error to explain the placeholder.
                crate::fmt_type::DiffFormatter::report(at, location_context, &diff).err()
            }
        }
    }
}

/// Format a static type error body.
///
/// This does not include the "Type mismatch." message, so that the body can be
/// used in various places.
pub fn report_type_mismatch<T1: AsTypeName, T2: AsTypeName>(
    expected: &T1,
    actual: &T2,
) -> Doc<'static> {
    // If types are atoms, they are short to format, so we can put the message
    // on one line. If they are composite, we put them in an indented block.
    match (expected.is_atom(), actual.is_atom()) {
        (true, true) => concat! {
            "Expected " expected.format_type()
            " but found " actual.format_type() "."
        },
        (true, false) => concat! {
            "Expected " expected.format_type() " but found this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { actual.format_type() }
        },
        (false, true) => concat! {
            "Expected this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { expected.format_type() }
            Doc::HardBreak Doc::HardBreak
            "But found " actual.format_type() "."
        },
        (false, false) => concat! {
            "Expected this type:"
            Doc::HardBreak Doc::HardBreak
            indent! { expected.format_type() }
            Doc::HardBreak Doc::HardBreak
            "But found this type: "
            Doc::HardBreak Doc::HardBreak
            indent! { actual.format_type() }
        },
    }
}
