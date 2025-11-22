// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Formatter that prints types as RCL type expressions.
//!
//! This formatter is superficially similar to the one in [`crate::fmt_rcl`].

use crate::error::{Error, IntoError};
use crate::markup::Markup;
use crate::pprint::{concat, group, indent, Doc};
use crate::source::Span;
use crate::type_diff::{report_type_mismatch, Mismatch, TypeDiff};
use crate::types::{FunctionArg, Side, SourcedType, Type};

/// Render a type.
pub fn format_type(type_: &Type) -> Doc {
    match type_ {
        // For primitive types the short name is the full name.
        Type::Any | Type::Bool | Type::Null | Type::Number | Type::String | Type::Void => {
            Doc::from(type_.short_name()).with_markup(Markup::Type)
        }

        // Collection types.
        Type::Dict(kv) => concat! {
            Doc::from("Dict").with_markup(Markup::Type)
            format_types("[", [(None, &kv.key.type_), (None, &kv.value.type_)], "]")
        },
        Type::List(element_type) => concat! {
            Doc::from("List").with_markup(Markup::Type)
            format_types("[", [(None, &element_type.type_)], "]")
        },
        Type::Set(element_type) => concat! {
            Doc::from("Set").with_markup(Markup::Type)
            format_types("[", [(None, &element_type.type_)], "]")
        },
        Type::Union(union) => concat! {
            Doc::from("Union").with_markup(Markup::Type)
            format_types("[", union.members.iter().map(|st| (None, &st.type_)), "]")
        },

        Type::Function(func) => concat! {
            format_types(
                "(",
                func.args.iter().map(|arg| (
                    arg.name.as_ref().map(|n| n.as_ref()),
                    &arg.type_.type_,
                )),
                ")"
            )
            " -> "
            format_type(&func.result.type_)
        },
    }
}

/// A list of types enclosed by opening and closing delimiters.
fn format_types<'a, Types: IntoIterator<Item = (Option<&'a str>, &'a Type)>>(
    open: &'static str,
    types: Types,
    close: &'static str,
) -> Doc<'a> {
    let mut parts = Vec::new();
    for (optional_name, t) in types {
        // We put the name and the type together in a group, so that in tall
        // mode, we get a `name: Type` per line as long as that fits.
        if let Some(name) = optional_name {
            let group = group! { name ":" Doc::Sep format_type(t) };
            parts.push(group);
        } else {
            parts.push(format_type(t));
        }

        parts.push(concat! { "," Doc::Sep });
    }

    // Remove the unconditional trailing comma and replace it with one that is
    // only present in tall mode.
    parts.pop();
    parts.push(Doc::tall(","));

    group! {
        open
        Doc::SoftBreak
        indent! { Doc::Concat(parts) }
        Doc::SoftBreak
        close
    }
}

struct MismatchAtom<'a> {
    expected: &'a SourcedType,
    actual: &'a SourcedType,
}

pub struct DiffFormatter<'a> {
    errors: Vec<MismatchAtom<'a>>,
}

impl<'a> DiffFormatter<'a> {
    pub fn report(at: Span, location_context: &'static str, mismatch: &'a Mismatch) -> Error {
        let mut state = DiffFormatter { errors: Vec::new() };
        let mut parts = vec![concat! {
            Doc::HardBreak
            Doc::HardBreak
            indent! { state.format_mismatch(mismatch) }
        }];
        for (i, inner_error) in state.errors.iter().enumerate() {
            let n = i + 1;
            let message = concat! {
                Doc::HardBreak
                Doc::HardBreak
                "At E" n.to_string() ": "
                report_type_mismatch(inner_error.expected, inner_error.actual)
            };
            parts.push(message);
        }
        let message = concat! { "Type mismatch inside this type" location_context ":" };
        let mut error = at.error(message).with_body(Doc::Concat(parts).into_owned());
        for inner_error in state.errors.iter() {
            inner_error
                .expected
                .explain_error(Side::Expected, &mut error);
            inner_error.actual.explain_error(Side::Actual, &mut error);
        }
        error
    }

    fn format_type_diff(&mut self, diff: &'a TypeDiff<SourcedType>) -> Doc<'a> {
        match diff {
            TypeDiff::Ok(t) => format_type(&t.type_),
            TypeDiff::Defer(t) => format_type(&t.type_),
            TypeDiff::Error(mismatch) => self.format_mismatch(mismatch),
        }
    }

    fn format_arg_diff(&mut self, diff: &'a TypeDiff<FunctionArg>) -> Doc<'a> {
        let (name, type_) = match diff {
            TypeDiff::Ok(t) => (t.name.as_ref(), format_type(&t.type_.type_)),
            TypeDiff::Defer(t) => (t.name.as_ref(), format_type(&t.type_.type_)),
            TypeDiff::Error(mismatch) => (None, self.format_mismatch(mismatch)),
        };
        match name {
            Some(name) => concat! { name.as_ref() ":" Doc::Sep type_ },
            None => type_,
        }
    }

    fn format_mismatch(&mut self, mismatch: &'a Mismatch) -> Doc<'a> {
        match mismatch {
            Mismatch::Atom { expected, actual } => {
                self.errors.push(MismatchAtom { expected, actual });
                // In the diff, we print the error node numbered as "<E1>" etc.
                let err_doc = concat! { "<E" self.errors.len().to_string() ">" };
                err_doc.with_markup(Markup::Error)
            }
            Mismatch::List(element) => concat! {
                Doc::from("List").with_markup(Markup::Type)
                Self::format_types("[", [element.as_ref()], "]", |t| self.format_type_diff(t))
            },
            Mismatch::Set(element) => concat! {
                Doc::from("Set").with_markup(Markup::Type)
                Self::format_types("[", [element.as_ref()], "]", |t| self.format_type_diff(t))
            },
            Mismatch::Dict(key, value) => concat! {
                Doc::from("Dict").with_markup(Markup::Type)
                Self::format_types("[", [key.as_ref(), value.as_ref()], "]", |t| self.format_type_diff(t))
            },
            Mismatch::Function(args, result) => concat! {
                Self::format_types("(", args.iter(), ")", |t| self.format_arg_diff(t))
                " -> "
                self.format_type_diff(result)
            },
        }
    }

    /// A list of types enclosed by opening and closing delimiters.
    fn format_types<T: 'a, Types: IntoIterator<Item = &'a T>, Format: FnMut(&'a T) -> Doc<'a>>(
        open: &'static str,
        types: Types,
        close: &'static str,
        mut format: Format,
    ) -> Doc<'a> {
        let mut parts = Vec::new();
        for t in types {
            parts.push(format(t));
            parts.push(concat! { "," Doc::Sep });
        }

        // Remove the unconditional trailing comma and replace it with one that
        // is only present in tall mode.
        parts.pop();
        parts.push(Doc::tall(","));

        group! {
            open
            Doc::SoftBreak
            indent! { Doc::Concat(parts) }
            Doc::SoftBreak
            close
        }
    }
}
