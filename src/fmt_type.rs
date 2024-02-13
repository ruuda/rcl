// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Formatter that prints types as RCL type expressions.
//!
//! This formatter is superficially similar to the one in [`fmt_rcl`].

use crate::markup::Markup;
use crate::pprint::{concat, group, indent, Doc};
use crate::types::Type;

/// Render a type.
pub fn format_type(type_: &Type) -> Doc {
    match type_ {
        Type::Any => Doc::from("Any").with_markup(Markup::Type),
        Type::Void => Doc::from("Void").with_markup(Markup::Type),

        // Primitive types.
        Type::Bool => Doc::from("Bool").with_markup(Markup::Type),
        Type::Int => Doc::from("Int").with_markup(Markup::Type),
        Type::Null => Doc::from("Null").with_markup(Markup::Type),
        Type::String => Doc::from("String").with_markup(Markup::Type),

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

        // The function type.
        Type::Function(func) => concat! {
            format_types(
                "(",
                func.args.iter().map(|(name, t)| (
                    name.as_ref().map(|n| n.as_ref()),
                    &t.type_,
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
        if let Some(name) = optional_name {
            parts.push(name.into());
            parts.push(":".into());
            parts.push(Doc::Sep);
        }
        parts.push(format_type(t));
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
