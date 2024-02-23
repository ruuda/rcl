// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Type sources are where types come from, used for error reporting.

use crate::error::Error;
use crate::pprint::{concat, Doc};
use crate::source::Span;
use crate::types::AsTypeName;

/// What side to explain the source of a type for.
pub enum Side {
    Expected,
    Actual,
}

/// The place where a type was constructed.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Source {
    /// There is no source for this type.
    ///
    /// This is the case for `Type::Any` when it is introduced by the
    /// typechecker for expressions that are not otherwise constrained.
    None,

    /// Expected `Void` because the collection at the given location is empty.
    EmptyCollection(Span),

    /// The type comes from a built-in function.
    /// TODO: Add more details.
    Builtin,

    /// The type was inferred from a literal.
    Literal(Span),

    /// It was a type annotation in the source code.
    Annotation(Span),

    /// A boolean is required, because it's used as a condition (`if` or `assert`).
    Condition,

    /// The type is the required type of the operator at the given span.
    Operator(Span),

    /// An integer is required due to indexing into a list.
    IndexList,
}

impl Source {
    pub fn span(&self) -> Option<Span> {
        match self {
            Source::EmptyCollection(s) => Some(*s),
            Source::Literal(s) => Some(*s),
            Source::Annotation(s) => Some(*s),
            Source::Operator(s) => Some(*s),
            // Note, we don't handle the cases without span with a `_` pattern
            // on purpose, so that if we add a variant that has a span, it
            // causes a compile error instead of silently not returning it here.
            Source::None => None,
            Source::Builtin => None,
            Source::Condition => None,
            Source::IndexList => None,
        }
    }

    /// Combine two sources of types that are known to be equal.
    ///
    /// When types meet and their meet is not equal to either side, then the
    /// source information is lost. But when both types are equal anyway, we can
    /// pick one of the sources as the source of the combined information. This
    /// should *only* be called if the types are equal. If they are not, we would
    /// be attributing types that we generated at runtime to spans in the source
    /// code!
    pub fn meet(&self, other: &Source) -> Source {
        match (self, other) {
            // If one side is missing a source, then now we learned one.
            (Source::None, _) => *other,
            (_, Source::None) => *other,
            // Builtins have no span to blame them on. We'd rather blame on spans.
            (Source::Builtin, other) => *other,
            (other, Source::Builtin) => *other,
            // All else equal, we go with the source that we discovered first.
            (first, _) => *first,
        }
    }

    /// Add context to a type error about why a particular type was expected.
    /// TODO: Move to SourcedType? Or at least add a shorthand there?
    pub fn clarify_error<T: AsTypeName>(&self, side: Side, type_: &T, error: Error) -> Error {
        let type_name = if type_.is_atom() {
            // If it's an atom, we have space to put the name of the type in the
            // sentence. But remove the coloring, it gets too distracting as the
            // type here is not the main purpose of the sentence.
            match type_.format_type() {
                Doc::Markup(_, type_name) => *type_name,
                _ => unreachable!("Formatted atoms have markup."),
            }
        } else {
            // If it's not an atom, the type might be huge, so we don't put it
            // in the middle of the sentence, we just say "type" and hope the
            // message is still clear enough.
            "type".into()
        };
        let side_verb = match side {
            Side::Expected => "Expected ",
            Side::Actual => "Found ",
        };
        match self {
            Source::None => error,

            // TODO: Add information about the builtin (function and arg name?).
            // At this point builtin types are not involved in type errors,
            // because we don't resolve anything that produces them at typecheck
            // time, and we don't yet typecheck arguments in function calls.
            Source::Builtin => panic!("Currently builtins are not involved in type errors."),

            Source::Literal(at) => {
                let msg = concat! {
                    side_verb type_name " because of this value."
                };
                error.with_note(*at, msg)
            }

            Source::EmptyCollection(at) => {
                let msg = concat! {
                    side_verb type_name " because this collection is empty."
                };
                error.with_note(*at, msg)
            }

            Source::Annotation(at) => {
                let msg = match side {
                    _ if type_.is_atom() => {
                        concat! { side_verb type_name " because of this annotation." }
                    }
                    Side::Expected => "The expected type is specified here.".into(),
                    Side::Actual => "Found a type that is specified here.".into(),
                };
                error.with_note(*at, msg)
            }

            Source::Operator(at) => error.with_note(
                *at,
                concat! {
                    side_verb type_name " because of this operator."
                },
            ),

            Source::Condition => {
                error.with_help("There is no implicit conversion, conditions must be boolean.")
            }

            Source::IndexList => error.with_help("List indices must be integers."),
        }
    }
}
