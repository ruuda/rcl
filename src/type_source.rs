// RCL -- A reasonable configuration language.
// Copyright 2024 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Type sources are where types come from, used for error reporting.

use crate::source::Span;

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

    /// The type is part of the expected type for build files for `rcl build`.
    BuildFile(&'static str),
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
            Source::BuildFile(..) => None,
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
}
