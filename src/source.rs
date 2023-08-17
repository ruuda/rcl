// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Types for dealing with input source code.

use std::fmt;

/// A named input document.
pub struct Document<'a> {
    /// Path can be a file path, but also a name such as "stdin".
    pub path: &'a str,

    /// The contents of the file.
    pub data: &'a str,
}

/// A list of input documents.
pub type Inputs<'a> = [Document<'a>];

/// The index of a document in the list of input files.
#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct DocId(pub u32);

impl fmt::Debug for DocId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Marks a location in a source file by byte offset.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Span {
    /// Index of the document that contains the span.
    pub doc: DocId,

    /// Start of the token, inclusive.
    pub start: usize,

    /// Length of the token.
    ///
    /// The length of a token is limited to 32 bits, such that `Span` itself,
    /// which also contains the document id, fits in two 128 bits.
    pub len: u32,
}

impl Span {
    /// End of the token, exclusive.
    pub fn end(&self) -> usize {
        self.start + self.len as usize
    }

    /// Return the slice from the input that the span spans.
    pub fn resolve<'a>(&self, input: impl Source<'a>) -> &'a str {
        input.resolve(*self)
    }

    /// Delete n bytes from the start of the span.
    pub fn trim_start(&self, n: u32) -> Span {
        let n_trim = self.len.min(n);
        Span {
            doc: self.doc,
            start: self.start + n_trim as usize,
            len: self.len - n_trim,
        }
    }

    /// Delete n bytes from the end of the span.
    pub fn trim_end(&self, n: u32) -> Span {
        let n_trim = self.len.min(n);
        Span {
            doc: self.doc,
            start: self.start,
            len: self.len - n_trim,
        }
    }

    /// Return a span that runs from self up to but not including `other`.
    pub fn until(&self, other: Span) -> Span {
        debug_assert_eq!(self.doc, other.doc);
        debug_assert!(other.end() >= self.end());
        Span {
            doc: self.doc,
            start: self.start,
            // TODO: Change types so this would fit.
            len: (other.start - self.start) as u32,
        }
    }
}

pub trait Source<'a> {
    /// Return the slice from the input that the span spans.
    fn resolve(self, span: Span) -> &'a str;
}

/// If we resolve against a string, then we assume that this string is the
/// right document for this span, and we ignore the document id in the span.
impl<'a> Source<'a> for &'a str {
    fn resolve(self, span: Span) -> &'a str {
        &self[span.start..span.end()]
    }
}

impl<'a> Source<'a> for &Inputs<'a> {
    fn resolve(self, span: Span) -> &'a str {
        let doc = self[span.doc.0 as usize].data;
        &doc[span.start..span.end()]
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn span_is_two_usizes() {
        assert!(std::mem::size_of::<Span>() <= std::mem::size_of::<usize>() * 2);
        assert_eq!(std::mem::align_of::<Span>(), std::mem::align_of::<usize>());
    }
}
