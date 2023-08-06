// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Types for dealing with input source code.

/// A list of input documents.
pub type Inputs<'a> = [&'a str];

/// The index of a document in the list of input files.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct DocId(pub u32);

/// Marks a location in a source file by byte offset.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
    /// Start of the token, inclusive.
    pub start: usize,

    /// Length of the token.
    ///
    /// The length of a token is limited to 32 bits, such that `Span` itself,
    /// which also contains the document id, fits in two 128 bits.
    pub len: u32,

    /// Index of the document that contains the span.
    pub doc: DocId,
}

impl Span {
    /// End of the token, exclusive.
    pub fn end(&self) -> usize {
        self.start + self.len as usize
    }

    /// Return the slice from the input that this span spans.
    pub fn resolve<'a>(&self, inputs: &Inputs<'a>) -> &'a str {
        let doc = inputs[self.doc.0 as usize];
        &doc[self.start..self.end()]
    }
}
