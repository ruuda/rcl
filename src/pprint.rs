// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Utilities for pretty-printing code.
//!
//! The approach in this module is inspired by Philip Wadler’s 2003 paper
//! [A Prettier Printer][wadler2003]. The implementation is slightly different
//! to make it more suitable for Rust’s strict evaluation, and new constructors
//! are added to deal with comments, which should be ignored for width
//! calculation because we don’t reflow them, and which should always force a
//! tall rather than wide layout.
//!
//! [wadler2003]: https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

/// A document tree that can be pretty-printed.
///
/// Every node can be printed in two ways: wide or tall. The goal of the
/// pretty-printer is to print as many nodes as possible in wide mode without
/// exceeding the line width limit. When a node is printed in tall mode, all of
/// its parents must be printed in tall mode as well.
///
/// Indentation is only output in tall mode; in wide mode this behaves like
/// a non-indented group. Note that a `Doc::Indent` and `Doc::Concat` do not
/// themselves represent the ability to choose wide or tall mode, only
/// `Doc::Group` does. Consider this example:
///
/// ```text
/// group! {
///   "["
///   SoftBreak
///   indent! {
///     "elem0" "," Sep
///     "elem1" "," Sep
///     "elem2" Doc::if_tall(",")
///   }
///   SoftBreak
///   "]"
/// }
/// ```
///
/// If we would allow making a wide/tall decision for the `Doc::Indent`
/// node, then the following three outputs are valid:
///
/// ```text
/// // Wide-wide
/// [elem0, elem1, elem2]
///
/// // Tall-wide
/// [
///   elem0, elem1, elem2
/// ]
///
/// // Tall-tall
/// [
///   elem0,
///   elem1,
///   elem2,
/// ]
/// ```
///
/// If we only make a wide/tall decision at `Doc::Group`, then only the
/// first and last outputs are valid, so this gives us more control: the
/// middle example can still be produced, by wrapping the `Doc::Indent` in
/// a `Doc::Group`.
enum Doc<'a> {
    /// A literal piece of content.
    String { content: &'a str, width: u32 },

    /// A string which does not contribute to the width of the document.
    ///
    /// We use this for printing comments. Because we do not reflow comments, if
    /// a comment makes a particular format exceed the line length limit, that
    /// should not force us to choose a taller/narrower layout; instead we just
    /// let the comments exceed the limit, and the user can reflow the comment
    /// later if desired.
    ZeroWidth(&'a str),

    /// An ascii character of width 1 which is only output in tall mode.
    ///
    /// This can be used to add tailing commas in a collection, but only when
    /// the collection is formatted in tall mode.
    IfTall(u8),

    /// A space in wide mode; a newline in tall mode.
    Sep,

    /// An empty string in wide mode; a newline in tall mode.
    SoftBreak,

    /// A newline. Forces tall mode onto all its parents.
    HardBreak,

    /// A concatenation of document fragments.
    Concat(Vec<Doc<'a>>),

    /// A group can be formatted either in wide mode or in tall mode.
    Group(Box<Doc<'a>>),

    /// An indented block.
    ///
    Indent(Box<Doc<'a>>),
}

/// Whether to format a node in wide mode or tall mode.
#[derive(Copy, Clone)]
enum Mode {
    Wide,
    Tall,
}

/// Whether printing in a particular mode fitted or not.
///
/// The `Ord` impl returns the worst possible result (overflow) as maximum.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
enum PrintResult {
    /// The content could be printed within the allocated width.
    Fits,

    /// The content exceeded the target width.
    Overflow,
}

impl PrintResult {
    pub fn is_overflow(&self) -> bool {
        matches!(self, PrintResult::Overflow)
    }
}

impl<'a> Doc<'a> {
    /// Construct a new document fragment of width zero.
    pub fn zero_width(value: &'a str) -> Doc<'a> {
        debug_assert!(
            !value.contains('\n'),
            "Doc fragments cannot contain newlines, use SoftBreak etc.",
        );
        Doc::ZeroWidth(value)
    }

    /// Construct a new document fragment that only gets emitted in tall mode.
    pub fn if_tall(ch: u8) -> Doc<'a> {
        debug_assert_ne!(
            ch, b'\n',
            "Doc fragments cannot contain newlines, use SoftBreak etc.",
        );
        Doc::IfTall(ch)
    }

    /// Whether any of the nodes in this tree force tall mode.
    ///
    /// A hard break forces tall mode.
    pub fn is_forced_tall(&self) -> bool {
        match self {
            Doc::HardBreak => true,
            Doc::Concat(children) => children.iter().any(|node| node.is_forced_tall()),
            Doc::Group(inner) => inner.is_forced_tall(),
            Doc::Indent(inner) => inner.is_forced_tall(),
            _ => false,
        }
    }

    pub fn print(&self, printer: &mut Printer, mode: Mode) -> PrintResult {
        match self {
            Doc::String { content, width } => printer.push_str(content, *width),
            Doc::ZeroWidth(content) => {
                let width = 0;
                printer.push_str(content, width)
            }
            Doc::IfTall(ch) => match mode {
                Mode::Tall => printer.push_char(*ch),
                Mode::Wide => PrintResult::Fits,
            },
            Doc::Sep => match mode {
                Mode::Tall => printer.newline(),
                Mode::Wide => printer.push_char(b' '),
            },
            Doc::SoftBreak => match mode {
                Mode::Tall => printer.newline(),
                Mode::Wide => PrintResult::Fits,
            },
            Doc::HardBreak => match mode {
                Mode::Tall => printer.newline(),
                Mode::Wide => unreachable!("HardBreak forces Tall mode."),
            },
            Doc::Concat(children) => children
                .iter()
                .fold(PrintResult::Fits, |r, doc| doc.print(printer, mode).max(r)),
            Doc::Group(inner) => {
                if inner.is_forced_tall() {
                    debug_assert!(matches!(mode, Mode::Tall));
                    return inner.print(printer, mode);
                }

                match mode {
                    // If we are wide, then the inner content must be wide too.
                    Mode::Wide => inner.print(printer, mode),

                    // If we are tall, then we can try to make the inner content
                    // wide. If that is too wide, then we backtrack and try to
                    // make it tall instead.
                    Mode::Tall => match printer.try_(|p| inner.print(p, Mode::Wide)) {
                        PrintResult::Overflow => inner.print(printer, Mode::Tall),
                        PrintResult::Fits => PrintResult::Fits,
                    },
                }
            }
            Doc::Indent(inner) => match mode {
                Mode::Wide => inner.print(printer, mode),
                Mode::Tall => printer.indented(|p| inner.print(p, mode)),
            },
        }
    }
}

struct Printer {
    /// Buffer where we place the output.
    out: String,

    /// Target width that we should try to not exceed.
    width: u32,

    /// The width so far of the line that we are currently writing.
    line_width: u32,

    /// The current indentation level, counted in spaces.
    indent: u32,

    /// Whether indentation has been written for the current line.
    needs_indent: bool,

    /// Checkpoints that we can revert to.
    ///
    /// The checkpoint records the length of the buffer at that time, and the
    /// `line_width` at that time. To revert to the checkpoint, we truncate the
    /// buffer back to that length. To commit the topmost checkpoint, we just
    /// pop it from this stack.
    checkpoints: Vec<(usize, u32)>,
}

impl Printer {
    /// Create a new printer with the given line width target.
    pub fn new(width: u32) -> Printer {
        Printer {
            out: String::new(),
            width,
            line_width: 0,
            indent: 0,
            needs_indent: true,
            checkpoints: Vec::new(),
        }
    }

    /// Return the result string printed to the printer.
    pub fn into_inner(self) -> String {
        debug_assert!(
            self.checkpoints.is_empty(),
            "Should only consume printer outside of a `try_`.",
        );
        self.out
    }

    /// Execute `f` against this printer. If the result was too wide, roll back.
    pub fn try_<F: FnOnce(&mut Printer) -> PrintResult>(&mut self, f: F) -> PrintResult {
        self.checkpoints.push((self.out.len(), self.line_width));
        let result = f(self);
        let (len, width) = self
            .checkpoints
            .pop()
            .expect("Impossible: push/pop are balanced.");
        if result.is_overflow() {
            self.out.truncate(len);
            self.line_width = width;
        }
        result
    }

    /// Execute `f` under increased indentation width.
    pub fn indented<F: FnOnce(&mut Printer) -> PrintResult>(&mut self, f: F) -> PrintResult {
        self.indent += 2;
        let result = f(self);
        self.indent -= 2;
        result
    }

    /// Write the indent after the newline, if needed.
    fn write_indent(&mut self) {
        if !self.needs_indent {
            return;
        }

        // 50 spaces.
        let spaces = "                                                  ";

        let mut n_left = self.indent as usize;
        while n_left > 0 {
            let n = n_left.min(spaces.len());
            self.out.push_str(&spaces[..n]);
            n_left -= n;
        }

        self.needs_indent = false;
    }

    /// Report whether the current content still fits.
    fn fits(&self) -> PrintResult {
        if self.line_width > self.width {
            PrintResult::Overflow
        } else {
            PrintResult::Fits
        }
    }

    pub fn push_str(&mut self, value: &str, width: u32) -> PrintResult {
        debug_assert!(
            !value.contains('\n'),
            "Use `newline` to push a newline instead."
        );
        self.write_indent();
        self.out.push_str(value);
        self.line_width += width;
        self.fits()
    }

    pub fn push_char(&mut self, ch: u8) -> PrintResult {
        debug_assert_ne!(ch, b'\n', "Use `newline` to push a newline instead.");
        self.write_indent();
        self.out.push(ch as char);
        self.line_width += 1;
        self.fits()
    }

    pub fn newline(&mut self) -> PrintResult {
        // TODO: Print indent.
        self.out.push('\n');
        self.line_width = 0;
        self.needs_indent = true;
        // For the print result, we measure until the end of the line, so a
        // newline fits by definition, even if the previous line might have
        // exceeded the target width. This is mostly to simplify call sites
        // where all match arms return `PrintResult`.
        PrintResult::Fits
    }
}

impl<'a> From<&'a str> for Doc<'a> {
    fn from(value: &'a str) -> Doc<'a> {
        use unicode_width::UnicodeWidthStr;
        debug_assert!(
            !value.contains('\n'),
            "Doc fragments cannot contain newlines, use SoftBreak etc.",
        );
        Doc::String {
            width: value.width() as u32,
            content: value,
        }
    }
}

macro_rules! group {
    { $($fragment:expr)* } => {
        // TODO: Add constructor that avoids constructing nested Concat and
        // inlines the children if they are concats.
        Doc::Group(Box::new(Doc::Concat(vec![$(
            $fragment.into(),
        )*])))
    }
}

macro_rules! indent {
    { $($fragment:expr)* } => {
        // TODO: Add constructor that avoids constructing nested Concat and
        // inlines the children if they are concats.
        Doc::Indent(Box::new(Doc::Concat(vec![$(
            $fragment.into(),
        )*])))
    }
}

#[cfg(test)]
mod test {
    use super::{Doc, Mode, Printer};

    fn print_width(doc: &Doc, width: u32) -> String {
        let mut printer = Printer::new(width);
        doc.print(&mut printer, Mode::Tall);
        printer.into_inner()
    }

    #[test]
    fn format_array_wide_tall() {
        use Doc::{Sep, SoftBreak};
        let doc = group! {
            "["
            SoftBreak
            indent! {
                "elem0" "," Sep
                "elem1" "," Sep
                "elem2" Doc::if_tall(b',')
            }
            SoftBreak
            "]"
        };
        assert_eq!(print_width(&doc, 80), "[elem0, elem1, elem2]");
        assert_eq!(print_width(&doc, 5), "[\n  elem0,\n  elem1,\n  elem2,\n]");
    }

    #[test]
    fn hard_break_forces_tall_mode() {
        use Doc::{HardBreak, SoftBreak};
        let doc = group! {
            "["
            indent! {
                HardBreak
                "// Comment."
                SoftBreak
                "elem0"
            }
            SoftBreak
            "]"
        };
        // Despite fitting in 80 columns, we should still format tall because of
        // the hard break.
        assert_eq!(print_width(&doc, 80), "[\n  // Comment.\n  elem0\n]");
    }
}
