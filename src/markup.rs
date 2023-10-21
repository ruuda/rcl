// RCL -- A reasonable configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Utilities for dealing with color and other markup.

use std::io::IsTerminal;

/// A markup hint, used to apply color and other markup to output.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Markup {
    /// Used for error message reporting, styled in bold.
    Error,
    /// Used for error message reporting, styled in bold.
    Warning,
    /// Used for trace message reporting, styled in bold.
    Trace,

    /// Make something stand out in error messages.
    ///
    /// We use this to play a similar role as backticks in Markdown,
    /// to clarify visually where the boundaries of a quotation are.
    Highlight,

    // These are meant for syntax highlighting.
    Builtin,
    Comment,
    Identifier,
    Keyword,
    Number,
    String,
}

/// How to treat color and other markup hints.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MarkupMode {
    /// Ignore all markup hints, do not output them.
    None,
    /// Output markup as ANSI escape sequences.
    Ansi,
}

/// Whether we should use ANSI colors when writing to this file descriptor.
///
/// Returns true when the file descriptor refers to a terminal, unless the
/// `NO_COLOR` environment variable is set to a nonempty string. See also
/// <https://no-color.org/>.
fn should_color<T: IsTerminal>(fd: &T) -> bool {
    if !fd.is_terminal() {
        return false;
    }
    match std::env::var("NO_COLOR") {
        Ok(no_color) => no_color == "",
        Err(..) => true,
    }
}

impl MarkupMode {
    /// Get the default markup configuration for a file descriptor.
    pub fn default_for_fd<T: IsTerminal>(fd: &T) -> Self {
        if should_color(fd) {
            MarkupMode::Ansi
        } else {
            MarkupMode::None
        }
    }

    /// Output the markup required to switch from the `from` style to the `to` style.
    pub fn get_switch(&self, from: Option<Markup>, to: Option<Markup>) -> &'static str {
        debug_assert_ne!(from, to, "Should not switch if from and to are the same.");
        match self {
            MarkupMode::None => "",
            MarkupMode::Ansi => switch_ansi(from, to),
        }
    }
}

/// Return the ANSI escape code to switch from style `from` to style `to`.
pub fn switch_ansi(_from: Option<Markup>, to: Option<Markup>) -> &'static str {
    let reset = "\x1b[0m";
    let bold_red = "\x1b[31;1m";
    let bold_yellow = "\x1b[33;1m";
    let bold_blue = "\x1b[34;1m";
    let red = "\x1b[31m";
    let green = "\x1b[32m";
    let blue = "\x1b[34m";
    let cyan = "\x1b[36m";
    let white = "\x1b[37m";

    let to = match to {
        None => return reset,
        Some(m) => m,
    };
    match to {
        Markup::Error => bold_red,
        Markup::Warning => bold_yellow,
        Markup::Trace => bold_blue,
        Markup::Highlight => white,
        Markup::Builtin => red,
        Markup::Comment => white,
        Markup::Identifier => blue,
        Markup::Keyword => green,
        Markup::Number => cyan,
        Markup::String => red,
    }
}
