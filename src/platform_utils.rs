// RCL -- A sane configuration language.
// Copyright 2023 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Platform-specific utilities.

use std::os::fd::{AsRawFd, RawFd};
use std::os::raw::c_int;

pub trait CouldBeTerminal: AsRawFd {
    /// Return whether the file is an open file descriptor referring to a terminal.
    ///
    /// This probably does not work on Windows. Feel free to open a pull request
    /// if it can be fixed with a few lines of code and without external
    /// dependencies.
    fn is_tty(&self) -> bool {
        #[link(name = "c")]
        extern "C" {
            fn isatty(fd: RawFd) -> c_int;
        }
        let fd = self.as_raw_fd();
        let result = unsafe { isatty(fd) };
        result == 1
    }

    /// Whether we should use ANSI colors when writing to this file descriptor.
    ///
    /// Returns true when the file descriptor refers to a terminal, unless the
    /// `NO_COLOR` environment variable is set to a nonempty string. See also
    /// <https://no-color.org/>.
    fn should_color(&self) -> bool {
        // TODO: Since Rust 1.70, there is `is_terminal` in the stdlib!
        if !self.is_tty() {
            return false;
        }
        match std::env::var("NO_COLOR") {
            Ok(no_color) => no_color == "",
            Err(..) => true,
        }
    }
}

impl<T: AsRawFd> CouldBeTerminal for T {}
