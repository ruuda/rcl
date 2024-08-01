#!/usr/bin/env python3

# RCL -- A reasonable configuration language.
# Copyright 2024 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
Publish files tracked in the RCL (mono)repository into separate repositories.

Some tools -- in particular around Tree-sitter -- are repository-oriented and
expect some code that we rather track in subdirectories, to live in their own
repository. This script updates those external repositories to match the source
of truth in this repository.

REPOSITORIES

We expect the following directories to exist relative to the repository root:

  ../tree-sitter-rcl
  ../zed-rcl

RATIONALE

Why export to an external repository rather than including those external
repositories as a Git submodule here?

  * Git submodules currently do not work very nicely with Nix flakes, which
    would make it harder to use the current Nix-based CI.
  * Some consumers of Tree-sitter grammars prefer to have the generated files
    be part of the repository, but in this repository we prefer to be minimal
    and don't have large assets that are effectively opaque like binaries. In
    an external repository, we can have both.
"""

import os
from os import path


def main() -> None:
    assert path.isdir("../tree-sitter-rcl/.git")
    assert path.isdir("../zed-rcl/.git")


if __name__ == "__main__":
    main()
