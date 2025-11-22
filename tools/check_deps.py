#!/usr/bin/env python3

# RCL -- A reasonable configuration language.
# Copyright 2025 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
Verify that dependencies match certain rules.

Similar to `cargo-deny`, except this is based on `cargo-license`. Unfortunately
neither can be easily made to work with Nix, both need Internet connectivity to
download crate metadata.
"""

import json
import subprocess
import sys


ALLOWED_LICENSES = [
    "Apache-2.0 OR Apache-2.0 WITH LLVM-exception OR MIT",
    "Apache-2.0 OR BSD-2-Clause OR MIT",
    "Apache-2.0 OR LGPL-2.1-or-later OR MIT",
    "Apache-2.0 OR MIT",
    "Apache-2.0",
    "MIT OR Unlicense",
    "MIT",
    "MPL-2.0",
]

ALLOWED_CRATES = [
    "unicode-ident",
]


def check_crate(manifest_path: str) -> None:
    cmd = [
        "cargo",
        "license",
        "--json",
        "--avoid-build-deps",
        "--manifest-path",
        manifest_path,
    ]
    crates = json.loads(subprocess.check_output(cmd, encoding="utf-8"))
    for crate in crates:
        try:
            if crate["name"] in ALLOWED_CRATES:
                continue

            assert crate["license"] in ALLOWED_LICENSES

        except AssertionError:
            print(f"Violation in dependency of {manifest_path}, crate:")
            json.dump(crate, sys.stdout, indent=True)
            print()
            raise


if __name__ == "__main__":
    check_crate("Cargo.toml")
    check_crate("pyrcl/Cargo.toml")
    check_crate("wasm/Cargo.toml")
