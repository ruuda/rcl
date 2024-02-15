#!/usr/bin/env python3

# RCL -- A reasonable configuration language.
# Copyright 2023 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
Seed the fuzz corpus from the golden tests.

SYNOPSIS

  tools/seed_fuzz_corpus.py
"""

import os

from hashlib import sha1
from statistics import quantiles
from typing import List


def seed_one(fname: str) -> int:
    """
    Extract the test case and write it into the fuzz directory. Return the
    length of the input in bytes.
    """
    # Prepend a comment line that is required for the fuzzer to decide how
    # it will treat this input, see also `fuzz_targets/main.rs`. `a` means
    # "evaluate", and `Z` is '\n' + 80, for a format width of 80 columns.
    input_lines: List[str] = ["//aZ\n"]

    with open(fname, "r", encoding="utf-8") as f:
        for line in f:
            if line == "# output:\n":
                break

            input_lines.append(line)

    input_bytes = "".join(input_lines).strip().encode("utf-8")

    # Libfuzzer by default names the fuzz inputs after their sha1sum, so we do
    # that as well.
    shasum = sha1(input_bytes).hexdigest()
    with open(f"fuzz/corpus/main/{shasum}", "wb") as f:
        f.write(input_bytes)
        print(f"{shasum} {len(input_bytes):4} {fname}")

    return len(input_bytes)


def main() -> None:
    corpus_dir = "fuzz/corpus/main"

    lens: List[int] = []
    for root, _dirs, files in os.walk("golden"):
        for fname in files:
            if fname.endswith(".test"):
                lens.append(seed_one(os.path.join(root, fname)))

    print("Length q0.25, q0.5, q0.75:", quantiles(lens, n=4))


if __name__ == "__main__":
    main()
