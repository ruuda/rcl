#!/usr/bin/env python3

# RCL -- A sane configuration language.
# Copyright 2023 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

# This test runner is adapted from the one in Squiller [1], which is also
# copyright Ruud van Asseldonk and licensed Apache 2.0.
# [1]: https://github.com/ruuda/squiller

"""
A test runner for running the golden tests.

The runner takes golden input files, splits them into inputs and expectations,
and then prints whether they match. Inputs and expectations are separated by a
double blank line.

SYNOPSIS

  golden/run.py [--rewrite-output] [<file>...]
  golden/run.py --help

OPTIONS

  <file>...          One or more input files to test. When empty, all .test
                     files in the golden directory are used.

  --rewrite-output   Rewrite the input files to contain the actual output. Use
                     this to update the goldens after making an intentional
                     change.

  RCL_BIN            Set this environment variable to override the binary to
                     execute, defaults to "target/debug/rcl".
                      
"""

import difflib
import os
import re
import subprocess
import sys

from typing import Iterable, List, Iterator


STRIP_ESCAPES = re.compile("\x1b[^m]+m")
RED = "\x1b[31m"
GREEN = "\x1b[32m"
RESET = "\x1b[0m"


def test_one(fname: str, *, rewrite_output: bool) -> bool:
    """
    Run the given golden test, return whether it was succesful.
    """
    input_lines: List[str] = []
    golden_lines: List[str] = []

    with open(fname, "r", encoding="utf-8") as f:
        consecutive_blank = 0
        target = input_lines
        for line in f:
            target.append(line)

            if line == "\n":
                consecutive_blank += 1
            else:
                consecutive_blank = 0

            if consecutive_blank >= 2:
                target = golden_lines

    # The input is separated from the output by a double blank line. Strip those
    # from the input fed to the program, if they were actually blank.
    for _ in range(2):
        if input_lines[-1].strip() == "":
            input_lines.pop()

    # Run with RUST_BACKTRACE=1 so we get a backtrace if the process panics.
    os.putenv("RUST_BACKTRACE", "1")

    # Allow overriding the binary that we run.
    rcl_bin = os.getenv("RCL_BIN", default="target/debug/rcl")

    result = subprocess.run(
        [rcl_bin, "eval", "-"],
        input="".join(input_lines),
        capture_output=True,
        encoding="utf-8",
    )
    output_lines = [
        # Strip ANSI escape codes from the output.
        STRIP_ESCAPES.sub("", line)
        for line in result.stdout.splitlines() + result.stderr.splitlines()
    ]

    is_good = True

    for diff_line in difflib.unified_diff(
        a=output_lines,
        b=[line[:-1] for line in golden_lines],
        fromfile="actual",
        tofile="golden",
        lineterm="",
    ):
        if is_good:
            print(f"\r[{RED}fail{RESET}]\n")
            is_good = False

        if diff_line.startswith("-"):
            print(RED + diff_line + RESET)
        elif diff_line.startswith("+"):
            print(GREEN + diff_line + RESET)
        else:
            print(diff_line)

    if is_good:
        print(f"\r[{GREEN} ok {RESET}]")
    else:
        print()

    if rewrite_output:
        with open(fname, "w", encoding="utf-8") as f:
            for line in input_lines:
                f.write(line)

            f.write("\n\n")

            for line in output_lines:
                f.write(line)
                f.write("\n")

    return is_good


def main() -> None:
    rewrite_output = False

    if "--rewrite-output" in sys.argv:
        sys.argv.remove("--rewrite-output")
        rewrite_output = True

    if "--help" in sys.argv:
        print(__doc__)
        sys.exit(0)

    fnames = sys.argv[1:]

    if len(fnames) == 0:
        for root, _dirs, files in os.walk("golden"):
            for fname in files:
                if fname.endswith(".test"):
                    fnames.append(os.path.join(root, fname))

    fnames.sort()
    num_good = 0

    for fname in fnames:
        # Print a status line. The test will later overwrite the status.
        print(f"[ .. ] {fname}", end="", flush=True)
        num_good += int(test_one(fname, rewrite_output=rewrite_output))

    num_bad = len(fnames) - num_good
    print(f"Tested {len(fnames)} inputs, {num_good} good, {num_bad} bad.")

    if num_good == len(fnames):
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
