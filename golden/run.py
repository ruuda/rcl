#!/usr/bin/env python3

# RCL -- A reasonable configuration language.
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
line that contains "# output:". Blank lines preceding it are not considered part
of the input.

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
import tomllib

from typing import Iterable, Iterator, List, Optional


STRIP_ESCAPES = re.compile("\x1b[^m]+m")
RED = "\x1b[31m"
GREEN = "\x1b[32m"
RESET = "\x1b[0m"


def test_one(fname: str, fname_friendly: str, *, rewrite_output: bool) -> Optional[str]:
    """
    Run the given golden test, return `None` if it was successful,
    or the diff if it was not.
    """
    input_lines: List[str] = []
    golden_lines: List[str] = []

    with open(fname, "r", encoding="utf-8") as f:
        target = input_lines
        for line in f:
            if line == "# output:\n":
                target = golden_lines
                continue

            target.append(line)

    # If there is a blank line before the output separator, remove that line,
    # it is not considered part of the input.
    if input_lines[-1].strip() == "":
        input_lines.pop()

    # Run with RUST_BACKTRACE=1 so we get a backtrace if the process panics.
    os.putenv("RUST_BACKTRACE", "1")

    # Allow overriding the binary that we run.
    rcl_bin = os.getenv("RCL_BIN", default="target/debug/rcl")

    # Decide which subcommand to test based on the test directory.
    match os.path.basename(os.path.dirname(fname)):
        case "build":
            cmd = ["build", "--dry-run"]

        case "error" | "types":
            cmd = ["eval"]

        case "error_json":
            cmd = ["eval", "--format=json"]

        case "error_raw":
            cmd = ["eval", "--format=raw"]

        case "fmt":
            cmd = ["fmt"]

        case "json":
            cmd = ["eval", "--format=json"]

        case "json_lines":
            cmd = ["eval", "--format=json-lines"]

        case "html":
            cmd = ["format", "--color=html"]

        case "raw":
            cmd = ["eval", "--format=raw"]

        case "rcl":
            cmd = ["eval", "--format=rcl"]

        case "toml":
            cmd = ["eval", "--format=toml"]
            # For TOML, when the test case is not an error, we additionally test
            # that Python can parse the expected output, because there have been
            # some cases where RCL outputs something that other parsers reject.
            if not os.path.basename(fname).startswith("error_"):
                try:
                    tomllib.loads("".join(golden_lines))
                except Exception as err:
                    raise Exception(f"Invalid TOML in {fname}") from err

        case "yaml_stream":
            cmd = ["eval", "--format=yaml-stream"]

        case unknown:
            raise ValueError(f"No command-line known for {unknown}.")

    result = subprocess.run(
        [rcl_bin, "-C", os.path.dirname(fname), *cmd, "-"],
        input="".join(input_lines),
        capture_output=True,
        encoding="utf-8",
    )
    common_root = os.path.dirname(__file__)
    output_lines = [
        # Strip ANSI escape codes from the output. Also replace references to
        # absolute paths with a known path to make the test results portable.
        STRIP_ESCAPES.sub("", line).replace(common_root, "/WORKDIR")
        for line in result.stdout.splitlines() + result.stderr.splitlines()
    ]

    report_lines: List[str] = []

    for diff_line in difflib.unified_diff(
        a=output_lines,
        b=[line[:-1] for line in golden_lines],
        fromfile="actual " + fname_friendly,
        tofile="golden " + fname_friendly,
        lineterm="",
    ):
        if diff_line.startswith("-"):
            report_lines.append(RED + diff_line + RESET)
        elif diff_line.startswith("+"):
            report_lines.append(GREEN + diff_line + RESET)
        else:
            report_lines.append(diff_line)

    if rewrite_output:
        with open(fname, "w", encoding="utf-8") as f:
            for line in input_lines:
                f.write(line)

            f.write("\n# output:\n")

            for line in output_lines:
                f.write(line)
                f.write("\n")

    if len(report_lines) > 0:
        return "\n".join(report_lines)
    else:
        return None


def main() -> None:
    rewrite_output = False

    if "--rewrite-output" in sys.argv:
        sys.argv.remove("--rewrite-output")
        rewrite_output = True

    if "--help" in sys.argv:
        print(__doc__)
        sys.exit(0)

    fnames = sys.argv[1:]
    golden_dir = os.path.dirname(os.path.abspath(__file__))

    if len(fnames) > 0:
        fnames = [os.path.abspath(fname) for fname in fnames]
    else:
        for root, _dirs, files in os.walk(golden_dir):
            for fname in files:
                if fname.endswith(".test"):
                    fnames.append(os.path.join(root, fname))

    fnames.sort()
    num_good = 0
    all_errors: List[str] = []

    for fname in fnames:
        # Print a status line. The test will later overwrite the status.
        prefix = os.path.commonpath([fname, golden_dir])
        fname_friendly = fname.removeprefix(prefix + "/")
        errors = test_one(fname, fname_friendly, rewrite_output=rewrite_output)
        if errors is None:
            num_good += 1
            print(f"[{GREEN} ok {RESET}] {fname_friendly}")
        else:
            all_errors.append(errors)
            print(f"[{RED}FAIL{RESET}] {fname_friendly}")

    for error in all_errors:
        print()
        print(error)

    num_bad = len(fnames) - num_good
    print()
    print(f"Tested {len(fnames)} inputs, {num_good} good, {num_bad} bad.")

    if num_good == len(fnames):
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
