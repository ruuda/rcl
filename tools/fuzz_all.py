#!/usr/bin/env python3

# RCL -- A reasonable configuration language.
# Copyright 2025 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
This script runs all fuzzers for a short amount of time. Its primary purpose
is to check that all fuzzers can run, and find no obvious issues, before making
a new release. This does not run the fuzzers for long enough to find non-obvious
issues, that must be tested for during development of new features.
"""

import subprocess
import time
import tomllib


CARGO_FUZZ = ["cargo", "+nightly-2023-11-09", "fuzz"]
BUILD_ARGS = ["--sanitizer=none", "--debug-assertions", "--release"]


def run_fuzzer(*, target: str, run_seconds: int) -> subprocess.Popen[bytes]:
    cmd = [
        *CARGO_FUZZ,
        "run",
        target,
        *BUILD_ARGS,
        "--",
        "-max_len=32",
        "-timeout=2",
        f"-max_total_time={run_seconds}",
    ]
    return subprocess.Popen(cmd)


def main() -> None:
    with open("fuzz/Cargo.toml", "rb") as f:
        manifest = tomllib.load(f)
        targets = [t["name"] for t in manifest["bin"] if t["name"].startswith("fuzz_")]

    # Build all fuzzers first. If we skip this, then the `cargo fuzz run` will
    # build them, but then they all fight for the lock on the build dir. We
    for target in targets:
        cmd = [*CARGO_FUZZ, "build", *BUILD_ARGS, target]
        subprocess.check_call(cmd)

    run_seconds = 180
    timeout_seconds = run_seconds + 60

    procs = []
    for target in targets:
        procs.append(run_fuzzer(target=target, run_seconds=180))
        # Don't start all processes at once; they will fight for the lock on the
        # package cache, even when there is nothing to build. That's not harmful,
        # but it's spammy.
        sleep_seconds = 0.05
        time.sleep(sleep_seconds)

    for target, proc in zip(targets, procs):
        proc.wait(timeout=timeout_seconds)
        assert proc.returncode == 0, f"Fuzzer {target} failed."


if __name__ == "__main__":
    main()
