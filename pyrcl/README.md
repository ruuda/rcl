# RCL Python Module

This directory contains Python bindings for RCL, built with PyO3.

## Building

Build the shared object, from the repository root:

    cargo build --manifest-path pyrcl/Cargo.toml

Give the shared object the appropriate name for the Python interpreter to
discover it:

    mv target/debug/{librcl,rcl}.so

Tell Python where to find the shared object, run the interpreter:

    PYTHONPATH=target/debug python3
    >>> import rcl
    >>> help(rcl.loads)
    >>> rcl.load_file("examples/buckets.rcl")
