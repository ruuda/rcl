# RCL Python Module

This directory contains Python bindings for RCL, built with PyO3.

## Building the module

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

## Building a Python package

[Maturin] can build a Python wheel:

    maturin build --manifest-path pyrcl/Cargo.toml

Maturin can also build a portable manylinux wheel. We automate this as part of
the Nix flake:

    nix build .#pyrcl-wheel --out-link result
    uv run --with result/*.whl python
    >>> import rcl
    >>> rcl.loads("20 + 22")
    42

The same flake attribute also contains an `sdist` archive. We publish them
together to Pypi using [Twine]:

    twine upload result/*

[Maturin]: https://www.maturin.rs/
[Twine]:   https://twine.readthedocs.io/en/latest/
