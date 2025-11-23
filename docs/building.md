# Building

RCL is written in Rust and builds with [Cargo][cargo]:

    cargo build --release
    target/release/rcl --help

The repository includes a `rust-toolchain.toml` file that specifies a compatible
Rust version. When Cargo is managed by [Rustup][rustup], Rustup will
automatically fetch the right toolchain.

To build a static binary rather than a dynamically linked one:

    cargo build --release --target x86_64-unknown-linux-musl
    target/x86_64-unknown-linux-musl/release/rcl --help

[cargo]:  https://doc.rust-lang.org/cargo/guide/
[rustup]: https://rust-lang.github.io/rustup/index.html

## Cross-compiling

With [cargo-zigbuild], we can cross-compile for other targets:

    cargo zigbuild --release --target aarch64-unknown-linux-gnu

This process is automated with Nix for supported targets:

    nix build .#binaries
    sha256sum result/*

The resulting binaries are independent of the Nix store. This is how we build
release artifacts for the supported platforms.

[cargo-zigbuild]: https://github.com/rust-cross/cargo-zigbuild

## Reproducibility

As of v0.10.0, builds for the following targets are confirmed to be bitwise
[reproducible] when building using Nix:

 * aarch64-unknown-linux-gnu
 * armv7-unknown-linux-gnueabihf
 * x86_64-unknown-linux-gnu

Unfortunately, the following targets are not reproducible, help to diagnose and
fix the discrepancies is welcome:

 * aarch64-apple-darwin

[reproducible]: https://reproducible-builds.org/

## Python module

See the readme in the `pyrcl` directory for how to build the Python module.

## Wasm module

See the readme in the `wasm` directory for how to build the Webassembly module.
