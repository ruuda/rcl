# RCL WebAssembly Module

This directory defines a Rust crate that can be compiled to WebAssembly. It is
intended to power an interactive demo on the webpage.

## Building with Nix

To build the module and bindings with Nix:

    $ nix build .#wasm --out-link result
    $ ls -l $(realpath result)
    total 292
    -r--r--r-- 1 root root 289692 Jan  1  1970 rcl_bg.wasm
    -r--r--r-- 1 root root   6526 Jan  1  1970 rcl.js

If you don't want to build with Nix, follow the steps below instead.

## Dependencies

The devshell in the Nix flake includes the required `wasm-pack` and
[`wasm-opt` from Binaryen][binaryen]. You will also need the
`wasm32-unknown-unknown` target. It is not by default part of
`rust-toolchain.toml` because it's no use for producing small binaries.
Add it manually:

    rustup target add wasm32-unknown-unknown

[binaryen]: https://github.com/WebAssembly/binaryen

## Building

You will need [`wasm-pack-cli`][wasm-pack-cli] and [
Build a large-ish binary:

    cargo build \
      --manifest-path wasm/Cargo.toml
      --profile=release-wasm \
      --target=wasm32-unknown-unknown \

Build a much smaller binary, but requires nightly:

    cargo +nightly build --lib \
      --manifest-path wasm/Cargo.toml \
      --profile=release-wasm \
      --target=wasm32-unknown-unknown \
      -Z build-std=std,panic_abort \
      -Z build-std-features=panic_immediate_abort

Shrink the binary further with [`wasm-opt` from Binaryen][binaryen]:

    wasm-opt -Oz \
      target/wasm32-unknown-unknown/release-wasm/rcl_wasm.wasm \
      --output target/rcl.wasm

Inspect the wasm file to confirm it doesn't contain needless fluff:

    wasm-dis target/rcl.wasm

Generate the Javascript bindings:

    wasm-bindgen \
      --out-dir target/web \
      --target no-modules \
      --no-typescript \
      target/rcl.wasm

Put everything together:

    cp wasm/index.html target/web
    python -m http.server --directory target/web
