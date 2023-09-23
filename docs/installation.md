# Installation

## From source

To build from source, clone the repository from one of the two mirrors:

    git clone https://github.com/ruuda/rcl.git
    git clone https://codeberg.org/ruuda/rcl.git

RCL is written in Rust and builds with [Cargo][cargo]. RCL specifies a
compatible Rust toolchain in `rust-toolchain.toml`. When Cargo is managed by
[Rustup][rustup], it will automatically fetch the right toolchain. To build:

    cargo build --release

Put the binary on your `PATH` to be able to use it system-wide, e.g.:

    cp target/release/rcl ~/.local/bin

[cargo]:  https://doc.rust-lang.org/cargo/guide/
[rustup]: https://rust-lang.github.io/rustup/index.html

## As a Nix flake

The repository includes a Nix flake. It is mainly used to provide a suitable
environment for local development and CI, but it also includes the application
itself. You can run RCL with a [flake-enabled][flakes] version of [Nix][nix],
such as Nix 2.18:

    nix run github:ruuda/rcl -- --help

[flakes]: https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake
[nix]:    https://nixos.org/download
