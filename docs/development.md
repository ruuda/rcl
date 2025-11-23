# Developing RCL

This chapter explains the tools and workflows involved in hacking on <abbr>RCL</abbr>.
To get started, clone the repository from one of the two mirrors:

    git clone https://codeberg.org/ruuda/rcl.git
    git clone https://github.com/ruuda/rcl.git

## Building

RCL is written in Rust and builds with Cargo:

    cargo build
    target/debug/rcl --help

See the [building chapter](building.md) for more details and alternative build
configurations.

## CI and automated tests

Almost all of the checks that are part of the <abbr>RCL</abbr> development
process are automated, and included in [the Nix flake][flake]. These checks are
verified on <abbr>CI</abbr> by [Garnix], but you can run them locally as well:

    nix flake check

This means that you can reproduce and debug any <abbr>CI</abbr> run locally.

While it is convenient to be able to run all checks in one command, if you know
what parts of <abbr>RCL</abbr> you changed, you can run just the relevant
checks. They are listed in [the section below](#individual-checks).

## Development environment

For the tools used for development, the Nix flake includes a devshell with the
right version of development dependencies (Python, MkDocs, Tree-sitter, ets.)
that you can enter with:

    nix develop --command $SHELL

Sourcing your tools elsewhere (e.g. your system package manager) will probably
work, but only the Nix flake is continuously tested on <abbr>CI</abbr>.

[flake]:  installation.md#as-a-nix-flake
[Garnix]: https://app.garnix.io/repo/ruuda/rcl

## Individual checks


Run the [golden tests](testing.md#golden-tests):

    cargo build
    golden/run.py

View coverage of the golden tests:

    nix build .#coverage --out-link result
    xdg-open result/index.html

Run unit tests and lints:

    cargo test --workspace
    cargo clippy --workspace

Typecheck Python sources:

    mypy --strict --exclude pyrcl .
    mypy --strict pyrcl

Autoformat Rust, Python, and <abbr>RCL</abbr> code:

<!-- For some reason MkDocs highlights `exec` in an indented code block ... -->
```sh
cargo fmt
black .
fd . --extension .rcl --exclude golden --exec-batch rcl format --in-place
```

Build and preview the manual:

    mkdocs serve

Build crate documentation and check for issues:

    RUSTFLAGS="--deny warnings" cargo doc --no-deps --workspace

## Fuzz tests

After implementing a feature, run the fuzzers to ensure the code still respects
all invariants and is free of crashes. See [the fuzzing section in the testing
chapter][fuzz].

[fuzz]: testing.md#running-the-fuzzers
