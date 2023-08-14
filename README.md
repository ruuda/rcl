# Ruud’s Configuration Language

**Vaporware warning:** This is a proof-of-concept toy project. I will probably
lose interest in it before it reaches a point where it is usable and useful.

Why another config language? Because:

 * HCL is too ad-hoc to be suitable for any serious abstraction (`setunion` is
   variadic so it only works with a statically known number of sets; `flatten`
   recursively flattens so it can’t be typed properly and breaks generic code,
   for comprehensions can’t do nested loops, `for_each` syntax is bolted on,
   etc.)

 * Nix-the-language is great but forces the entire Nix store on you when all I
   want is to evaluate expressions.

 * Python is great but requires some boilerplate for doing the IO if you want
   to use it as a configuration language. Also the syntactic order of list
   comprehensions prevents autocomplete in editors.

 * Dhall has the right ideas but the syntax and heavy use of Unicode symbols
   make it look ugly.

 * CUE and Nickel were not invented here.

## Classification

 * Purely functional: RCL consists of expressions and has no statements.
   It has immutable values and no mutable objects. Functions are values.

 * **Vaporware, not fully implemented:** RCL is a superset of json.

 * **Vaporware, not implemented:** RCL should be statically typed, in the
   sense that errors in unreachable code should cause evaluation to fail.

 * **Vaporware, not implemented:** RCL should be structurally typed with
   Hindley-Milner-style type inference. Types should *not* be first-class,
   in the sense that the type and value namespaces don’t mix, and you can’t
   bind types to variables.

 * Strictly evaluated. Maybe it should become lazily evaluated when you want to
   build something big like Nixpkgs with it; for now strict is fast enough.

## Usage

Build:

    cargo build

Evaluate an RCL expression to json:

    target/debug/rcl eval examples/tags.rcl | jq

Highlight an RCL expression in your terminal:

    target/debug/rcl highlight examples/tags.rcl

Autoformat an RCL expression (non-destructive, prints to stdout):

    target/debug/rcl fmt examples/tags.rcl

## Development

Run all tests and checks below in one command:

    nix flake check

Run golden tests:

    cargo build
    golden/run.py

Check the grammar for ambiguities:

    bison -Werror=all src/grammar.y

Run unit tests and lints:

    cargo test
    cargo clippy

Typecheck Python sources

    mypy --strict .

Check formatting:

    cargo fmt
    black .

View coverage of the golden tests:

    nix build .#coverage --out-link result
    xdg-open result/index.html

Run the fuzzer:

    cargo +nightly-2023-06-03 fuzz run main

## License

RCL is licensed under the [Apache 2.0][apache2] license. It may be used in
free software as well as closed-source applications, both for commercial and
non-commercial use under the conditions given in the license. If you want to
use RCL in your GPLv2-licensed software, you can add an [exception][except]
to your copyright notice. Please do not open an issue if you disagree with the
choice of license.

[apache2]: https://www.apache.org/licenses/LICENSE-2.0
[except]:  https://www.gnu.org/licenses/gpl-faq.html#GPLIncompatibleLibs
