# Testing

RCL is tested extensively. The two primary means of testing are golden tests
and fuzzing.

## Golden tests

Golden tests are located in the `golden` directory as files ending in `.test`.
Every file consists of two sections: an input, then a line

    # output:

and then an expected output. The test runner `golden/run.py` takes these files,
executes <abbr>RCL</abbr>, and compares the actual output against the expected
output. The mode in which `run.py` executes <abbr>RCL</abbr> depends on the
subdirectory that the tests are in. See the docstring in `run.py` for more
information.

The goal of the golden tests is to cover all relevant branches of the code. For
example, every error message that <abbr>RCL</abbr> can generate should be
accompanied by a test that triggers it. It is instructive to inspect the
coverage report to see what tests are missing, or to discover pieces of dead
code. The easiest way to generate the report is with Nix:

    nix build .#coverage --out-link result
    xdg-open result/index.html

Alternatively, you can build with coverage support and run `grcov` manually,
see `flake.nix` for the exact flags and commands.

## Fuzz tests

The other means of testing are fuzz tests. RCL contains two primary fuzzers
called `fuzz_source` and `fuzz_smith`. The input to the source-based fuzzer
are valid <abbr>RCL</abbr> files that contain a little bit of metadata about
what mode to exercise. Putting everything in one fuzzer enables sharing the
corpus across the different modes. The input to the smith-based fuzzer are
small bytecode programs for a _smith_ that synthesizes <abbr>RCL</abbr> programs
from them. The smith fuzzer is a lot faster than the source-based fuzzer,
because it does not have to overcome trivial obstacles like having balanced
parentheses or using the same name in a variable definition and usage site.
Getting past such obstacles is hard for the source-based fuzzer because it
requires two separate mutations to work together to make a valid file. The
downside of the smith-based fuzzer is that it does not explore the full input
space, in particular regarding parse errors, so that’s why a combination of both
fuzzers is ideal.

The basic modes of the fuzzer just test that the lexer, parser, and evaluator
do not crash. They are useful for finding mistakes such as slicing a string
across a code point binary, or trying to consume tokens past the end of the
document. However the more interesting fuzzers are the formatter, and the value
pretty-printer. They verify the following properties:

 * **The formatter is idempotent.** Running `rcl fmt` on an already-formatted
   file should not change it. This sounds obvious, but the fuzzer caught a few
   interesting cases related to trailing whitespace and multiline strings.
 * **The evaluator is idempotent.** RCL can evaluate to json, and is itself a
   superset of json, so evaluating the same input again should not change it.
 * **The formatter agrees with the pretty-printer.** When <abbr>RCL</abbr>
   evaluates to json, it pretty-prints the json document using the value pretty
   printer. The formatter that formats source files on the other hand,
   pretty-prints the concrete syntax tree. This check tests that formatting a
   pretty-printed json value is a no-op.
 * **The json output can be parsed by Serde.** Even if <abbr>RCL</abbr> can read
   back a json output, that is no guarantee that a third-party parser considers
   the output valid, so we also test against the widely used Serde deserializer.
 * **The toml output can be parsed by Serde.** Same as above, but for toml
   output.

## Running the fuzzers

To run the fuzzers you need [`cargo-fuzz`][cargo-fuzz] and a nightly toolchain.
Then start e.g. the smith fuzzer or source-based fuzzer:

    cargo +nightly-2023-11-09 fuzz run fuzz_smith -- -timeout=3
    cargo +nightly-2023-11-09 fuzz run fuzz_source -- -dict=fuzz/dictionary.txt -timeout=3

Unlike the other fuzzers, the inputs to the smith fuzzer are not human-readable.
To see what kind of inputs the fuzzer is exploring, you can use the `smithctl`
program to interpret the smith programs:

    cargo build --manifest-path=fuzz/Cargo.toml --bin smithctl

To print all inputs discovered in the past 5 minutes:

    fd . fuzz/corpus/fuzz_smith --changed-within 5m --exec target/debug/smithctl print

[cargo-fuzz]: https://github.com/rust-fuzz/cargo-fuzz

## Unit tests

There is less of a focus on unit tests. Constructing all the right environment
and values for even a very basic test quickly becomes unwieldy; even the
<abbr>AST</abbr> of a few lines of code, when constructed directly in Rust,
becomes hundreds of lines of code. Rather than constructing the <abbr>AST</abbr>
by hand, we can just use the parser to construct one. Similarly, for expected
output values, instead of constructing these in Rust, we can format them, and
express the entire process as a golden test instead.
