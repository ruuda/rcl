<h1 align="center">The RCL Configuration Language</h1>
<p align="center">
<a href="#getting-started">Getting Started</a> ·
<a href="https://docs.ruuda.nl/rcl/">Documentation</a> ·
<a href="./docs/changelog.md">Changelog</a> ·
<a href="https://rcl-lang.org/#try-it-yourself">Online Playground</a>
</p>

RCL is a domain-specific language for generating configuration files and
querying json documents. It is a superset of json that extends it into a simple,
gradually typed, functional programming language that resembles Python and Nix.

RCL can be used through the [`rcl`][cmd] command-line tool that can export
documents to json, yaml, toml, [and more][output]. It can also be used through
a native Python module, with an interface similar to the `json` module.

## About

RCL solves the following problems:

 * Copy-pasted yaml blocks that differ by a single value.
 * Broken configs due to whitespace and escaping footguns in templating engines.
 * Drift between tools due to lack of a single source of truth.
 * Struggling to remember `jq` syntax.

It does that as follows:

 * **A real language.**
   Use variables, loops, imports, and functions to eliminate duplication.
 * **Familiar syntax.**
   Have you used Python, TypeScript, or Rust before? Then you will find RCL
   obvious to read and natural to write.
 * **Generate rather than template.**
   Manipulate data structures, not strings.
   Generate correct json, yaml, and toml.
 * **Built to integrate.**
   Generate configs for tools that do not natively talk to each other, all
   from a single source of truth. Integrate with your existing build tools,
   use the Python module, or the built-in [`rcl build`][cmd-build] to update
   generated files.
 * **Gradual types.**
   Add type annotations where they help to eliminate bugs and make code
   self-documenting, omit them in straightforward code.
 * **Built-in json queries.**
   A language for manipulating structured data makes a pretty good query tool.
   Run map and filter pipelines straight from your terminal.

[cmd]:       https://docs.ruuda.nl/rcl/rcl/
[cmd-build]: https://docs.ruuda.nl/rcl/rcl_build/#
[output]:    https://docs.ruuda.nl/rcl/rcl_evaluate/#-o-output-format

## Example

Given this input:

```rcl
{
  // Generate backup buckets for each database and retention period.
  backup_buckets = [
    let retention_days = { hourly = 4, daily = 30, monthly = 365 };
    for database in ["alpha", "bravo"]:
    for period, days in retention_days:
    {
      name = f"{database}-{period}",
      region = "eu-west",
      lifecycle_policy = { delete_after_seconds = days * 24 * 3600 },
    }
  ],
}
```

RCL generates:

```jsonc
{
   "backup_buckets": [
      {
         "name": "alpha-hourly",
         "region": "eu-west",
         "lifecycle_policy": { "delete_after_seconds": 345600 }
      },
      {
         "name": "alpha-daily",
         "region": "eu-west",
         "lifecycle_policy": { "delete_after_seconds": 2592000 }
      },
      // And 4 similar entries, omitted here for brevity.
   ]
}
```

For an interactive demo in your browser, see <https://rcl-lang.org>.

## Getting started

After the interactive examples [on the website](https://rcl-lang.org/),
[the manual](https://docs.ruuda.nl/rcl/) is the best resource for further
information. The most useful chapters to get started:

 * [Installation](https://docs.ruuda.nl/rcl/installation/)
 * [Tutorial](https://docs.ruuda.nl/rcl/tutorial/)
 * [Syntax guide](https://docs.ruuda.nl/rcl/syntax/)

You may also find the examples in the `examples` directory instructive.
Some helpful commands after a clone:
```bash
# Build
cargo build --release

# Print usage
target/release/rcl
target/release/rcl eval --help

# Evaluate an RCL expression to json
target/release/rcl eval --format=json examples/tags.rcl

# Query an RCL or json document
target/release/rcl query examples/tags.rcl input.tags.ams01

# Autoformat and highlight an RCL expression (non-destructive, prints to stdout)
target/release/rcl fmt examples/tags.rcl
```

## Status

RCL is a hobby project without stability promise. It is usable and useful,
well-tested, and well-documented, but also still experimental, and it may have
breaking changes. Syntax highlighting is available for major editors like Vim,
Emacs, Helix, and Zed.

## Support RCL

One thing that holds RCL back from being useful to more people is the lack of
widespread support for syntax highlighting on platforms such as GitHub. If RCL
is useful to you, you can help by using RCL publicly in a GitHub repository
[to demonstrate traction][linguist]. Use it seriously of course, please don’t
game the metric. Other things you can help with are getting RCL packaged for
your favorite package manager, and developing syntax highlighting for your
favorite editor if it is not yet supported.

[linguist]: https://github.com/github-linguist/linguist/blob/4ac734c15a96f9e16fd12330d0cb8de82274f700/CONTRIBUTING.md#adding-a-language

## Development

Run all tests and checks below in one command:

    nix flake check

Run golden tests:

    cargo build
    golden/run.py

Run unit tests and lints:

    cargo test
    cargo clippy

Typecheck Python sources

    mypy --strict --exclude pyrcl .
    mypy --strict pyrcl

Check formatting:

    cargo fmt
    black .

View coverage of the golden tests:

    nix build .#coverage --out-link result
    xdg-open result/index.html

For how to run the fuzzers, see [`docs/testing.md`](docs/testing.md).

## Building WASM

See [the readme in the `wasm` directory](wasm/README.md).

## License

RCL is licensed under the [Apache 2.0][apache2] license. It may be used in
free software as well as closed-source applications, both for commercial and
non-commercial use under the conditions given in the license. If you want to
use RCL in your GPLv2-licensed software, you can add an [exception][except]
to your copyright notice. Please do not open an issue if you disagree with the
choice of license.

[apache2]: https://www.apache.org/licenses/LICENSE-2.0
[except]:  https://www.gnu.org/licenses/gpl-faq.html#GPLIncompatibleLibs
