# Tree-sitter

The repository includes a [Tree-sitter][tree-sitter] grammar at
`grammar/tree-sitter-rcl`. This chapter is about hacking on the grammar.
For editors that support this grammar, see [the syntax highlighting chapter](syntax_highlighting.md).

[tree-sitter]: https://tree-sitter.github.io/tree-sitter/

## Editing the grammar

Tree-sitter grammars are written in Javascript. Regenerating the parser involves
executing Javascript outside of a browser, but fortunately no <abbr>NPM</abbr>
packages or `node_modules` are involved. The Nix development environment
includes all the tools that are needed.

If you get Tree-sitter from the Nix development environment, make sure to set
`CC` to a compiler provided by Nix as well. Without this, the resulting shared
object links against a `libstdc++.so.6` that cannot be located.

    export CC="$NIX_CC/bin/gcc" CXX="$NIX_CC/bin/g++"

Then regenerate the `src/grammar.json`, and generate the other necessary files:

    cd grammar/tree-sitter-rcl
    tree-sitter generate --build

Test the parse tests:

    tree-sitter test

Now we can try to parse a file:

    tree-sitter parse ../../examples/tags.rcl

Even highlight it, after [adding the `rcl/grammar` directory to your Tree-sitter
per-user configuration][ts-user-config].

    tree-sitter highlight ../../examples/tags.rcl

Compile and test the Rust bindings:

    cargo test

[ts-user-config]: https://tree-sitter.github.io/tree-sitter/syntax-highlighting#per-user-configuration

## Queries

The Tree-sitter queries for highlighting are editor-specific, because editors
look for different scopes.

 * [Helix supported scopes](https://docs.helix-editor.com/themes.html#syntax-highlighting)
 * [Neovim supported scopes](https://neovim.io/doc/user/treesitter#treesitter-highlight)
 * [Tree-sitter <abbr>CLI</abbr> supported scopes](https://github.com/tree-sitter/tree-sitter/blob/v0.22.1/highlight/README.md)
 * [Zed supported scopes](https://zed.dev/docs/extensions/languages#syntax-highlighting)

## Testing

Aside from Tree-sitter specific tests, there is a fuzzer that verifies that
any output that is accepted by <abbr>RCL</abbr>’s parser is also accepted by
Tree-sitter. It can help to ensure that the grammars remain in sync. To run it:

    cargo +nightly-2023-06-03 fuzz run fuzz_tree_sitter

See also the [testing chapter](testing.md#running-the-fuzzers).

## Using the parser

See the [syntax highlighting chapter](syntax_highlighting.md) for how to put the
parser to use.

## External repository

The `grammar/tree-sitter-rcl` directory in the repository is the source of truth
for the grammar. It is in the main repository to make it easy to keep in sync
with the parser. Unfortunately many tools based on Tree-sitter assume that
Tree-sitter grammars live in a dedicated repository, and often they expect
generated files to be checked in too. For this purpose, there is a dedicated
repository: <https://github.com/rcl-lang/tree-sitter-rcl>. The script
`tools/update_repos.py` can update a checkout of this repository automatically.
