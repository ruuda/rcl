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

Even though `src/grammar.json` is a generated file, we commit it to the
repository, so the grammar can be used without a complex build process.

[ts-user-config]: https://tree-sitter.github.io/tree-sitter/syntax-highlighting#per-user-configuration

## Using the grammar

TODO: This part is also not clear to me! Can we point Helix or Neovim at the
directory and just expect it to work?
