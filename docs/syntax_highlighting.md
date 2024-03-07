# Syntax highlighting

Definitions for syntax coloring are available for the applications below.

## Emacs

See <https://github.com/qezz/rcl-mode>.

## Helix

Helix can use [the Tree-sitter grammar](#tree-sitter). In your configuration
directory, ensure [`languages.toml`][helix-lang] exists, and add the following
sections:

```toml
[[language]]
name = "rcl"
auto-format = false
file-types = ["rcl"]
formatter = { command = "rcl", args = ["format", "-"] }
indent = { tab-width = 2, unit = "  " }
roots = ["build.rcl"]
scope = "source.rcl"
grammar = "rcl"

[[grammar]]
name = "rcl"
source = { git = "https://github.com/ruuda/rcl.git", rev = "master", subpath = "grammar/tree-sitter-rcl" }
```

Furthermore, copy `grammar/tree-sitter-rcl/queries/highlights_helix.scm` into
your Helix configuation directory at `runtime/queries/rcl/highlights.scm`.

[helix-lang]: https://docs.helix-editor.com/guides/adding_languages.html

## Neovim

Neovim should be able to use [the Tree-sitter grammar](#tree-sitter), though I
haven't yet looked into how.

## Pygments

The directory `grammar/pygments` contains a file `rcl.py` that you can drop into
a Pygments fork in the `pygments/lexers` directory. This lexer powers the syntax
highlighting in this manual.

## Tree-sitter

The directory `grammar/tree-sitter-rcl` contains a [Tree-sitter][tree-sitter]
grammar. It can be used by various tools, see the other sections on this page.
For hacking on the grammar, see also [the Tree-sitter chapter](tree_sitter.md).

[tree-sitter]: https://tree-sitter.github.io/tree-sitter/

## Vim

The directory `grammar/rcl.vim` contains support for highlighting in Vim.
You can symlink the contents into your `~/.vim`, or use a plugin manager like
Pathogen and symlink the directory into `~/.vim/bundle`.

## External

Aside from editor support, [`rcl highlight`](rcl_highlight.md) will highlight an
expression using its internal parser.
