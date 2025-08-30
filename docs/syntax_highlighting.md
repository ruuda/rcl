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
auto-format = false
comment-tokens = ["//"]
file-types = ["rcl"]
formatter = { command = "rcl", args = ["format", "-"] }
grammar = "rcl"
indent = { tab-width = 2, unit = "  " }
name = "rcl"
roots = ["build.rcl"]
scope = "source.rcl"

[[grammar]]
name = "rcl"
source = { git = "https://github.com/ruuda/rcl.git", rev = "master", subpath = "grammar/tree-sitter-rcl" }
```

Furthermore, copy `grammar/tree-sitter-rcl/queries/highlights_helix.scm` into
your Helix configuation directory at `runtime/queries/rcl/highlights.scm`.

[helix-lang]: https://docs.helix-editor.com/guides/adding_languages.html

## Neovim

Neovim can use [the Tree-sitter grammar](#tree-sitter) through the
[nvim-treesitter][nvim-ts] plugin. Clone the `rcl` repository, and add the
following to your `init.lua`:

```lua
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.rcl = {
  install_info = {
    url = "/checkout/rcl/grammar/tree-sitter-rcl",
    files = {},
    generate_requires_npm = false,
    requires_generate_from_grammar = true,
  }
}
```

If you haven’t done so already, configure nvim-treesitter, including [enabling
syntax highlighting][nvim-ts-highlight]. Run `:TSInstall rcl` to compile the
parser and put the shared object on the runtimepath, and copy the highlight
query into the `queries` subdirectory:

```
cp /checkout/rcl/grammar/tree-sitter-rcl/queries/highlights_nvim.scm /pasers-path/queries/rcl/highlights.scm
```

Then either `:set filetype=rcl` on a buffer to highlight as <abbr>RCL</abbr>,
or enable autodetection on the file extension:

```lua
vim.cmd "au BufNewFile,BufRead *.rcl setf rcl"
```

[nvim-ts]: https://github.com/nvim-treesitter/nvim-treesitter
[nvim-ts-highlight]: https://github.com/nvim-treesitter/nvim-treesitter/blob/57205313dda0ac82ac69e21d5e2a80f3297c14cc/README.md#highlight

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

## Zed

The [Zed extension](https://github.com/rcl-lang/zed-rcl) is available from the
Zed extension registry. Open the command panel, search for _zed: extensions_,
then search for _<abbr>RCL</abbr>_ in the search box.

If you want to install a development version of the extension, the extension
is developed in the main repository. You can install it from the Zed command
panel with the _zed: install dev extension_ command, and picking the `grammar/zed`
directory.

## External

Aside from editor support, [`rcl highlight`](rcl_highlight.md) will highlight an
expression using its internal parser.
