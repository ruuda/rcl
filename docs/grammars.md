# Grammars

The `grammar` directory contains various versions of the <abbr>RCL</abbr>
grammar, as well as plugins and extensions for editors and other tools to add
support for <abbr>RCL</abbr>. If you are interested in using those, see the
[syntax highlighting chapter][sh].

[sh]: syntax_highlighting.md

The source of truth for <abbr>RCL</abbr>'s grammars are the lexer and parser in
`src/lexer.rs` and `src/parser.rs`. These contain a hand-written lexer
and recursive descent parser. The parser produces a _concrete syntax tree_
(<abbr>CST</abbr>) and the same parser is shared by the evaluator and
autoformatter.

## Bison

In `bison` there is a Bison grammar. It can be compiled to check for errors,
but the goal is not to be used directly; the goal is to provide a readable yet
precise specification of the grammar, and to act as an aid in designing the
grammar, to avoid making ad-hoc decisions in the hand-written parser that are
difficult to parse with more general tools.

## Tree-sitter

The `tree-sitter-rcl` directory contains a Tree-sitter grammar that forms the
basis of various editor integrations. It has [its own chapter][ts].

[ts]: tree_sitter.md

## Vim

The directory `rcl.vim` contains the Vim plugin. The final `rcl.vim` is
generated from a template by `tools/generate_keywords.py`, so the source of
truth for builtins to highlight can be kept in a single place.

The Vim documentation contains [a section with standard group names][vim-groups]
to use for highlighting, similar to scopes for Tree Sitter.

[vim-groups]: https://vimhelp.org/syntax.txt.html#group-name

## Zed

The `zed` directory contains the Zed plugin. This directory is the source of
truth for the plugin. We subsequently export the directory, including generated
Tree-sitter files, into [an external repository][zed-rcl]. The script
`tools/update_repos.py` syncs changes from this repository into an external
checkout.

[zed-rcl]: https://github.com/rcl-lang/zed-rcl
