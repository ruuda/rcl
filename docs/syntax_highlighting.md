# Syntax highlighting

The repository ships with basic syntax definitions for the applications below.

## Pygments

The directory `etc/pygments` contains a file `rcl.py` that you can drop into a
Pygments fork in the `pygments/lexers` directory. This lexer powers the syntax
highlighting in this manual.

## Vim

The directory `etc/rcl.vim` contains support for highlighting in Vim. You can
symlink the contents into your `~/.vim`, or use a plugin manager like Pathogen
and symlink the directory into `~/.vim/bundle`.

## External

Aside from editor support, [`rcl highlight`](rcl_highlight.md) will highlight an
expression using its internal parser.
