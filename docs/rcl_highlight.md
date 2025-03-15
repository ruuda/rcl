# rcl highlight

    rcl highlight [--] [<file>]

## Description

Read an <abbr>RCL</abbr> expression from `<file>`, and print a syntax-highlighted
result to stout. When `<file>` is `-`, read from stdin instead. When no file is
specified, the input defaults to stdin.

Note that [`rcl format`](rcl_format.md) may be a more useful tool. It also
highlights the document, but because it acts on the syntax tree rather than
token stream, it can highlight more details than `rcl highlight`, such as types,
and key-value pairs in dicts. Of course, it does also format the output.

## Options

`rcl highlight` supports [all of the global options](rcl.md#global-options),
in particular `--color` to select the color mode.
