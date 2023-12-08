# rcl highlight

    rcl highlight [--] [<file>]

## Description

Read an <abbr>RCL</abbr> expression from `<file>`, and print a syntax-highlighted
result to stout. When `<file>` is `-`, read from stdin instead. When no file is
specified and stdin is not a <abbr>TTY</abbr>, the input defaults to stdin.[^1]

[^1]: When stdin is a <abbr>TTY</abbr>, it is not the default input, to avoid
      confusing new users, who might not realize that `rcl` is waiting for an
      <abbr>EOF</abbr> on stdin. If using stdin is intentional, specify `-` as
      the file.

## Options

TODO: There should be an option to write html instead of ansi escape codes, or
some other format that could easily be consumed, for example to integrate with
Pygments. Or maybe only some directives to compare against an external lexer.
