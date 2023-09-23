# rcl highlight

    rcl highlight <input>

## Description

Read an RCL expression from the file `<input>`, and print a syntax-highlighted
result to stout. When `<input>` is `-`, read from stdin instead.

## Options

TODO: There should be an option to write html instead of ansi escape codes, or
some other format that could easily be consumed, for example to integrate with
Pygments. Or maybe only some directives to compare against an external lexer.
