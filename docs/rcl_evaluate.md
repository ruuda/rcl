# rcl evaluate

    rcl evaluate [-o | --output <format>] [-w | --width <width>] [--] <file>

Shorthands:

    rcl eval
    rcl e

## Description

Read an RCL expression from the file `<file>`, and evaluate it. When `<file>`
is `-`, read from stdin instead. Print the evaluated result to stdout.

## Options

### `-o` `--output <format>`

Output in the given format. Can be one of `json` or `rcl`. Defaults to `rcl`.

### `-w` `--width <width>`

Target width for pretty-printing, in columns. Must be an integer. Defaults to 80.
