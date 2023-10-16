# rcl format

    rcl format [-w | --width <width>] [-i | --in-place] [--] <file>...

Shorthands:

    rcl fmt
    rcl f

## Description

Read an RCL expression from `<file>` and format it according to the standard
style. When `<file>` is `-`, read from stdin instead. Print the result to
stdout, unless `--in-place` is used.

## Options

### `-i` `--in-place`

Instead of printing to stdout, rewrite files in-place. When this option is used,
the command accepts multiple input files. Without this option, there must be
exactly one input file.

### `-c` `--check`

TODO: This option currently does not exist, but it should. It should make the
program exit with code 1 if formatting is incorrect.

### `-w` `--width <width>`

Target width in columns. Must be an integer. Defaults to 80. Note that the
formatter is not always able to stay within the desired width limit.
