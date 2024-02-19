# rcl format

    rcl format [-w | --width <width>] [-i | --in-place] [--] [<file>...]

Shorthands:

    rcl fmt
    rcl f

## Description

Read an <abbr>RCL</abbr> expression from `<file>` and format it according to the
standard style. When `<file>` is `-`, read from stdin. When no files are
specified, the input defaults to stdin. Print the result to stdout, unless
`--in-place` is used.

## Options

### `-c` `--check`

TODO: This option currently does not exist, but it should. It should make the
program exit with code 1 if formatting is incorrect.

### `-i` `--in-place`

Instead of printing to stdout, rewrite files in-place. When this option is used,
the command accepts multiple input files. Without this option, there must be
exactly one input file.

### `-o` `--output <outfile>`

Write the output to the given file instead of stdout. This option is
incompatible with `--in-place`.

### `-w` `--width <width>`

Target width in columns. Must be an integer. Defaults to 80. Note that the
formatter is not always able to stay within the desired width limit.
