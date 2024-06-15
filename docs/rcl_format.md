# rcl format

    rcl format [-w | --width <width>] [-i | --in-place] [--] [<file>...]

Shorthands:

    rcl fmt
    rcl f

## Description

Read an <abbr>RCL</abbr> expression from `<file>` and format it according to the
standard style. When `<file>` is `-`, read from stdin. When no files are
specified, the input defaults to stdin.

In the default mode, there must be exactly one input file, and the formatted
result is printed to stdout. With `--in-place` and `--check`, you can provide
multiple input files.
## Options

### `--check`

Report whether any files would be reformatted. If so, exit with exit code 1.
When all files are already formatted correctly, exit with exit code 0. This
can be used on <abbr>CI</abbr> or in a Git pre-commit hook to ensure that
<abbr>RCL</abbr> files are formatted in the standard style.

When this option is used, the command accepts multiple input files. This option
is incompatible with `--in-place`.

### `-i` `--in-place`

Instead of printing to stdout, rewrite files in-place.

When this option is used, the command accepts multiple input files. This option
is incompatible with `--check`.

### `-o` `--output <outfile>`

Write the output to the given file instead of stdout. When [`--directory`][dir]
is set, the output path is relative to that directory.

This option is incompatible with `--check` and `--in-place`.

[dir]: rcl.md#-c-directory-dir

### `-w` `--width <width>`

Target width in columns. Must be an integer. Defaults to 80. Note that the
formatter is not always able to stay within the desired width limit.

## The standard style

The output of `rcl format` should generally be sensible and readable, though as
with any mechanical formatter, it cannot please everybody for every possible
input. The format is not configurable aside from the target [width](#-w-width-width).
Although the formatter tries to not exceed the target width, it is not always
possible to stay within the limit.

The formatter is not sensitive to initial formatting, with the exception of
trailing commas, to give some control over how collections are formatted.[^1]
Collections with at least two elements that have a trailing comma will be
formatted tall, even when they fit on a line. To format the collection wide,
remove the trailing comma. This applies to any place where trailing commas are
allowed, not just collection literals.

```rcl
// This collection is formatted wide.
let xs = [1, 2];

// Even though it fits on one line, this collection is kept tall due to the
// trailing comma.
let ys = [
  1,
  2,
];
```

[^1]: This was inspired by how the Black Python formatter
      [treats trailing trailing commas][magic-comma].
[magic-comma]: https://black.readthedocs.io/en/stable/the_black_code_style/current_style.html#the-magic-trailing-comma
