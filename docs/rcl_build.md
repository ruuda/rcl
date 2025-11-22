# rcl build

    rcl build [--] [<buildfile>]

## Description

The _build_ command writes formatted values to files, as specified in a build
file. It can be used to update multiple generated files in one command, similar
to a build tool like Make or Ninja, but with the build targets specified in
<abbr>RCL</abbr> rather than a makefile.

RCL’s build support is a lightweight alternative to [using an external build tool
such as Ninja](using_ninja.md). It is simplistic: it has no ability to call
external programs, and it will rewrite all outputs even when the inputs did
not change. For large configurations this might be an issue, but for quickly
replacing a few repetitive <abbr>YAML</abbr> files with generated ones,
`rcl build` can be a quick way to adopt <abbr>RCL</abbr> without introducing
multiple new tools at once.

When no file is specified, `rcl build` reads from `build.rcl` as the default.
This is unlike other <abbr>RCL</abbr> commands, which default to stdin.
When `<buildfile>` is `-`, read from stdin.

## Example

The following `build.rcl` writes two files to the `users` directory:
`rachael.toml` and `rbatty.toml`:

```rcl
let default_options = {
  banner = "# This file is generated from build.rcl.",
  format = "toml",
};
{
  "users/rachael.toml": {
    ...default_options,
    contents = { name = "Rachael Tyrell", generation = 7 },
  },
  "users/rbatty.toml": {
    ...default_options,
    contents = { name = "Roy Batty", generation = 6 },
  },
}
```

For a more elaborate introduction, see the [generating files chapter](generating_files.md).

## Build files

A build file is an <abbr>RCL</abbr> document that contains a dictionary of build
targets. Every key is one output path. This path is relative to the location of
the build file itself. The value is a dict with the following schema:

```rcl
type Target = {
  banner: Union[String, Null],
  contents: Any,
  format: String,
  width: Number,
}
```

The following fields are supported:

### banner

A string to prepend to the output. This can be useful to add a comment to
clarify that a file is generated, and point readers at the original source.
This field is optional and defaults to `null`, which means no banner is emitted.
If the banner is not null, a newline is implicitly added between the banner and
output. Corresponds to [`--banner`](rcl_evaluate.md#-banner-message).

### contents

The value to write to the file in the desired format.

### format

The output format (`json`, `toml`, etc.). This must be one of the formats
supported by [`--format`](rcl_evaluate.md#-f-format-format).

### width

The target width for pretty-printing in columns. See also
[`--width`](rcl_evaluate.md#-w-width-width).
This field is optional and defaults to 80.

## Options

### `--check`

Report whether any files would be created or rewritten. If so, exit with exit
code 1. When all target files are already up to date, exit with exit code 0.
This can be used on <abbr>CI</abbr> or in a Git pre-commit hook to ensure that
generated files which are checked in to a repository are up to date.

### `--dry-run`

By default, `rcl build` writes the evaluated contents of a build target to the
target file, overwriting it if it exists. With `--dry-run`, `rcl build` prints
the contents that it would write to the file to stdout instead.

### `--sandbox <mode>`

See [`--sandbox` in `rcl evaluate`](rcl_evaluate.md#-sandbox-mode). Sandbox
requirements apply to output paths as well as input paths. In _workdir_ mode,
<abbr>RCL</abbr> will not write outside the working directory.
