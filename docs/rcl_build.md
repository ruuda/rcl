# rcl build

    rcl build [--] [<buildfile>]

## Description

The _build_ command writes formatted <abbr>RCL</abbr> values to files, as
specified in a build file. It can be used to update multiple generated files in
one command, similar to a build tool like Make or Ninja, but with the build
targets specified in <abbr>RCL</abbr> rather than a makefile.

TODO: Link to a docs chapter like the Ninja one.

When no file is specified, `rcl build` reads from `build.rcl` as the default.
(This is unlike other <abbr>RCL</abbr> subcommands, which default to stdin.)
When `<buildfile>` is `-`, read from stdin.

## Build files

A build file is an <abbr>RCL</abbr> document that contains a dictionary of build
targets. Every key is one output path. This path is relative to the location of
the build file itself. The value is a dict with the following schema:

```rcl
type Target = {
  banner: Bool,
  contents: Any,
  format: String,
  width: Int,
}
```

## Example

The following `build.rcl` writes two files to the `users` directory:
`rachael.toml` and `rbatty.toml`:

```rcl
{
  "users/rachael.toml": {
    contents = { name = "Rachael Tyrell", generation = 7 },
    format = "toml",
  },
  "users/rbatty.toml": {
    contents = { name = "Roy Batty", generation = 6 },
    format = "toml",
  },
}
```

## Options

### `--sandbox <mode>`

See [`--sandbox` in `rcl evaluate`](rcl_evaluate.md#-sandbox-mode). Sandbox
requirements apply to output paths as well as input paths. In _workdir_ mode,
<abbr>RCL</abbr> will not write outside the working directory.
