# rcl build

    rcl build [--] [<buildfile>]

## Description

The build command writes evaluated documents to files. It can be used to
update many generated files in one command, similar to a build tool like Make
or Ninja, but with the build targets specified in RCL rather than a Makefile.

TODO: Link to a docs chapter like the Ninja one.

When no file is specified, `rcl build` reads from `build.rcl` as the default.
(This is unlike other <abbr>RCL</abbr> subcommands, which default to stdin.)
When `<buildfile>` is `-`, read from stdin.

## Build target schema

TODO: Document.

## Options

### `--sandbox <mode>`

See [`--sandbox` in `rcl evaluate`][rcl_evaluate.md#-sandbox-mode].
