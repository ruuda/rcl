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

### `--sandbox <mode>`

Limit which files can be imported in [import expressions](imports.md#security).
Three modes are available:

<dl>
  <dt>pure</dt>
  <dd>Only allow importing files specified on the command-line with
  <code>--include</code>. Do not allow any filesystem access aside from these
  includes.</dd>
  <dt>workdir</dt>
  <dd>Only allow importing files inside the working directory, including
  subdirectories. For example, when <code>rcl</code> is executed in
  <code>/home/user/exprs</code>, importing <code>/home/user/exprs/a/b.rcl</code>
  is allowed, but importing <code>/home/user/.config/private.rcl</code> is not.
  </dd>
  <dt>unrestricted</dt>
  <dd>Grant unrestricted filesystem access, allow importing any file.</dd>
</dl>

The default sandboxing mode is _workdir_.

### `-I` `--include <alias>:<file>`

Enables importing the given `<file>` even with `--sandbox=pure`. To import the
file, the import expression must be of the form `import "<alias>"`. For example,
the following call prints the evaluation of `a.rcl`:

    echo 'import "a"' | rcl eval - --sandbox=pure --include=a:a.rcl

This option can be specified multiple times.
