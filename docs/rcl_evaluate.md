# rcl evaluate

    rcl evaluate [-o | --output <format>] [-w | --width <width>] [--] [<file>]

Shorthands:

    rcl eval
    rcl e   (uses default --output=rcl)
    rcl je  (sets --output=json)

## Description

Read an <abbr>RCL</abbr> expression from the file `<file>`, and evaluate it.
Print the evaluated result to stdout. When `<file>` is `-`, read from stdin.
When no file is specified and stdin is not a <abbr>TTY</abbr>, the input
defaults to stdin.[^1]

[^1]: When stdin is a <abbr>TTY</abbr>, it is not the default input, to avoid
      confusing new users, who might not realize that `rcl` is waiting for an
      <abbr>EOF</abbr> on stdin. If using stdin is intentional, specify `-` as
      the file.

## Options

### `-o` `--output <format>`

Output in the given format. Can be one of `json` or `rcl`. Defaults to `rcl`.

### `-w` `--width <width>`

Target width for pretty-printing, in columns. Must be an integer. Defaults to 80.

### `--sandbox <mode>`

Limit which files can be imported in [import expressions](imports.md#security).
Two modes are available:

<dl>
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
