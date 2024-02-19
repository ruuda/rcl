# rcl evaluate

    rcl evaluate [-o | --output <format>] [-w | --width <width>] [--] [<file>]

Shorthands:

    rcl eval
    rcl e   (uses default --output=rcl)
    rcl je  (sets --output=json)

## Description

Read an <abbr>RCL</abbr> expression from the file `<file>`, and evaluate it.
Print the evaluated result to stdout. When `<file>` is `-`, read from stdin.
When no file is specified, the input defaults to stdin.

## Options

### `-o` `--output <format>`

Output in the given format. The following formats are supported:

<dl>
  <dt>json</dt>
  <dd>Output pretty-printed <abbr>JSON</abbr>.</dd>

  <dt>raw</dt>
  <dd>If the document is a string, output the string itself. If the document is
  a list or set of strings, output each string on its own line.</dd>

  <dt>rcl</dt>
  <dd>Output pretty-printed <abbr>RCL</abbr>.</dd>

  <dt>toml</dt>
  <dd>Output <abbr>TOML</abbr>.</dd>

  <dt>yaml-stream</dt>
  <dd>If the document is a list, output every element as a <abbr>JSON</abbr>
  document, prefixed by the <code>---</code> <abbr>YAML</abbr> document
  separator. Top-level values other than lists are not valid for this format.</dd>
</dl>

The default output format is `rcl`. For the `je` command shorthand, the default
output format is `json`.

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
