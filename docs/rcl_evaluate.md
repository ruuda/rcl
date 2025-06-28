# rcl evaluate

    rcl evaluate [-f | --format <format>] [--] [<file>] [--output <outfile>]

Shorthands:

    rcl eval
    rcl e   (uses default --format=rcl)
    rcl je  (sets --format=json)
    rcl re  (sets --format=raw)

## Description

Read an <abbr>RCL</abbr> expression from the file `<file>`, and evaluate it.
Print the evaluated result to stdout. When `<file>` is `-`, read from stdin.
When no file is specified, the input defaults to stdin.

## Options

### `--banner <message>`

Prepend the banner message to the output. This can be useful to add a comment
to a generated file to clarify that the file is generated. RCL implicitly adds
a line break between the banner and the output.

### `-f` `--format <format>`

Output in the given format. The following formats are supported:

<dl>
  <dt>json</dt>
  <dd>Output pretty-printed <abbr>JSON</abbr>.</dd>

  <dt>json-lines</dt>
  <dd>If the document is a list, output every element as a <abbr>JSON</abbr>
  value on its own line, consistent with the
  <a href="https://jsonlines.org/"><abbr>JSON</abbr> lines</a> format.
  Top-level values other than lists are not valid for this format.</dd>

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

### `--output-depfile <depfile>`

Write the names of the files that were loaded during evaluation in Makefile
syntax to the file `<depfile>`. This can be used by build systems to re-run
`rcl` when one of the inputs changes. See also [the depfile section of the
Ninja documentation][ninja-depfile]. Because the depfile includes the name of
the dependent file, this option can only be used in combination with `--output`.

[ninja-depfile]: https://ninja-build.org/manual.html#_depfile

### `-o` `--output <outfile>`

Write the output to the given file instead of stdout. When [`--directory`][dir]
is set, the output path is relative to that directory. [`--color`][color] does
not apply when using `--output`.

[dir]:   rcl.md#-c-directory-dir
[color]: rcl.md#-color-mode

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

### `-w` `--width <width>`

Target width for pretty-printing, in columns. Must be an integer. Defaults to 80.
