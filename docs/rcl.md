# rcl

    rcl [--version] [-h | --help] [--color <mode>] <command> [<args>]

## Description

The `rcl` executable; see the commands for more details:

 * [evaluate](rcl_evaluate.md)
 * [format](rcl_format.md)
 * [highlight](rcl_highlight.md)
 * [query](rcl_query.md)

## Global options

The global options apply to every command. Global options can be used before as
well as after the command.

### `--color <mode>`

Set how output is colored. The following modes are available:

<dl>
  <dt>ansi</dt>
  <dd>Always color output using <abbr>ANSI</abbr> escape codes.</dd>
  <dt>auto</dt>
  <dd>Use <abbr>ANSI</abbr> if the output file is a <abbr>TTY</abbr> and the
  <a href="https://no-color.org/"><code>NO_COLOR</code></a> environment variable
  is not set to a non-empty string. This is the default.</dd>
  <dt>none</dt>
  <dd>Do not color output at all.</dd>
</dl>
