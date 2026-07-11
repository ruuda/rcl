# Output formats

RCL can print output in the formats below. The format can be selected with
[`--format`](rcl_evaluate.md#-f-format-format) on the command line, and with
[`format`](rcl_build.md#format) in build files.
The format names are written lowercase.

## json

Output pretty-printed <abbr>JSON</abbr>. Sets become lists, functions are not
supported. Because <abbr>RCL</abbr> is a superset of <abbr>JSON</abbr>, every
<abbr>JSON</abbr> value maps to itself. Note that <abbr>YAML</abbr> is a
superset of <abbr>JSON</abbr>, so this format is appropriate for generating
configuration for tools that accept <abbr>YAML</abbr>, such as Kubernetes and
GitHub Actions.

```rcl
{
  series = "Nexus-6",
  instances = {"Roy Batty", "Leon Kowalski"},
}
```
Formats as:
```
{
  "instances": ["Leon Kowalski", "Roy Batty"],
  "series": "Nexus-6"
}
```

> **Tip:** You can quickly select <abbr>JSON</abbr> output on the command line
> with the [`je`](rcl_evaluate.md) and [`jq`](rcl_query.md) shorthands.

## json-lines

If the document is a list, output every element as a <abbr>JSON</abbr> value on
its own line, consistent with the
<a href="https://jsonlines.org/"><abbr>JSON</abbr> lines</a> format.
Top-level values other than lists are not valid for this format.

```rcl
[
  { name = "Roy Batty", serial = "N6MAA10816" },
  { name = "Leon Kowalski", serial = "N6MAC41717" },
]
```
Formats as:
```
{"name": "Roy Batty", "serial": "N6MAA10816"}
{"name": "Leon Kowalski", "serial": "N6MAC41717"}
```

## raw

If the document is a string, output the string itself. If the document is a list
or set of strings, output each string on its own line.

```rcl
["First line", "Second line\nThird line"]
```
Formats as:
```
First line
Second line
Third line
```

> **Tip:** You can quickly select raw output on the command line with the
> [`re`](rcl_evaluate.md) and [`rq`](rcl_query.md) shorthands.

## rcl

Output pretty-printed <abbr>RCL</abbr>.

```rcl
{
  "where_possible": "Record syntax will be used.",
  "where impossible": "Expression form is used.",
}
```
Formats as:
```rcl
{
  "where impossible": "Expression form is used.",
  where_possible = "Record syntax will be used.",
}
```

## systemd

Output as a [systemd unit][sd-syntax]. The format is superficially similar to
<abbr>TOML</abbr>, but handles nested data differently. The top-level value must
always be a dict. Its keys become sections. Its values must be dicts as well,
which become the section contents:

```rcl
{
  Unit = {
    Description = "Example systemd unit.",
    After = "network-online.target",
  },
}
```
```systemd
[Unit]
After=network-online.target
Description="Example systemd unit."
```

Systemd allows repeating keys. To emit those, use a list or set value in
<abbr>RCL</abbr>:

```rcl
{
  Service = {
    BindReadOnlyPaths = ["/etc/resolv.conf", "/var/www"],
  },
}
```
```systemd
[Service]
BindReadOnlyPaths=/etc/resolv.conf
BindReadOnlyPaths=/var/www
```

Some settings allow space-separated values. To emit those, use a nested list
or set:

```rcl
{
  Service = {
    ExecStart = [["/usr/bin/nsd", "-P", "", "-c", "/etc/nsd.conf"]],
  },
},
```
```systemd
[Service]
ExecStart=/usr/bin/nsd -P "" -c /etc/nsd/nsd.conf
```

For some settings, systemd accepts the empty string to clear previous
assignments. While `""` works, using `null` avoids printing the quotes.
The difference between `""` and `null` is purely cosmetic.

```rcl
{
  Service = {
    Environment = [
      null,
      ["LOG_LEVEL=debug", "PORT=8000"],
      "ENVIRONMENT=prod",
    ],
  },
}
```
```systemd
[Service]
Environment=
Environment=LOG_LEVEL=debug PORT=8000
Environment=ENVIRONMENT=prod
```

Finally, some units support repeated sections. To emit those, wrap the sections
in a list or set:

```rcl
{
  Route = [
    { Gateway = "0.0.0.0", Table = 1 },
    { Gateway = "::", Table = 2 },
  ],
}
```
```systemd
[Route]
Gateway=0.0.0.0
Table=1

[Route]
Gateway=::
Table=2
```

RCL will use escape sequences and quote strings when needed to keep the unit
file valid, and to preserve values exactly. For some settings, such as
[`ExecStart=`][sd-start], systemd performs
[environment variable substitution][sd-var] and
[specifier substitution][sd-spec], which means that `$` and `%` have special
meaning for those settings. RCL does not apply any special handling for these
characters.

[sd-syntax]: https://www.freedesktop.org/software/systemd/man/latest/systemd.syntax.html
[sd-start]:  https://www.freedesktop.org/software/systemd/man/latest/systemd.service.html#ExecStart=
[sd-var]:    https://www.freedesktop.org/software/systemd/man/latest/systemd.service.html#Command%20lines
[sd-spec]:   https://www.freedesktop.org/software/systemd/man/latest/systemd.unit.html#Specifiers

## toml

Alias for [`toml-1.0`](#toml-10), for maximum compatibility.

## toml-1.0

Output as <abbr>TOML 1.0</abbr>. This version is the most widely supported, but
can be less readable because inline tables cannot span multiple lines.

```rcl
{
  profile = {
    release = {
      lto = "thin",
      panic = "abort",
      strip = "true",
    },
  },
}
```
Formats as:
```toml
[profile]
release = { lto = "thin", panic = "abort", strip = "true" }
```

## toml-1.1

Output <abbr>TOML 1.1</abbr>. This version supports multi-line tables, but it
was only released in December 2025, so it is less widely supported. The same
example as before outputs as follows:

```toml
[profile]
release = {
  lto = "thin",
  panic = "abort",
  strip = "true",
}
```

## yaml-stream

If the document is a list, output every element as a <abbr>JSON</abbr> document,
prefixed by the <code>---</code> <abbr>YAML</abbr> document separator. Top-level
values other than lists are not valid for this format.

```rcl
[
  { name = "Roy Batty", serial = "N6MAA10816" },
  { name = "Leon Kowalski", serial = "N6MAC41717" },
]
```
Formats as:
```yaml
---
{"name": "Roy Batty", "serial": "N6MAA10816"}
---
{"name": "Leon Kowalski", "serial": "N6MAC41717"}
```
