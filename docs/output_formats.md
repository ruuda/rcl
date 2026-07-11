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
