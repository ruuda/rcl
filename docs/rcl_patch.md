# rcl patch

    rcl patch [-i | --in-place] [--] [<file>] <path> <replacement>

## Description

The `patch` command edits an <abbr>RCL</abbr> document, to replace the
expression identified by [`<path>`](#path-matching) with the given
`<replacement>`. This can be used to make automation edit a configuration that
is otherwise written by hand. This command formats the new document in standard
style, like [`rcl format`](rcl_format.md) would.

When `<file>` is `-`, this command reads from stdin. When no file is specified,
the input defaults to stdin. In the default mode, the patched and formatted
result is printed to stdout. With `--output` the output can be written to a file
instead, and with `--in-place`, `rcl patch` rewrites the input file.

Use good judgment when integrating `rcl patch` into your automation: it may be
simpler to [`import`](syntax.md#imports) separate files managed by automation,
than to have a single file be managed by humans and automation. See also the
[alternatives below](#alternatives).

## Example

Consider the file `example.rcl`:

```rcl
let app = { version = "1.33.7" };
app
```

The following command:

```
rcl patch --in-place example.rcl app.version '"1.42.0"'
```

Would rewrite the file to:

```rcl
let app = { version = "1.42.0" };
app
```

Note that we use single quotes around the replacement value, to ensure that the
shell passes `"1.42.0"` to <abbr>RCL</abbr>, including the double quotes.

## Options

### `--check`

Report whether the target file would be changed. If so, exit with exit code 1.
When the patch operation is a no-op, and the file is already formatted correctly,
exit with code 0.

This option is incompatible with `--in-place` and `--output`.

### `-i` `--in-place`

Instead of printing to stdout, rewrite files in-place.

This option is incompatible with `--check` and `--output`.

### `-o` `--output <outfile>`

Write the output to the given file instead of stdout. When [`--directory`][dir]
is set, the output path is relative to that directory.

This option is incompatible with `--check` and `--in-place`.

[dir]: rcl.md#-c-directory-dir

### `-w` `--width <width>`

Target width in columns for formatting. Must be an integer. Defaults to 80. Note
that the formatter is not always able to stay within the desired width limit.


## Path matching

The _path_ identifies the place in the document to patch. It consists of a list
of [identifiers][ident] separated by dots. Every identifier can match either:

* A let-binding.
* A key in a dictionary, when that key is using [record form][dict],
  i.e. `key = value` rather than `"key": value`.

[ident]: syntax.md#identifiers
[dict]: syntax.md#dictionaries

Matching is greedy: when an identifier matches, we try to match the remainder
of the path inside the matching expression. If that fails, that’s an error, even
if the path could still match later in the document.

Let’s look at an artificial example:

```rcl
let alpha = {
  let bravo = {
    charlie = 1,
    "delta": 2,
  };
  echo = 3,
};
{
  alpha = { bravo = { foxtrot = 4 } },
  zulu = alpha,
}
```

In this document, we can match the following paths:

`alpha.bravo.charlie` matches `1`: `alpha` and `bravo` match the first two
let-bindings, while `charlie` matches the dict key.

`alpha.bravo.delta` does not match: we can only match dict keys that use record
notation, but `"delta"` is using json form.

`alpha.bravo.foxtrot` does not match: matching is greedy, so `alpha` and `bravo`
match the first two let-bindings, and inside the matching dict, there is no key
`foxtrot`.

`zulu` matches the final dict entry.

`zulu.echo` does not match: patching acts on the syntax tree, before any evaluation.

While these limitations mean that paths cannot reference arbitrary locations in
an <abbr>RCL</abbr> document, it is always possible introduce a let-binding
earlier in the document, that can be matched.

## Use cases and alternatives

The goal of `rcl patch` is to enable automation to edit files that are primarily
maintained by humans. For example, to bump a version number, or to update a
pinned commit hash. RCL already features a more general way to enable automation
to update a configuration: write the automation-managed parts to separate files,
and import them with [`import`](syntax.md#imports) or [`std.read_file_utf8`](stdlib.md#read_file_utf8).
Automation can rewrite those entire files, which is simpler than safely editing
a syntax tree. The downside though, is that this may create a sprawl of files,
which can make it difficult to understand the configuration at a glance. For
those cases, `rcl patch` can be a solution.

A more general approach to patching files, is of course `sed`. `rcl patch` is
safer, because it operates on syntax trees rather than text streams. It can
locate the value to edit by path rather than regex, it can edit values that span
multiple lines, and in general it ensures that the output is syntactically valid,
which `sed` cannot guarantee.
