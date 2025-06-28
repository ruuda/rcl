# Standard library

The <abbr>RCL</abbr> standard library is a dict of values that is in scope by
default under the name `std`. Most of the built-in functionality is not in this
`std` dict, but in methods on the builtin types. See the next chapters for those.

## empty_set

```rcl
std.empty_set: Set[Void]
```

An empty set. This constant exists, because without type annotations, `{}` is an
empty dict rather than an empty set. This constant is the standard way to refer
to an empty set.

## format_json

    std.format_json: (value: Any) -> String

Format the value as json, in the same way that [`rcl evaluate --format=json`](rcl_evaluate.md)
would, except without a maximum width, so the result contains no newlines.

```rcl
std.format_json({a = 1})
// Evaluates to:
"{\"a\": 1}"
```

## range

    std.range: (lower: Number, upper: Number) -> List[Number]

Return the range of integers `lower` through `upper`. The lower bound is
inclusive and the upper bound is exclusive. When the lower bound is greater
than the upper bound, `range` returns an empty list.

```rcl
std.range(3, 7)
// Evaluates to:
[3, 4, 5, 6]

std.range(7, 3)
// Evaluates to:
[]
```

## read_file_utf8

    std.read_file_utf8: (path: String) -> String

Return the contents of the file at the given path. Paths are treated the same
as [for imports](imports.md#import-location), and are subject to the same
[sandbox restrictions](rcl_evaluate.md#-sandbox-mode). The file must contain
valid <abbr>UTF-8</abbr> text without byte order mark.
