# Standard library

The <abbr>RCL</abbr> standard library is a dict of functions that is in scope by
default under the name `std`. Most of the built-in functionality is not in this
`std` dict, but in methods on the builtin types. See the next chapters for those.

## read_file_utf8

    std.read_file_utf8: (path: String) -> String

Return the contents of the file at the given path. Paths are treated the same
as [for imports](imports.md#import-location), and are subject to the same
[sandbox restrictions](rcl_evaluate.md#-sandbox-mode). The file must contain
valid <abbr>UTF-8</abbr> text without byte order mark.
