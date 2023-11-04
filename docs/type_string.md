# String

The `String` type has the following methods.

## len

    String.len: (self: String) -> Int

Return the number of bytes in the <abbr>UTF-8</abbr> encoding of the string.
For example:

```rcl
// Evaluates to [7, 2, 10].
[for s in ["example", "ü", "🕴︎︎"]: s.len()]
```
