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

## split

    String.split: (self: String, separator: String) -> List[String]

Split the string on all occurrences of the separator. If the separator occurs
multiple times in a row, or at the start or end of the string, this produces
empty strings in the result.

```rcl
"Leon, Roy, Rachael".split(", ")
// Evaluates to:
["Leon", "Roy", "Rachael"]

"|Kowalski||Batty||Tyrell|".split("|")
// Evaluates to:
["", "Kowalski", "", "Batty", "", "Tyrell", ""]
```

## split_lines

    String.split_lines: (self: String) -> List[String]

Split the string on line endings `\n` and `\r\n`. The line endings themselves
are not included in the result. The final line ending is optional.

```rcl
"Leon\nRoy\nRachael\n".split_lines()
// Evaluates to:
["Leon", "Roy", "Rachael"]

"Kowalski\r\nBatty\rTyrell".split_lines()
// Evaluates to:
["Kowalski", "Batty\rTyrell"]
```

## parse_int

    String.parse_int: (self: String) -> Int

Parse the string as a signed integer in base 10. If the input is not an integer,
evaluation aborts with an error.

```rcl
// Evaluates to -42.
"-42".parse_int()
```
