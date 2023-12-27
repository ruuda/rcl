# String

The `String` type has the following methods.

## chars

    String.chars: (self: String) -> List[String]

Return a list of the individual Unicode code points that make up the string. The
result is a list of single-character strings (where a character is a code point).
Like Python, <abbr>RCL</abbr> does not have a separate character type.

```rcl
// The string "Z\u{00fc}rich"
"Zürich".chars()
// Evaluates to:
["Z", "ü", "r", "i", "c", "h"]

// The string "Zu\u{0308}rich"
"Zürich".chars()
// Evaluates to:
["Z", "u", "̈", "r", "i", "c", "h"]
```

## contains

    String.contains: (self: String, needle: String) -> Bool

Return whether the string contains `needle` as a substring.

```rcl
// Evaluates to true.
"racecar".contains("ace")

// Evaluates to false.
"racecar".contains("cart")
```

## ends_with

    String.ends_with: (self: String, suffix: String) -> Bool

Return whether the string ends in `suffix`.

```rcl
// Evaluates to true.
"racecar".ends_with("car")

// Evaluates to false.
"racecar".ends_with("ace")
```

## len

    String.len: (self: String) -> Int

Return the number of bytes in the <abbr>UTF-8</abbr> encoding of the string.
For example:

```rcl
// Evaluates to [7, 2, 10].
[for s in ["example", "ü", "🕴︎︎"]: s.len()]
```

## parse_int

    String.parse_int: (self: String) -> Int

Parse the string as a signed integer in base 10. If the input is not an integer,
evaluation aborts with an error.

```rcl
// Evaluates to -42.
"-42".parse_int()
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

## starts_with

    String.starts_with: (self: String, prefix: String) -> Bool

Return whether the string starts with `prefix`.

```rcl
// Evaluates to true.
"racecar".starts_with("race")

// Evaluates to false.
"racecar".starts_with("ace")
```
