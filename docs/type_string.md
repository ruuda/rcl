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

## join

To concatenate list elements with a separator in between,
use [`List.join`](type_list.md#join).
(This entry is here to point people who are used to Python’s
[`str.join`](https://docs.python.org/3/library/stdtypes.html#str.join)
in the right direction.)

## len

    String.len: (self: String) -> Int

Return the number of Unicode code points that make up the string. This can be
less than the byte length of the <abbr>UTF-8</abbr> encoding of the string.

```rcl
[
  "example".len(),
  // The string "Z\u{00fc}rich"
  "Zürich".len(),
  // The string "Zu\u{0308}rich"
  "Zürich".len(),
  // The string "\u{1f574}\u{fe0e}"
  "🕴︎".len(),
]
// Evaluates to:
[7, 6, 7, 2]
```

## parse_int

    String.parse_int: (self: String) -> Int

Parse the string as a signed integer in base 10. If the input is not an integer,
evaluation aborts with an error.

```rcl
// Evaluates to -42.
"-42".parse_int()
```

## remove_prefix

    String.remove_prefix: (self: String, prefix: String) -> String

Return the string without the given prefix. When `self` does not start with
`prefix`, this aborts evaluation with an error.

```rcl
// Evaluates to "example.com".
"https://example.com".remove_prefix("https://")

// Results in an error.
"http://example.com".remove_prefix("https://")

// When an error is not desired, you can define this function:
let remove_prefix = (str, prefix) =>
  if str.starts_with(prefix):
    str.remove_prefix(prefix)
  else
    str;
```

## remove_suffix

    String.remove_suffix: (self: String, suffix: String) -> String

Return the string without the given suffix. When `self` does not end with
`suffix`, this aborts evaluation with an error.

```rcl
// Evaluates to "example".
"example.com".remove_suffix(".com")

// Results in an error.
"example.com".remove_suffix(".org")

// When an error is not desired, you can define this function:
let remove_suffix = (str, suffix) =>
  if str.ends_with(suffix):
    str.remove_suffix(suffix)
  else
    str;
```

## replace

    String.replace: (self: String, needle: String, replacement: String) -> String

Replace all occurrences of `needle` with `replacement`.

```rcl
"I saw the telephone through the telescope".replace("tele", "micro")
// Evaluates to:
"I saw the microphone through the microscope"
```

## split

    String.split: (self: String, separator: String) -> List[String]

Split the string on all occurrences of the separator. If the separator occurs
multiple times in a row, or at the start or end of the string, this produces
empty strings in the result. Trying to use an empty string as separator will
abort evaluation with an error.

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

## to_lowercase

    String.to_lowercase: (self: String) -> String

Convert the string to lowercase. This is implemented in terms of Rust’s
[`to_lowercase`](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase),
which defines lowercase in terms of the Unicode derived core property _Lowercase_.
Beware that Unicode can behave in unexpected ways!

```rcl
// Evaluates to "o_creat".
"O_CREAT".to_lowercase()

// Evaluates to false, İ lowercases to i followed by U+0307 Combining Dot Above.
"İstanbul".to_lowercase() == "istanbul"
```

## to_uppercase

    String.to_uppercase: (self: String) -> String

Convert the string to uppercase. This is implemented in terms of Rust’s
[`to_uppercase`](https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase),
which defines uppercase according to the Unicode derived core property _Uppercase_.
Beware that Unicode can behave in unexpected ways!

```rcl
// Evaluates to "O_CREAT".
"o_creat".to_uppercase()

// Evaluates to false, ß uppercases to SS instead of ẞ.
"straße".to_uppercase() == "STRAẞE"
```
