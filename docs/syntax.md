# Syntax

RCL is a superset of json. This means that RCL supports literals for the
following data types:

## Booleans

The booleans are written `true` and `false`.

## Strings

Strings are quoted with `"` and support the same escape sequences as json.

    "This is a string."

Strings can also be quoted with `"""`. In that case, any whitespace that is
common across all the lines of the string literal is not part of the contents
of the string, and if the `"""` is followed directly by a newline, then that
newline is not part of the string either. The following list contains two
identical strings:

    [
      "This is\na string.\n",
      """
      This is
      a string.
      """,
    ]

## Lists

Lists are surrounded by `[]`. The list separator is `,` and a trailing comma is
allowed but not required.

    [
      ["Apple", "Banana", "Pear"],
      ["Eggplant", "Pepper", "Zuccini"],
    ]

## Records

Records are surrounded by `{}`. Records can be written in json form, where the
left-hand side is an expression. Then the key and value are separated by `:` and
the element separator is `,`. A trailing comma is optional.

    {
      "name": "apple",
      "flavor": "sweet",
    }

The left-hand side does not have to be a string, although using other types than
strings precludes serialization to json.

    {
      1: "I",
      5: "V",
      5 + 5: "X",
    }

Alternatively, records can be written in record form, where the left-hand side
is an identifier. Then the key and value are separated by `=` and the element
separator is `;`. A trailing semicolon is optional. The following value is
identical to the first one above.

    {
      name = "apple";
      flavor = "sweet";
    }

Note, the empty collection `{}` is a set, not a record.
TODO: Should infer the type.

## Sets

Sets are surrounded by `{}` and work otherwise the same as lists. The empty
collection `{}` is a set, not a record. The following list contains two
identical sets:

    [
      {"Apple", "Pear"},
      {"Apple", "Pear", "Apple"},
    ]

## Let bindings

Values can be bound to names with a let-binding.

    let name = "apple";
    let flavor = "sweet";
    [name, flavor]

A let-binding is an _expression_, not an assignment statement. The expression
evaluates to the expression after `;`.

## Operators

Operators are written with keywords.

Unary operators:

 * `not` — Boolean negation.

Binary operators:

 * `and` — Boolean <abbr>AND</abbr>.
 * `or` — Boolean <abbr>OR</abbr>.
 * `|` — Set or record union.

Unlike most other languages (but like [Pony][pony-ops]), RCL does not have
different precedence levels. To avoid confusion, you have to use parentheses:

    // Invalid: Unclear whether this means (X and Y) or Z, or X and (Y or Z).
    let should_log_verbose =
      settings.contains("log") and settings.log_level >= 2
      or settings.contains("debug");

    // Disambiguate with parens:
    let should_log_verbose =
      (settings.contains("log") and (settings.log_level >= 2))
      or settings.contains("debug");

[pony-ops]: https://tutorial.ponylang.io/expressions/ops.html#precedence

## Loops

Inside collection literals (lists, records, and sets), aside from single
elements, it is possible to use loops. There are three possible loop structures:
`for`, `if`, and `let`.

    let record = {"name": "pear"};
    [for key, value in record: value]
    // Evaluates to:
    ["pear"]

    [if log_level >= 2: "Verbose message"]
    // When log_level < 2, evaluates to:
    []
    // When log_level >= 2, evaluates to:
    ["Verbose message"]

    {let x = 10; "value": x}
    // Evaluates to:
    {"value": 10}

These can be combined arbitrarily:

    let labels = {
      for server in servers:
      let all_server_labels = server_labels[server] | default_labels;
      for label in all_server_labels:
      if not excluded_labels.contains(label):
      label
    };

There can be multiple loops per collection, and they can be mixed with single
elements:

    let small_numbers = [1, 2, 3];
    let large_numbers = [100, 200, 300];
    [
      for n in small_numbers: n,
      10,
      for n in large_numbers: n,
    ]
    // Evaluates to:
    [1, 2, 3, 10, 100, 200, 300]
