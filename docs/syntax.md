# Syntax

RCL is a superset of json. Any json document is a valid <abbr>RCL</abbr>
expression which evaluates to itself as json. RCL furthermore features the
following constructs.

## Comments

Comments start with `//` and run until the end of the line. Comments in
<abbr>RCL</abbr> are slightly unusual in that there are some locations where
comments are not allowed.[^1] Generally, prefer to put comments on their own
line, before the item they comment on.

```rcl
// Comment like this.
let answer = 42;
let question = "unknown"; // The formatter would move this to the next line.
{ question: answer }
```

At the start of the document, a line that starts with `#!` is allowed, in order
to support executable files. For example:

```rcl
#!/usr/bin/env -S rcl eval
"This document prints this string when executed."
```

[^1]: The reason for disallowing comments in arbitrary locations, is that
<abbr>RCL</abbr> has a single syntax tree that is used both by the formatter and
the evaluator. The upside of this, is that the formatter is much less likely to
have subtle bugs where it will drop comments that are in weird locations that
are not represented in the <abbr title="Concrete Syntax Tree">CST</abbr> (such
as before the `in` in a `for ... in` construct). The downside is that the parser
will sometimes ask you to move comments.

## Booleans and null

The booleans are written `true` and `false`, null is written `null`.

## Numbers

RCL supports the same number formats as json, and adds hexadecimal integers
prefixed by `0x`, binary integers prefixed by `0b`, and support for numeric
underscores. See [the chapter on numbers](numbers.md) for more details.

```rcl
let numbers = [42, 4.2e1, 0x2a, 0b10_1010, 42_000, 0.000_420];
```

## Strings and f-strings

Strings are quoted with `"` and support the same escape sequences as json.

```rcl
"This is a string."
```

Multi-line strings can be quoted with `"""`. In both cases, adding an `f` in
front turns the string into a format string, which can have one or more _holes_
delimited by `{}`, to interpolate content into it:

```rcl
f"""
The answer to the ultimate question is {2 * 3 * 7}.
"""
```

See [the chapter on strings](strings.md) for the full details.

## Identifiers

Names of variables, and dict fields that use record syntax, are _identifiers_.
Identifiers must start with an underscore or <abbr>ASCII</abbr> letter, and can
furthermore contain <abbr>ASCII</abbr> digits, and `-`, a hyphen.

Allowing the hyphen in identifiers makes some types of configuration dicts
cleaner to write, but the downside is that it can cause confusion with the `-`
operator. The expression `a-b` is an identifier, not the binary operator `-`
applied to `a` and `b`. To subtract `b` from `a`, add spaces around the
operator: `a - b`.

## Lists

Lists are enclosed by `[]`. The list separator is `,` and a trailing comma is
allowed but not required.

```rcl
[
  ["Apple", "Banana", "Pear"],
  ["Eggplant", "Pepper", "Zuccini"],
]
```

## Dictionaries

Dictionaries, _dicts_ for short, are enclosed by `{}`.  Dicts can be written
in json form, where the left-hand side is an expression. Then the key and value
are separated by `:`. The element separator is `,`. A trailing comma is
optional.

```rcl
{
  "name": "apple",
  "flavor": "sweet",
}
```

The left-hand side does not have to be a string, although using other types than
strings precludes serialization to json.

```rcl
{
  1: "I",
  5: "V",
  5 + 5: "X",
}
```

Alternatively, dicts can be written in record form, where the left-hand side
is an [identifier](#identifiers). Then the key and value are separated by `=`.
A trailing comma is optional. The following value is identical to the first one
above.

```rcl
{
  name = "apple",
  flavor = "sweet",
}
```

Note, without type annotations, the empty collection `{}` is a dict, not a set.

## Sets

Sets are enclosed by `{}` and work otherwise the same as lists. The following
list contains two identical sets:

```rcl
[
  {"Apple", "Pear"},
  {"Apple", "Pear", "Apple"},
]
```

Note, without type annotations, the empty collection `{}` is a dict, not a set.
To produce an empty set, we can use [`std.empty_set`](stdlib.md#empty_set). A
type annotation will also force `{}` to be a set:

```rcl
let empty_set: Set[Number] = {};
```

## Let bindings

Values can be bound to names with a let-binding.

```rcl
let name = "apple";
let flavor = "sweet";
[name, flavor]
```

A let-binding is an _expression_, not an assignment statement. The expression
evaluates to the expression after `;`.

Let bindings can optionally contain [a type annotation](types.md):

```rcl
let answer: Number = 42;
```

## List indexing

Brackets are used to index into lists. Indices must be integers and are 0-based.
Negative indices index from the back of the list.

```rcl
let xs = ["Deckard", "Rachael", "Tyrell"];
// Evaluates to "Deckard".
xs[0]
// Evaluates to "Tyrell".
xs[-1]
```

## Dictionary indexing

Brackets are also used to look up a key in a dictionary.

```rcl
let replicants = {
  "NEXUS-7 N7FAA52318": "Rachael",
  "NEXUS-6 N6MAA10816": "Roy Batty",
  "NEXUS-6 N6MAC41717": "Leon Kowalski",
};
// Evaluates to "Rachael".
replicants["NEXUS-7 N7FAA52318"]
```

Looking up a key that does not occur in the dictionary causes evaluation to
abort with an error. To handle optional keys gracefully, use
[the `Dict.get` method](type_dict.md#get).

## Field access

The `.` can be used to access methods on values.

```rcl
// Evaluates to 3.
"abc".len()

// Evaluates to false.
{1, 2, 3}.contains(4)
```

The `.` can also be used to access fields of dictionaries. In most cases this
can be used as a more readable alternative to indexing notation.

```rcl
let replicant = { name = "Zhora Salome", model = "NEXUS-6 N6FAB61216" };
// Evaluates to "Zhora Salome".
replicant.name
```

When a dictionary contains a key with the same name as a built-in method, the
method takes precedence.

```rcl
let confusing = { len = 100 };

// Evaluates to builtin method Dict.len, not to the integer 100.
confusing.len
```

To access the value, use indexing notation instead. The same applies to keys
that are not valid identifiers, such as keys with spaces or
non-<abbr>ASCII</abbr> letters.

```rcl
let confusing = { len = 100 };
// Evaluates to 100.
confusing["len"]

let populations = {
  "Amsterdam": 1_459_402,
  "Düsseldorf": 1_220_000,
  "New York": 19_426_449,
};
// Evaluates to [1459402, 1220000, 19426449].
[populations.Amsterdam, populations["Düsseldorf"], populations["New York"]]
```

## Conditionals

An if-else expression evaluates to the _then_ or _else_ part depending on the
condition:

```rcl
let log_level = if flags.contains("--verbose"): 5 else: 1;

let rustc_codegen_opts =
  if config.is_debug:
    { opt-level = 0, debuginfo = 2 }
  else:
    { opt-level = 2, target-cpu = "native" };
```

Because an if-else expression is an _expression_, the _else_ part is mandatory.

## Operators

The following operators are supported. Most of them are similar to Python.

Unary operators that operate to the left of an expression, e.g. `not x`:

| Operator | Description |
|----------|-------------|
| `not`    | Boolean negation |
| `-`      | Numeric negation |

Binary operators that operate between two expressions, e.g. `x and y`:

| Operator | Description |
|----------|-------------|
| `and`    | Boolean <abbr>AND</abbr> |
| `or`     | Boolean <abbr>OR</abbr> |
| `==`     | Equal to |
| `!=`     | Not equal to |
| `<`      | Less than |
| `<=`     | Less than or equal to |
| `>`      | Greater than |
| `>=`     | Greater than or equal to |
| `+`      | Numeric addition |
| `-`      | Numeric subtraction |
| `*`      | Numeric multiplication |
| `/`      | Numeric division |

Unlike most other languages (but [like Pony][pony-ops]), <abbr>RCL</abbr> does not have
different precedence levels. To avoid confusing combinations of operators, you
have to use parentheses:

```rcl
// Invalid: Unclear whether this means (X and Y) or Z, or X and (Y or Z).
let should_log_verbose =
  settings.contains("log") and settings.log_level >= 2
  or settings.contains("debug");

// Disambiguate with parens:
let should_log_verbose =
  (settings.contains("log") and (settings.log_level >= 2))
  or settings.contains("debug");
```

[pony-ops]: https://tutorial.ponylang.io/expressions/ops.html#precedence

## Unpack

Inside collection literals (list, dicts, and sets), `..` and `...` unpack other
collections. A double dot, `..`, unpacks elements from lists and sets:

```rcl
let xs = [1, 2, 3];
[0, ..xs, 4]
// Evaluates to:
[0, 1, 2, 3, 4]
```

A triple dot, `...`, unpacks key-value pairs from dicts:
```rcl
let opts = { model = "Nexus", generation = 7 };
{ ...opts, name = "Rachael" }
/// Evaluates to:
{
  model = "Nexus",
  generation = 6,
  name = "Rachael",
}
```

When a key occurs multiple times in a dict, the last value is kept. This applies
to unpack as well:

```rcl
let defaults = { kind = "fruit", tasty = true };

{ ...defaults, name = "grapefruit", tasty = false }
// The last 'tasty' wins, the above evaluates to:
{ kind = "fruit", name = "grapefruit", tasty = false }

{ name = "grapefruit", tasty = false, ...defaults }
// The defaults overwrite earlier keys, the above evaluates to:
{ kind = "fruit", name = "grapefruit", tasty = true }
```

## Comprehensions

Inside collection literals (lists, dicts, and sets), aside from single
elements, it is possible to use comprehensions. There are three supported
constructs: `for`, `if`, and `let`.

```rcl
let dict = {"name": "pear", "flavor": "sweet"};
[for key, value in dict: value]
// Evaluates to:
["pear", "sweet"]

[if log_level >= 2: "Verbose message"]
// When log_level < 2, evaluates to:
[]
// When log_level >= 2, evaluates to:
["Verbose message"]

{let x = 10; "value": x}
// Evaluates to:
{"value": 10}
```

These can be combined arbitrarily:

```rcl
let labels = {
  for server in servers:
  let all_server_labels = { ..server_labels[server], ..default_labels };
  for label in all_server_labels:
  if not excluded_labels.contains(label):
  label
};
```

An `if` inside a comprehension controls the loop, it is not part of an
[if-else expression](#conditionals). To use an if-else expression inside a
comprehension, enclose it in parentheses:

```rcl
let target_os = {
  for server in servers:
  // This 'if' excludes servers from before 2021 from the resulting dict.
  if server.year_acquired >= 2021:
  server.name:
  // This 'if' is part of an if-else expression.
  (if server.year_acquired >= 2023: "ubuntu:22.04" else: "ubuntu:20.04")
};
```

There can be multiple loops per collection, and they can be mixed with single
elements:

```rcl
let small_numbers = [1, 2, 3];
let large_numbers = [100, 200, 300];
[
  for n in small_numbers: n,
  10,
  for n in large_numbers: n,
]
// Evaluates to:
[1, 2, 3, 10, 100, 200, 300]
```

Unpack can be combined with comprehensions:

```rcl
let nested = [[1, 2], [3, 4]];
[for xs in nested: ..xs]
// Evaluates to:
[1, 2, 3, 4]
```

## Assertions

You can use assertions in expressions and inside comprehensions:

```rcl
// Expression form:
assert condition: "Message for when the assertion fails.";
body

// Comprehension form:
[
  for widget in widgets:
  assert widget.is_valid(): f"Widget {widget.id} is invalid.";
  widget
]
```

The message is mandatory (unlike in Python). When the assertion fails,
evaluation aborts with the given message. The message does not have to be a
string, it can be an arbitrary value. When the assertion succeeds, the message
does not get evaluated at all.

## Debug tracing

In larger programs it can sometimes be useful to print what is going on during
evaluation. However, <abbr>RCL</abbr> is a purely functional language without
side effects; the only output it can produce is the final value. To still aid
debugging, `trace` acts as an escape hatch: it has the side effect of printing
a value to stderr during evaluation.

Like assertions, you can use `trace` in expressions and inside comprehensions:

```rcl
// Expression form:
trace "Value that gets printed just before we evaluate `body`.";
body

// Comprehension form:
let widget_ids = [for widget in widgets: trace widget; widget.id];
```

The message does not have to be a string, it can be an arbitrary value.

## Imports

An `import` expression evaluates to the contents of another <abbr>RCL</abbr>
document.

```rcl
let inventory = import "inventory.rcl";
[for server in inventory: server.name]
```

Import paths are relative to the location of the document itself, but there
are some restrictions on whether imports are allowed. See [the imports
chapter](imports.md) for full details.

## Functions

A `=>` arrow introduces a function.

```rcl
let double_input = x => x * 2;
let add = (x, y) => x + y;
// Evaluates to 42.
add(double_input(11), 20)
```

See [the chapter on functions](functions.md) for more details.
