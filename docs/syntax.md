# Syntax

RCL is a superset of json. Any json document is a valid RCL expression which
evaluates to itself as json. RCL furthermore features the following constructs.

## Comments

Comments start with `//` and run until the end of the line. Comments in RCL are
slightly unusual in that there are some locations where comments are not
allowed.[^1] Generally, prefer to put comments on their own line, before the
item they comment on.

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

[^1]: The reason for disallowing comments in arbitrary locations, is that RCL
has a single syntax tree that is used both by the formatter and the evaluator.
The upside of this, is that the formatter is much less likely to have subtle
bugs where it will drop comments that are in weird locations that are not
represented in the <abbr title="Concrete Syntax Tree">CST</abbr> (such as before
the `in` in a `for ... in` construct). The downside is that the parser will
sometimes ask you to move comments.

## Booleans and null

The booleans are written `true` and `false`, null is written `null`.

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

## Lists

Lists are surrounded by `[]`. The list separator is `,` and a trailing comma is
allowed but not required.

```rcl
[
  ["Apple", "Banana", "Pear"],
  ["Eggplant", "Pepper", "Zuccini"],
]
```

## Dictionaries

Dictionaries, _dicts_ for short, are surrounded by `{}`.  Dicts can be written
in json form, where the left-hand side is an expression. Then the key and value
are separated by `:` and the element separator is `,`. A trailing comma is
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
is an identifier. Then the key and value are separated by `=`. A trailing comma
is optional. The following value is identical to the first one above.

```rcl
{
  name = "apple",
  flavor = "sweet",
}
```

Note, the empty collection `{}` is a dict, not a set.

## Sets

Sets are surrounded by `{}` and work otherwise the same as lists. The following
list contains two identical sets:

```rcl
[
  {"Apple", "Pear"},
  {"Apple", "Pear", "Apple"},
]
```

Note, the empty collection `{}` is a dict, not a set. There is currently no
literal for the empty set. It is possible to work around this using
comprehensions:

```rcl
// An empty set.
{for x in []: x}
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

## Operators

The following operators are supported. Most of them are similar to Python.

| Operator | Arity  | Description |
|----------|--------|-------------|
| `not`    | unary  | Boolean negation |
| `and`    | binary | Boolean <abbr>AND</abbr> |
| `or`     | binary | Boolean <abbr>OR</abbr> |
| `|`      | binary | Set or dict union, right-biased for dicts |

Unlike most other languages (but [like Pony][pony-ops]), RCL does not have
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

## Indexing

Brackets are used to index into collections. At the moment, only lists are
supported. Indices must be integers and are 0-based.

```rcl
let xs = ["Deckard", "Rachael", "Tyrell"];
xs[0]
// Evaluates to "Deckard".
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
  let all_server_labels = server_labels[server] | default_labels;
  for label in all_server_labels:
  if not excluded_labels.contains(label):
  label
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

## Assertions

You can use assertions in expressions and inside comprehensions:

```rcl
// Expression form:
assert condition, "Message for when the assertion fails.";
body

// Comprehension form:
[
  for widget in widgets:
  assert widget.is_valid(), f"Widget {widget.id} is invalid.";
  widget
]
```

The message is mandatory (unlike in Python). When the assertion fails,
evaluation aborts with the given message. The message does not have to be a
string, it can be an arbitrary value. When the assertion succeeds, the message
does not get evaluated at all.

## Debug tracing

In larger programs it can sometimes be useful to print what is going on during
evaluation. However, RCL is a purely functional language without side effects;
the only output it can produce is the final value. To still aid debugging,
`trace` acts as an escape hatch: it has the side effect of printing a value to
stderr during evaluation.

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

An `import` expression evaluates to the contents of another RCL document.

```rcl
let inventory = import "inventory.rcl";
[for server in inventory: server.name]
```

Import paths are relative to the location of the document itself, but there
are some restrictions on whether imports are allowed. See [the imports
chapter](imports.md) for full details.
