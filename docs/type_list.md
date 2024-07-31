# List

The `List` type has the following methods.

## contains

```rcl
List.contains: (self: List[T], element: T) -> Bool
```

Return whether the list contains a given element. For example:

```rcl
[for needle in ["a", "z"]: ["a", "b", "c"].contains(needle)]
// Evaluates to:
[true, false]
```

## enumerate

```rcl
List.enumerate: (self: List[T]) -> Dict[Int, T]
```

Return a mapping from index to list element. The index is zero-based.

```rcl
["x", "y", "z"].enumerate()
// Evaluates to:
{ 0: "x", 1: "y", 2: "z" }
```

This is mostly useful for iterating:

```rcl
let pieces = ["pawn", "queen", "bisshop"];
let unordered_pairs = [
  for i, piece_i in pieces.enumerate():
  for j in std.range(i + 1, pieces.len()):
  let piece_j = pieces[j];
  [piece_i, piece_j]
];
unordered_pairs
// Evaluates to:
[["pawn", "queen"], ["pawn", "bisshop"], ["queen", "bisshop"]]
```

## filter

```rcl
List.filter: (self: List[T], predicate: T -> Bool) -> List[T]
```

Construct a new list that contains only the elements where `predicate` returned
true. The result is equivalent to a [list comprehension](syntax.md#comprehensions),
`a` and `b` are identical in this example:

```rcl
let xs = [1, 2, 3];
let a = xs.filter(x => x > 1);
let b = [
  for x in xs:
  if x > 1:
  x
];
// Both a and b evaluate to:
[2, 3]
```

List comprehensions are more general than `filter`: they support nested loops,
and let-bindings are accessible to the inner scope. Still, `filter` can be
useful, especially for iteratively refining a query in an [`rcl query`][query]
command.

[query]: rcl_query.md

## flat_map

```rcl
List.flat_map: (self: List[T], map_element: T -> List[U]) -> List[U]
```

Construct a new list by taking every element in the list, applying `map_element`
to it (which should return a collection), and concatenating those results.
`flat_map` is like [`map`](#map), except that it flattens the result. It is
equivalent to a [list comprehension](syntax.md#comprehensions) with a nested
loop: `a` and `b` are identical in this example:

```rcl
let apps = [
  { name = "sshd", ports = [22] },
  { name = "nginx", ports = [80, 443] },
];
let a = apps.flat_map(app => app.ports);
let b = [
  for app in apps:
  for port in app.ports:
  port
];
// Both a and b evaluate to:
[22, 80, 443]
```

List comprehensions are often clearer in configuration files, especially when
the body is large. They are also more general: list comprehensions support
arbitrary nesting, filtering with `if`, and let-bindings are accessible to the
inner scope. Still, `flat_map` can be useful, especially for iteratively
refining a query in an [`rcl query`][query] command.

## fold

```rcl
List.fold: (self: List[T], seed: U, reduce: (U, T) -> U) -> U
```

Left-fold the function `reduce` over the list, with `seed` as the initial
accumulator value.

```rcl
[2, 3, 5, 7, 11].fold(
  { min = 99, max = 0 },
  (acc, x) => {
    min = if acc.min < x: acc.min else: x,
    max = if acc.max > x: acc.max else: x,
  },
)
// Evaluates to:
{ max = 11, min = 2 }
```

## group_by

```rcl
List.group_by: (self: List[T], get_key: T -> U) -> Dict[U, List[T]]
```

Group the elements of the list by a key selected by `get_key`. Within groups,
the original order of elements is preserved.

```rcl
let foods = [
  { category = "fruit", name = "apple" },
  { category = "fruit", name = "pear" },
  { category = "vegetable", name = "onion" },
  { category = "vegetable", name = "carrot" },
];
foods.group_by(food => food.category)

// Evaluates to:
{
  fruit = [
    { category = "fruit", name = "apple" },
    { category = "fruit", name = "pear" },
  ],
  vegetable = [
    { category = "vegetable", name = "onion" },
    { category = "vegetable", name = "carrot" },
  ],
}
```

## join

```rcl
List.join: (self: List[T], separator: String) -> String
```

Concatenate the elements with the separator in between. This is equivalent to
using a format string, therefore the list elements must be string formattable.

```rcl
// Evaluates to "foo-bar".
["foo", "bar"].join("-")

// Evaluates to "2,3,5".
[2, 3, 5].join(",")

// Error, dicts are not string formattable.
[{}, {}].join("")
```

## key_by

```rcl
List.key_by: (self: List[T], get_key: T -> U) -> Dict[U, T]
```

Build a dictionary with the key selected by `get_key` as key, and the list
elements as values. The keys must be unique. When a key is not unique, this
method fails and reports the conflicting values.

```rcl
let replicants = [
  { name = "rachael", generation = 7 },
  { name = "rbatty", generation = 6 },
  { name = "zsalome", generation = 6 },
];

replicants.key_by(r => r.name)
// Evaluates to:
{
  rachael = { name = "rachael", generation = 7 },
  rbatty = { name = "rbatty", generation = 6 },
  zsalome = { name = "zsalome", generation = 6 },
}

replicants.key_by(r => r.generation)
// This fails with the following error:
// Error: The key 6 is not unique. The following values use this key:
//   { generation = 6, name = "rbatty" }
//   { generation = 6, name = "zsalome" }
```

## len

```rcl
List.len: (self: List[T]) -> Int
```

Return the number of elements in the list. For example:

```rcl
// Evaluates to 3.
[1, 2, 3].len()
```

## map

```rcl
List.map: (self: List[T], map_element: T -> U) -> List[U]
```

Construct a new list by applying `map_element` to every element in the list.
The result is equivalent to a [list comprehension](syntax.md#comprehensions),
`a` and `b` are identical in this example:

```rcl
let xs = [1, 2, 3];
let a = [for x in xs: x * 2];
let b = xs.map(x => x * 2);
// Both a and b evaluate to:
[2, 4, 6]
```

List comprehensions are often clearer in configuration files, especially when
the body is large. They are also more general: list comprehensions support
nested loops and filtering with `if`. Still, `map` can be useful, especially
for iteratively refining a query in an [`rcl query`][query] command.

## reverse

```rcl
List.reverse: (self: List[T]) -> List[T]
```

Return the list in reverse.

```rcl
[1, 2, 3].reverse()
// Evaluates to:
[3, 2, 1]
```

## sum

```rcl
List.sum: (self: List[Int]) -> Int
```

Return the sum of the elements in the list. For example:

```rcl
// Evaluates to 42.
[3, 7, 11, 21].sum()
```
