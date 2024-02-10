# List

The `List` type has the following methods.

## contains

    List.contains: (self: List[T], element: T) -> Bool

Return whether the list contains a given element. For example:

```rcl
[for needle in ["a", "z"]: ["a", "b", "c"].contains(needle)]
// Evaluates to:
[true, false]
```

## enumerate

    List.enumerate: (self: List[T]) -> Dict[Int, T]

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

## fold

    List.fold: (self: List[T], seed: U, reduce: (U, T) -> U) -> U

Left-fold the function `reduce` over the list, with `seed` as the initial
accumulator value.

```rcl
[2, 3, 5, 7, 11].fold(
  { min = 99, max = 0 },
  (acc, x) => {
    min = if acc.min < x: acc.min else x,
    max = if acc.max > x: acc.max else x,
  },
)
// Evaluates to:
{ max = 11, min = 2 }
```

## group_by

    List.group_by: (self: List[T], get_key: T -> U) -> Dict[U, List[T]]

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

    List.join: (self: List[T], separator: String) -> String

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

    List.key_by: (self: List[T], get_key: T -> U) -> Dict[U, T]

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

    List.len: (self: List[T]) -> Int

Return the number of elements in the list. For example:

```rcl
// Evaluates to 3.
[1, 2, 3].len()
```

## reverse

    List.reverse: (self: List[T]) -> List[T]

Return the list in reverse.

```rcl
[1, 2, 3].reverse()
// Evaluates to:
[3, 2, 1]
```
