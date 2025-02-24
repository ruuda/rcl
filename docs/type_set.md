# Set

The `Set` type has the following methods.

## all

```rcl
Set.all: (self: Set[T], predicate: T -> Bool) -> Bool
```

Return whether the predicate is true for all elements in the set. For example:

```rcl
// Evaluates to true.
{11, 17, 42}.all(x => x > 0)

// Evaluates to false.
{11, 17, 42}.all(x => x > 20)

// Evaluates to true.
std.empty_set.all(x => false)
```

This method short-circuits: when the outcome is decided, it will not call the
predicate for the remaining elements in the set.

## any

```rcl
Set.any: (self: Set[T], predicate: T -> Bool) -> Bool
```

Return whether the predicate is true for any element in the set. For example:

```rcl
// Evaluates to true.
{11, 17, 42}.any(x => x > 17)

// Evaluates to false.
{11, 17, 42}.any(x => x > 42)

// Evaluates to false.
std.empty_set.any(x => true)
```

This method short-circuits: when the outcome is decided, it will not call the
predicate for the remaining elements in the set.

## contains

```rcl
Set.contains: (self: Set[T], element: T) -> Bool
```

Return whether the set contains a given element. For example:

```rcl
// Evaluates to [true, false].
[for needle in ["a", "z"]: {"a", "b", "c"}.contains(needle)]
```

## except

```rcl
Set.except: (self: Set[T], element: T) -> Set[T]
```

Return a copy of the set, with the given element omitted. If the element was not
present in the first place, then the result is identical to the input.

```rcl
{1, 2, 3}.except(2)
// Evaluates to:
{1, 3}
```

## filter

```rcl
Set.filter: (self: Set[T], predicate: T -> Bool) -> Set[T]
```

Construct a new set that contains only the elements where `predicate` returned
true. The result is equivalent to a [set comprehension](syntax.md#comprehensions),
`a` and `b` are identical in this example:

```rcl
let xs = {1, 2, 3};
let a = xs.filter(x => x > 1);
let b = {
  for x in xs:
  if x > 1:
  x
};
// Both a and b evaluate to:
{2, 3}
```

Set comprehensions are more general than `filter`: they support nested loops,
and let-bindings are accessible to the inner scope. Still, `filter` can be
useful, especially for iteratively refining a query in an [`rcl query`][query]
command.

[query]: rcl_query.md

## flat_map

```rcl
Set.flat_map: (self: Set[T], map_element: T -> Set[U]) -> Set[U]
```

Construct a new set by taking every element in the set, applying `map_element`
to it (which should return a collection), and concatenating those results.
`flat_map` is like [`map`](#map), except that it flattens the result. It is
equivalent to a [set comprehension](syntax.md#comprehensions) with a nested
loop: `a` and `b` are identical in this example:

```rcl
let apps = {
  { name = "sshd", ports = {22} },
  { name = "nginx", ports = {80, 443} },
};
let a = apps.flat_map(app => app.ports);
let b = {
  for app in apps:
  for port in app.ports:
  port
};
// Both a and b evaluate to:
{22, 80, 443}
```

Set comprehensions are often clearer in configuration files, especially when
the body is large. They are also more general: set comprehensions support
arbitrary nesting, filtering with `if`, and let-bindings are accessible to the
inner scope. Still, `flat_map` can be useful, especially for iteratively
refining a query in an [`rcl query`][query] command.

## group_by

```rcl
Set.group_by: (self: Set[T], get_key: T -> U) -> Dict[U, Set[T]]
```

Group the elements of the set by a key selected by `get_key`.

```rcl
let foods = {
  { category = "fruit", name = "apple" },
  { category = "fruit", name = "pear" },
  { category = "vegetable", name = "onion" },
  { category = "vegetable", name = "carrot" },
};
foods.group_by(food => food.category)

// Evaluates to:
{
  fruit = {
    { category = "fruit", name = "apple" },
    { category = "fruit", name = "pear" },
  },
  vegetable = {
    { category = "vegetable", name = "carrot" },
    { category = "vegetable", name = "onion" },
  },
}
```

## key_by

```rcl
Set.key_by: (self: Set[T], get_key: T -> U) -> Dict[U, T]
```

Build a dictionary with the key selected by `get_key` as key, and the set
elements as values. The keys must be unique. When a key is not unique, this
method fails and reports the conflicting values. See also
[`List.key_by`](type_list.md#key_by) for an example.

## map

```rcl
Set.map: (self: Set[T], map_element: T -> U) -> Set[U]
```

Construct a new set by applying `map_element` to every element in the set.
The result is equivalent to a [set comprehension](syntax.md#comprehensions),
`a` and `b` are identical in this example:

```rcl
let xs = {1, 2, 3};
let a = {for x in xs: x * 2};
let b = xs.map(x => x * 2);
// Both a and b evaluate to:
{2, 4, 6}
```

Set comprehensions are often clearer in configuration files, especially when
the body is large. They are also more general: set comprehensions support
nested loops and filtering with `if`. Still, `map` can be useful, especially
for iteratively refining a query in an [`rcl query`][query] command.

## len

```rcl
Set.len: (self: Set[T]) -> Int
```

Return the number of elements in the set. For example:

```rcl
// Evaluates to 3.
{1, 1, 2, 2, 3, 3}.len()
```

## sort

```rcl
Set.sort: (self: Set[T]) -> List[T]
```

Return a sorted version of the set. Elements of the same type will be sorted
with respect to each other. The relative order of elements of different types
is an implementation detail that may change between versions.

```rcl
{11, 5, 7}.sort()
// Evaluates to:
[5, 7, 11]
```

## sort_by

```rcl
Set.sort_by: (self: Set[T], get_key: T -> U) -> List[T]
```

Return a copy of the set, sorted on a key selected by the function `get_key`.
Keys are compared in the same way as for [`sort`](#sort). The relative order
is unspecified when keys compare equal, and may change in future versions.
<!-- TODO: When we have sets with stable order, this should be a stable sort,
just like List.sort. -->

```rcl
let characters = {
  "Rachael",
  "Rick Deckard",
  "Gaff",
  "Pris",
  "Eldon Tyrell",
};
characters.sort_by(name => name.len())

// Evaluates to:
["Gaff", "Pris", "Rachael", "Eldon Tyrell", "Rick Deckard"]
```

## sum

```rcl
Set.sum: (self: Set[Int]) -> Int
```

Return the sum of the elements in the set. For example:

```rcl
// Evaluates to 42.
{3, 7, 11, 21}.sum()
```
