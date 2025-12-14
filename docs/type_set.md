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

To flat-map a function over a set, you can either use a
[comprehension](syntax.md#comprehensions), or convert the set to a list with
[`to_list`](#to_list) and then use [`List.flat_map`](type_list.md#flat_map):

```rcl
let apps = {
  { name = "sshd", ports = {22} },
  { name = "nginx", ports = {80, 443} },
  { name = "caddy", ports = {80, 443} },
};

// Convert to list and then map, evaluates to {22, 80, 443}.
apps.to_list().flat_map(app => app.ports).to_set_dedup()

// List comprehension, evaluates to [80, 443, 80, 443, 22].
[for app in apps: ..app.ports]

// Set comprehension, evaluates to {22, 80, 443}.
{for app in apps: ..app.ports}
```

In the past, sets had a `flat_map` method. It returned a set, and the implicit
deduplication of the results could be unexpected. Therefore, `Set.flat_map` was
removed in <abbr>RCL</abbr> 0.12.0 in favor of more explicit alternatives.

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

To map a function over a set, you can use a
[comprehension](syntax.md#comprehensions), or convert the set to a list with
[`to_list`](#to_list) and then use [`List.map`](type_list.md#map):

```rcl
let fruits = {"apple", "orange", "banana"};

// Convert to list and then map, evaluates to [5, 6, 6].
fruits.to_list().map(fruit => fruit.len())

// List comprehension, evaluates to [5, 6, 6].
[for fruit in fruits: fruit.len()]

// Set comprehension, evaluates to {5, 6}.
{for fruit in fruits: fruit.len()}
```

In the past, sets had a `map` method, but it was a common source of mistakes.
It returned a set, and the implicit deduplication was often unexpected.
Therefore, `Set.map` was removed in <abbr>RCL</abbr> 0.12.0 in favor of more
explicit alternatives.

## len

```rcl
Set.len: (self: Set[T]) -> Number
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
Set.sum: (self: Set[Number]) -> Number
```

Return the sum of the elements in the set. For example:

```rcl
// Evaluates to 42.
{3, 7, 11, 21}.sum()
```

## to_list

```rcl
Set.to_list: (self: Set[T]) -> List[T]
```

Convert the set to a list. This is equivalent to a [list
comprehension](syntax.md#comprehensions):

```rcl
let set = {1, 2, 3};
let a = [..set];
let b = set.to_list();
assert a == b: "Comprehension and to_list are equivalent";
```

Because sets are currently implemented as trees, the returned list is sorted.
This may change in future versions. Such a change would be announced prominently
in the changelog.

## transitive_closure

```rcl
Set.transitive_closure: (self: Set[T], expand: (T) -> Set[T]) -> Set[T]
```

Return the _transitive closure_ of the relation `expand`. In graph terms,
starting with a set of nodes `self`, the transitive closure is the set of all
nodes that are reachable from the initial set of nodes, whether directly through
an edge, or indirectly through multiple hops. The function `expand` takes one
node, and should return the nodes one hop removed; it should follow the
outgoing edges. It can return those nodes either as a `Set[T]` or `List[T]`.

The transitive closure is often useful to flatten trees. For example:

```rcl
let packages = {
  is-number = { version = "7.0.0" },
  is-odd = { version = "3.0.1", deps = ["is-number"] },
  is-even = { version = "1.0.0", deps = ["is-odd", "mocha"] },
  mocha = { version = "11.7.5", deps = ["diff", "picocolors"] },
  diff = { version = "8.0.2" },
  picocolors = { version = "1.1.1" },
};

// Evaluates to {"is-number", "is-odd"}.
{"is-odd"}.transitive_closure(p => packages[p].get("deps", []))

// Evaluates to {"diff", "is-even", "is-number", "is-odd", "mocha",
// "picocolors"}.
{"is-even"}.transitive_closure(p => packages[p].get("deps", []))
```

It is fine when multiple calls to `expand` return the same value, and even to
return values that were already expanded, for example in a graph where nodes
have multiple incoming edges or cycles:

```rcl
let out_edges = { a = {"b", "c"}, b = {"c", "a"}, c = {"a", "b"} };

// Does not hang, despite the cycles in the graph.
// Evaluates to {"a", "b", "c"}.
{"a"}.transitive_closure(p => out_edges[p])
```

## union

To take the union of sets, use [unpack](syntax.md#unpack):

```rcl
let xs = { 1, 2, 3 };
let ys = { 3, 4, 5 };
// Evaluates to { 1, 2, 3, 4, 5 }.
{ ..xs, ..ys }
```
