# Set

The `Set` type has the following methods.

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

## group_by

```rcl
Set.group_by: (self: Set[T], get_key: T -> U) -> Dict[U, Set[T]]
```

Group the elements of the list by a key selected by `get_key`.

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

## len

```rcl
Set.len: (self: Set[T]) -> Int
```

Return the number of elements in the set. For example:

```rcl
// Evaluates to 3.
{1, 1, 2, 2, 3, 3}.len()
```
