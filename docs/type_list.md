# List

The `List` type has the following methods.

## contains

    List.contains: (self: List[T], element: T) -> Bool

Return whether the list contains a given element. For example:

```rcl
// Evaluates to [true, false].
[for needle in ["a", "z"]: ["a", "b", "c"].contains(needle)]
```

## group_by

    List.group_by: (self: List[T], get_key: T -> U) -> Dict[U, List[T]]

Group the elements of the list by a key selected by `get_key`.

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

## len

    List.len: (self: List[T]) -> Int

Return the number of elements in the list. For example:

```rcl
// Evaluates to 3.
[1, 2, 3].len()
```
