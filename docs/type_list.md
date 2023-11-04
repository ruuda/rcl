# List

The `List` type has the following methods.

## contains

    List.contains :: List[T] -> T -> Bool

Return whether the list contains a given element. For example:

```rcl
// Evaluates to [true, false].
[for needle in ["a", "z"]: ["a", "b", "c"].contains(needle)]
```

## len

    List.len :: List[T] -> Int

Return the number of elements in the list. For example:

```rcl
// Evaluates to 3.
[1, 2, 3].len()
```
