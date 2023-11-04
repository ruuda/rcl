# Set

The `Set` type has the following methods.

## contains

    Set.contains :: Set[T] -> T -> Bool

Return whether the set contains a given element. For example:

```rcl
// Evaluates to [true, false].
[for needle in ["a", "z"]: {"a", "b", "c"}.contains(needle)]
```

## len

    Set.len :: Set[T] -> Int

Return the number of elements in the set. For example:

```rcl
// Evaluates to 3.
{1, 1, 2, 2, 3, 3}.len()
```
