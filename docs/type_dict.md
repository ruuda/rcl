# Dict

The `Dict` type has the following methods.

## contains

    Dict.contains :: Dict[K, V] -> K -> Bool

Return whether the dict contains the given key. For example:

```rcl
// Evaluates to [true, false].
[for needle in ["a", "z"]: { a = 1, b = 2, c = 3 }.contains(needle)]
```

## get

    Dict.get :: Dict[K, V] -> K -> V -> V

If the dict contains the given key, return the associated value. If not, return
the default value. For example:

```rcl
let d = { a = 1, b = 2 };
// Evaluates to [1, 26].
[for needle = ["a", "z"]: d.get(needle, 26)]
```

## len

    Dict.len :: Dict[K, V] -> Int

Return the size of the dict. For example:

```rcl
// Evaluates to 3.
{ four = 4, five = 5, six = 6 }.len()
```
