# Dict

The `Dict` type has the following methods.

## contains

    Dict.contains: (self: Dict[K, V], key: K) -> Bool

Return whether the dict contains the given key. For example:

```rcl
// Evaluates to [true, false].
[for needle in ["a", "z"]: { a = 1, b = 2, c = 3 }.contains(needle)]
```

## get

    Dict.get: (self: Dict[K, V], key: K, default: V) -> V

If the dict contains the given key, return the associated value. If not, return
the default value. For example:

```rcl
let d = { a = 1, b = 2 };
// Evaluates to [1, 26].
[for needle = ["a", "z"]: d.get(needle, 26)]
```

## keys

    Dict.keys: (self: Dict[K, V]) -> Set[K]

Return the keys of the dict as a set.

```rcl
{ username = "etyrell", full_name = "Eldon Tyrell" }.keys()
// Evaluates to:
{ "username", "full_name" }
```

## len

    Dict.len: (self: Dict[K, V]) -> Int

Return the number of keys in the dict. For example:

```rcl
// Evaluates to 3.
{ four = 4, five = 5, six = 6 }.len()
```

## values

    Dict.values: (self: Dict[K, V]) -> List[V]

Return the values of the dict as a list. The values are returned in arbitrary
order. TODO: Dicts should preserve insertion order.

```rcl
{ username = "etyrell", full_name = "Eldon Tyrell" }.values()
// Evaluates to:
["Eldon Tyrell", "etyrell"]
```
