# Dict

The `Dict` type supports the following methods and operators.

## Indexing

Brackets can be used to get the value for a particular key:

```rcl
// Evaluates to 2.
let d = { x = 2, y = 3 }; d["x"]
```

When the key is not present, evaluation aborts with an error. Use [`get`](#get)
to handle optional keys gracefully.

## Union operator

The `|` operator returns the union of two dictionaries. When a key occurs on
both sides, the value is taken from the right side.

```rcl
{ x = 2, y = 3 } | { y = 5, z = 7 }
// Evaluates to:
{ x = 2, y = 5, z = 7 }
```

## contains

    Dict.contains: (self: Dict[K, V], key: K) -> Bool

Return whether the dict contains the given key. For example:

```rcl
// Evaluates to [true, false].
[for needle in ["a", "z"]: { a = 1, b = 2, c = 3 }.contains(needle)]
```

## except

    Dict.except: (self: Dict[K, V], key: K) -> Dict[K, V]

Return a copy of the dictionary, with the given key omitted. If the key was not
present in the first place, then the result is identical to the input.

```rcl
let replicant = {
  name = "Leon Kowalski",
  serial = "NEXUS-6 N6MAC41717",
};
replicant.except("serial")
// Evaluates to:
{ name = "Leon Kowalski" }
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
{ "full_name", "username" }
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

Discard the keys, and return only the values stored in the dict. The values are
returned as a list because the same value may occur multiple times. The order of
the values is arbitrary. TODO: Dicts should preserve insertion order.

```rcl
let machine_distros = {
  database01 = "ubuntu:20.04",
  database02 = "ubuntu:20.04",
  worker01 = "ubuntu:22.04",
  worker02 = "ubuntu:22.04",
  desktop = "ubuntu:23.10",
};
machine_distros.values()
// Evaluates to:
["ubuntu:20.04", "ubuntu:20.04", "ubuntu:23.10", "ubuntu:22.04", "ubuntu:22.04"]
```

If you want to get all the _unique_ values, you can convert the result into a
set with a comprehension:

```rcl
{for distro in machine_distros.values(): distro}
// Evaluates to:
{"ubuntu:20.04", "ubuntu:22.04", "ubuntu:23.10"}
```
