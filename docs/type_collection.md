# Collection

A collection is either a [`List`](type_list.md) or a [`Set`](type_set.md).
`Collection` the type of values
that a single-variable [`for` loop](syntax.md#comprehensions)
can iterate over.

`Collection[T]` is a friendly name
for the union type [`Union[List[T], Set[T]]`](types.md#union-types).
You might see it pop up in error messages
in places where both lists and sets are allowed.
In type annotations,
prefer to use the more specific `List` or `Set` over `Collection`.

The methods that are available on a value that fits `Collection[T]` depend on
whether that value is a list or a set, though many methods are available on both.
See the docs for [`List`](type_list.md) and [`Set`](type_set.md) for full details.

As a contrived example,
we can use `Collection` to capture either the keys or values of a dict:

```rcl
let mode = "keys";
let data = {
  name = "Pris Stratton",
  model = "NEXUS-6 N6FAB21416",
};

// The dict keys are a Set[String], the values a List[String],
// and both of those are a Collection[String].
let strings: Collection[String] =
  if mode == "keys":
    data.keys()
  else:
    data.values();

// Compute the total length of the strings in the collection.
[for s in strings: s.len()].sum()
```
