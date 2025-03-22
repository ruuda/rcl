# Collection

The `Collection` type is the supertype of [`List`](type_list.md) and [`Set`](type_set.md).
It is a shorthand:
instead of the verbose [`Union[List[T], Set[T]]`](types.md#union-types),
we can write `Collection[T]`.

The `Collection` type should rarely be used directly. Prefer to use the more
specific `List` or `Set` instead. Why then, does `Collection` exist? It acts as
the type for “things that can be iterated over”, which the typechecker uses when
it checks [`for` loops](syntax.md#comprehensions).

The methods that are available on a value that fits `Collection[T]` depend on
whether that value is a list or a set, though many methods are available on both.
See the docs for [`List`](type_list.md) and [`Set`](type_set.md) for full details.
