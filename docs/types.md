# Types

_**Warning**: The type system is a work in progress. In particular, record types
are missing, and the implementation of the typechecker is too ad-hoc._

RCL has a type system that can help to prevent bugs and make configuration more
self-documenting.

 * **The type system is gradual.** You can add type annotations to variables,
   and those annotations will be enforced.[^1] Variables that are not annotated
   are unconstrained.
 * **The type system is static.** Type errors in unreachable code will be
   reported. Sufficient type information needs to be available at typecheck time
   to be able to prove that an expression contains a type error. When not all
   types are known statically, typechecks are deferred to runtime.
 * **The type system is structural, not nominal.** Two different type aliases of
   the same type can be used interchangeably.

[^1]: Unlike in Python, where external tools can statically typecheck type
      annotations, but they are ignored by the runtime.

Type annotations can be applied at [let bindings](syntax.md#let-bindings) by
adding a colon after the name:

```rcl
let x: Int = 42; x
```

The names of all types start with a capital letter.

## Primitive types

The primitive types are:

 * `Bool`, the type of `true` and `false`.
 * `Null`, the type of `null`.
 * `String`, the type of strings.
 * `Int`, the signed integer type.
 * Currently there is no type for non-integer numbers. (TODO: Add one.)

## Collection types

There are three built-in generic collection types:

 * `Dict[K, V]`, a dictionary with keys of type `K` and values of type `V`.
 * `List[T]`, a list with elements of type `T`.
 * `Set[T]`, a set with elements of type `T`.

For example, we can annotate this dict as follows:

```rcl
let port_names: Dict[Int, String] = {
  22: "ssh",
  80: "http",
  443: "https",
};
```

## Record types

The intention is to support record types and type aliases, but this is not yet
implemented. They would look roughly like this:

```rcl
type User = {
  full_name: String,
  email: String,
  uid: Int,
  groups: Set[String],
};

let users: List[User] = [
  {
    full_name = "Eldon Tyrell",
    email = "etyrell@tyrell.com",
    uid = 1,
    groups = {"executive"},
  },
  {
    full_name = "Rachael Tyrell",
    email = "rachael@tyrell.com",
    uid = 7,
    groups = {"research"},
  },
];
```

## Function types

Function types are written as an argument list between parentheses, a thin
arrow, and then the result type:

```rcl
let add: (Int, Int) -> Int = (x, y) => x + y;
```

The parentheses are mandatory, even for functions that take a single argument.
A trailing comma is optional.

## The Any type

Any possible value is an instance of the `Any` type. It is the least informative
type: nothing more specific is known statically. Variables that have type `Any`
never cause static type errors, but they can still cause runtime type errors.

```rcl
let x: Int = 32;
let y: Any = x;
// Not a static type error: an expression with type Any could evaluate to a
// string, so assigning it to a variable of type String is allowed. But at
// runtime, we verify that the value is really a string, and that check fails.
let z: String = y;
z
```

Annotating a variable with `Any` is not useful. At best it behaves the same
as not annotating the variable, but in the worst case it forces the typechecker
to discard type information that it was able to infer. However, `Any` can be
useful as part of a more complex type, to partially enforce some structure:

```rcl
let widgets: Dict[String, Any] = {
  frobnicator = { foobar = 42 },
  turbo-encabulator = { prefabulated = true, bearings = "spurving" },
};
```

## The Void type

No possible value is an instance of the `Void` type. It is the inferred element
type for empty collections.

```rcl
// Inferred to have type `List[Void]`.
let xs = [];
```

## Static checks

The type system is designed to help prevent bugs first and foremost, and to help
make code more self-documenting and readable second. It is _not_ a goal that
every possible document that could be evaluated without the typechecker, is
well-typed. For example, the following program has a static type error:

```rcl
let xs: List[Int] = [];
// Error: Expected String but found Int.
let ys: List[String] = xs;
ys
```

If we remove the type annotations, the document evaluates to `[]`, so the
typechecker rejects a document that could have been evaluated.

Here is another example of a program that has a static type error, despite
not even having type annotations:

```rcl
// This list is empty, but nonetheless inferred to have type `List[Int]`.
let integers = [for i in [1, 2, 3]: if i > 100: i];
// Type error: Expected Bool as argument to `not`, but found `Int`.
[for i in integers: not i]
```

Without the typechecker this code would execute just fine, because the
ill-typed code path is never executed. But this above code is very likely a bug,
and would fail when the parameters change. Therefore <abbr>RCL</abbr> prefers
keeping the typechecker simple over making every executable document well-typed.
