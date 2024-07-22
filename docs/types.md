# Types

_The type system is a work in progress. In particular, record types are missing._

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

## Union types

A union type allows instances of any member of the union. For example:

```rcl
// Both of these are okay!
let x: Union[Int, String] = 0;
let y: Union[Int, String] = "zero";

// But this is a type error: expected Int or String but found Null.
let z: Union[Int, String] = null;
```

Unions have two or more members:

```rcl
// Error, this is equivalent to using Int directly, so the Union is pointless.
let u1: Union[Int] = 42;

// But these are all okay.
let u2: Union[Int, String] = 43;
let u3: Union[Int, String, List[Int]] = 43;
let u4: Union[Int, String, List[Int], Bool] = 43;
```

## Type inference

In all code, annotated or not, <abbr>RCL</abbr> will infer types. Type inference
is forward-only[^2] and — with one exception — bottom-up. For example,
<abbr>RCL</abbr> can infer the following types:

```rcl
// Elements are inferred to have type `Int`, `xs` has type `List[Int]`.
let xs = [1, 2, 3];

// Inferred to have type `List[Int]`, because `xs` has that type.
let ys = xs;

// Type error: operator `or` expects `Bool`, but `ys[0]` has type `Int`.
ys[0] or ys[1]
```

Because type inference is forward-only, function arguments have type `Any` when
they are not explicitly annotated:

```rcl
// Inferred to have type `(Any, Any) -> Bool`.
let xor = (x, y) => (x or y) and (not (x and y));
```

Because we use `x` and `y` with `and` and `or` operators, they have to have type
`Bool`. However, the typechecker has to assign a type to `x` and `y` _before_
typechecking the function body, and without additional information, it can only
pick `Any`. When it continues to check the `or` expression, it inserts a runtime
type check to confirm that `x` and `y` are `Bool`. To give `xor` a more precise
type, we have to annotate it:

```rcl
let xor: (Bool, Bool) -> Bool = (x, y) => (x or y) and (not (x and y));
```

The exception to bottom-up inference are type annotations. The typechecker
propagates type requirements top-down into expressions. For example for list
elements:

```rcl
let xs: List[Int] = [
  42,
  // Type error: expected `Int` but found `String`.
  "43",
];
```

This means that the placement of a type annotation can affect how <abbr>RCL</abbr>
reports a type error. Although all annotations are enforced, placing annotations
closer to definitions generally results in clearer error messages. Suppose we
modified the above example as follows:

```rcl
// Inferred to have type `List[Any]` because we mix `Int` and `String`.
let xs = [42, "43"];

// Runtime type error: expected an instance of `Int`, but found string `"43"`.
let ys: List[Int] = xs;
```

In this case <abbr>RCL</abbr> can only report that a value failed a type check,
and that this happened inside a list at index 1, but it can no longer pinpoint
the location in the source code where the offending value came from.

[^2]: Some languages, in particular those that use Hindley–Milner style type
      inference such as Haskell and Rust, can use information that occurs at a
      later point in the program to infer a type earlier in the program. While
      this is powerful, it also makes type errors more difficult to interpret,
      especially when combined with record types. Because <abbr>RCL</abbr> aims
      to be a language that humans can easily _reason_ about, we opt for less
      powerful but simpler forward-only inference.

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

## Further reading

For more background on the design of the type system and its implementation,
see the blog post series, <em>A type system for <abbr>RCL</abbr></em>.

 1. [_Introduction_][blog-1] lays out the requirements of a
    type system for a configuration language.
 2. [_The type system_][blog-2] describes the resulting type
    system and the theory underpinning it.
 3. [_Related work_][blog-3] highlights type systems that
    inspired <abbr>RCL</abbr>’s type system, and contrasts it with a few other
    configuration languages.
 4. [_Implementing a typechecker in Rust_][blog-4] goes over
    some of the implementation details, including how the error reporting works.

This series reflects the type system as of <abbr>RCL</abbr> 0.4.0 in July 2024.

[blog-1]: https://ruudvanasseldonk.com/2024/a-type-system-for-rcl-part-1-introduction
[blog-2]: https://ruudvanasseldonk.com/2024/a-type-system-for-rcl-part-2-the-type-system
[blog-3]: https://ruudvanasseldonk.com/2024/a-type-system-for-rcl-part-3-related-work
[blog-4]: https://ruudvanasseldonk.com/2024/implementing-a-typechecker-for-rcl-in-rust
