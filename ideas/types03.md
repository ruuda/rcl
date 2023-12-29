# Types, iteration 3

## Constraints

 1. The main goal is to document the schema of some data, for reference when
    other people add an instance of it, and to be able to verify that data
    against the schema.
 2. It should be possible to use RCL without type annotations, and it should
    not behave differently than it currently does.
 3. Let's not worry too much about possible laziness and imports right now,
    focus on the problem at hand.

## Observations

Json data can be difficult to type. For example, what is the type of `felix` in
the following?

    let felix = {
      name = "Felix",
      species = "cat",
      age_years = 10,
    };

If we have struct types, union types, and a generic dict type, we could describe
this in many ways. Some options:

    // Static:
    felix1: {
      name: String,
      species: "cat" | "dog",
      age_years: Int,
    }

    // Dictly-typed (triggers Python Dict[str, Any] PTSD):
    type Species = "cat" | "dog";
    felix2: Dict[String, String | Species | Int]

Although both are valid, the former is more precise and more useful than the
latter ... except when we start to mess with the fields. For example, we have
the method `Dict.except`:

    Dict.except: (self: Dict[K, V], key: K) -> Dict[K, V]

This type is compatible with `felix2`, but what do we do when called with a
value of `felix1`?

    let felix1-a = felix1.except("species");
    let felix2-a = felix2.except("species");

What is the type of `felix{1,2}-a`? Some options:

 * Coerce `felix1` to fit the type `Dict[K, V]` for appropriate values of `K`
   and `V`. This is possible, we get the type for `felix2`. Then the call to
   `except` does not change the type, so `felix1-a` has the same type as
   `felix2`. This is a shame, because it has `| Species` in there, which is now
   no longer needed.
 * Something dependently typed with row types like in PureScript, see below.
 * We don't actually need to give it a type. The function works well at runtime,
   it's implementation does not depend on the type. `felix{1,2}-a` just don't
   have a type, until we add an explicit annotation and force it to have one.

Dependently typed, might be:

    Dict.except:
      forall K :: Type:
      forall ToRemove :: Type:
      forall to_remove :: ToRemove:
      where key == to_remove:
      (self: { to_remove: K, ..fields }, key: ToRemove) -> { ..fields }

But that is getting rather messy! A solution to sidestep the problem would be to
not make `Dict.except` a method, but some built-in syntactic construct, for
example the `-` operator. Then we don't need to describe how it works with a
function type. (This is also why `import` should be a separate construct, not
a built-in function `std.import`.)

With a separate construct, we don't need a _syntax_ to describe the complex type
of the operation. We can just say hand-wavey how it acts on types:

    { field: K, ..fields } -> { ..fields }

Fortunately I don’t have so many methods that are difficult to type.

 * For `Dict.contains`, coercion is fine.
 * For `Dict.get`, we can take the type of the default value as the return type.
   That makes `self` still difficult to type though.
 * For `Dict.keys` and `Dict.values`, if you want a collection, then coercion is
   actually what you want.
 * For `{Dict,String,Set}.len`, we don't care about an accurate `self` type at
   all.
 * For `List.group_by`, the element type is completely opaque, so a type
   variable works fine there. For `List.fold` it's also like that.

So let's forget that `Dict.except` exists for now, we can even remove it. Maybe
the `-` operator should act on sets on the right-hand side so you can remove
multiple fields, but anyway.

Dict union is already an operator, so even if we can't write down its type in a
notation that would be supported by the language, we can say what it does:

    lhs: { ..fields1 }
    rhs: { ..fields2 }
    lhs | rhs : { ..fields1, ..fields2 }

And it would be right-biased on the type level, just like it is on the value
level.

## Where do types live?

Variables bind values. Multiple types can describe the same value. So we might
make types a property of the _variable_. Assignment to a typed variable would be
a runtime type check:

    let felix = { name = "Felix", species = "cat", age_years = 10 };
    let felix1: { name: String, species: "cat" | "dog", age_years: Int } = felix;
    let felix2: Dict[String, String | Int] = felix;

The runtime value would be the same, all three variables would point to the same
value in the `Rc`. But we could attach some type information to the bindings.

This also makes typechecking possible for functions. They don't have values
assigned to the variables at check time. It should be possible to type the
functions explicitly, but inference should work too.

## Optional and excess fields

Let's say we have Felix again:

    let felix = { name = "Felix", species = "cat", age_years = 10 };

Should it be valid to assign it to a variable with fewer fields?

    let felix1: { name: String, age_years: Int } = felix;

On the one hand, yes. All the fields we try to access on `felix1` are there. On
the other hand, should `felix1.species` now be an error even though it's there
on the value? So I think struct types should be closed by default, but we might
add an escape hatch and make it open.

    // Not allowed:
    let felix1: { name: String, age_years: Int } = felix;

    // Allowed:
    let felix2: { name: String, age_years: Int, ... } = felix;

And supposing we did have variables `felix1` and `felix2`, then field lookup
works like this:

    // Definite type error, `felix1` does not have this field.
    felix1.species

    // Deferred typecheck; possible type error. If at runtime all values do have
    // the field, then this is allowed. The type of the expression can differ
    // for different values of `felix2`.
    felix2.species

But adding an ellipsis allows any arbitrary fields. In some cases we want fields
that are optional, but when they are present, they must have a specific type.
We could encode that:

    let felix1: { name: String, species?: "cat" | "dog", age_years: Int } = ...;

    // If this field is present, it must have type `"cat" | "dog"`.
    felix1.species

Bikeshed: Is the separator for optional fields `:?` or `?:`? I find `:?`
slightly easier to type. TypeScript uses `?:`.

## Runtime representation

In addition to a representation for types, there should be an `Untyped` type,
which means “typecheck deferred”. It would be the default for let bindings and
function arguments without explicit type annotation.

Typing rules:

 * A typed let binding where the right-hand side is `Untyped` has to be checked
   every time it is bound. (Which is once at the top level, but can happen
   multiple times in a comprehension, or in a function body, though those just
   evaluate the body multiple times so that works the same as top-level.)
 * A typed let binding where the right-hand side is typed, unifies the two
   types. Unification sounds symmetric, but it should really be more of a
   coercion or fits-check to the left-hand type.
 * Field access on a typed dict has a statically known type, from the dict.
 * Field access on a dict of type `Untyped` results in a value of type
   `Untyped`.

So the current runtime type checks will have to stay to a large extent, but we
can add static type checks as an addition.

This also fortunately ensures that we don't have to keep very big types around
for every value, or do very deep type inference. Say you write a very big list
literal, with dicts with many fields, all of the same type. If we would by
default type every expression (possibly with inference), then we'd separately
infer the type of every dict, get the same type on all of them, then unify them
O(n) times, which is going to be a lot of runtime work that may go to waste if
the type is not used. Instead, if we don't typecheck anything until a typed let
binding, then we _have_ the supposed type given by the user, and we only need to
check that the runtime value conforms to it. No unification needed, no need to
store a type per expression.

## The type universe.

The simplest use case would be type annotations on let bindings. Let's ignore
functions for now, and complex dict operations.

We have the primitive types:

 * `Bool`
 * `String`
 * `Null`
 * `Int` ... Should there be `UInt` as well? And how to deal with number types
   in general? Allow constraints like in Cue?

We have type constructors for the collections:

 * `Dict[K, V]`
 * `Set[T]`
 * `List[T]`

We have struct types, which are also dictionaries at runtime, but which can have
a more accurate type description:

 * Open: `{ field_n: TypeN, ... }`
 * Closed: `{ field_n: TypeN }`
 * With optional fields: `{ field:? Type }`.

String literals can also be types, which have a single valid value: that string:

 * `"foo"`, `"bar"`, etc.

Values of a union type can have either value:

 * `Int | Bool`
 * Can be used multiple times: `Int | Bool | String`.

It would have no special meaning on struct types. But maybe we do want a way to
combine struct types, that reflects the behavior of the `|` operator?

    type Animal = { species: "cat" | "dog", age_years: Int };
    type Pet = { name: String };
    let felix: Animal + Pet = { ... };

But then it's not so logical that `|` acts as union on dicts, if it doesn't do
that on the type level. Maybe the dict union operator should be `+` instead?
That would also be symmetric with making `-` the operator to omit keys.

How does TypeScript do all of this? They are facing pretty much the same
problem, they might have solved it in a reasonable way, and at the very least
many people will be familiar with it. Also PureScript row types, but as far as
I'm aware they don't have operators at the type level, only ways to combine the
rows.

Let's move on to functions, we can give them arrow types:

    let double: Int -> Int = x => x * 2;
    let add1: (Int, Int) -> Int = (x, y) => x + y;

If we choose to allow it, we could put types on the arguments too:

    let add2 = (x: Int, y: Int) => x + y;

If we go with the implementation outlined before, there is a subtle difference.
For `add1`, we have the type statically, and we can compute the type of
`add1(x, y)` without executing the call. On the other hand, `add2(x, y)` is
untyped. As a first step of the call, we would verify that the provided
arguments have the correct types. For `add1`, we have to typecheck the entire
body to confirm the return type.

Finally, we need type variables to type many of the builtin methods. We could
have a rule that all free variables are type variables, but maybe that's
fragile. Maybe type variables need to be lowercase, and type constructors
uppercase, like in Haskell?

    Dict.get: (self: Dict[k, v], key: k, default: v) -> v

Type variables are one way to type an empty dict or list, though if we had a
type lattice that may be even nicer.

    let empty_list: List[a] = []
    let empty_dict: Dict[k, v] = {}
    let empty_list: List[Void] = []

But we don't need the `Void` type if we don't infer a type for the empty list in
the first place.
