# Typechecking

In we could do Hindley-Milner typechecking. At every unknown, create a type
variable, then emit constraints where appropriate, then solve them (on the go
if possible). But one challenge is: RCL has subtyping. For example the record
type

    {
        name: String,
        email: String,
    }

is a subtype of

    Dict[String, String]

Then there is also the `Dynamic` type, which I'm not sure should be represented
as a type there, but if we do, then in one sense types become a lattice which
makes many things elegant.

## Lattice

Another approach is to define the lattice, and to do syntax-directed checking.
But then we need to be super precise, and for example for dict comprehensions,
I don't think that we can infer the type in general, so in some cases we'll need
to give up on inferring a record type and instead infer `Dict[String, Dynamic]`,
and then the rest needs to be checked at runtime.

## Pushing down constraints

One idea that I implemented a prototype of: all this inferring of record types,
the types can become huge, and it may be expensive, for no gain if the user
didn't even ask to check. So what if we "push down" type annotations into the
bindings?

    let xs: List[User] = [
        // Here we'll validate the literal against `User`, and we can do that
        // top-down when we enter.
        { name = "...", email = "..." },
    ];
    let ys = [
        // Here we don't validate anything at all, the expected type is `Dynamic`.
        { name = "...", email = "..." },
    ];

This became a bit unwieldy, though maybe it can be rescued. It also leads to
weird situations where extracting something into a let binding can change the
behavior. Consider this:

    // Case 1: Push requirement down, error reported at the definition site.
    let xs: List[User] = [
        // Here we do validate the literal immediately against `User`.
        { name = "...", e-mail = "..." },
        //              ^~~~~~ field name misspelled.
    ];

Versus this:

    // Case 2: Extract into let-binding.
    // Type inferred for `user` is `Dynamic`, or maybe `Dict[String, String]`.
    let user = { name = "...", e-mail = "..." };
    // We can statically check the `List` requirement, but the inner check that
    // every element is a `User`, we now need to insert a runtime check there.
    // So the error is going to be about a runtime value, no longer syntactic!
    let ys: List[User] = [user];

Maybe this is fine. If you take the imperative point of view, then in case 2,
we defined our user already and got past that point without errors. Then if we
do something with it later, that could cause an error. Sure. But if we take the
functional point of view, everything is immutable, and I want referential
transparency. Giving a value a name should not change the behavior of the
program. (It doesn't for correct programs, it only changes the way the error is
discovered. But still, it feels deeply wrong to me that extracting something
changes the error.)

## Constraints after all?

So let's explore, can we push down types, but also leverage constraints, such
that they do get propagated backwards through let bindings and the like?

    let user: t0 = { ... };
    let ys: List[User] = [user];
            ----------   ------
               tmp0       tmp1

When checking the let binding, we would have an expected type from the binding,
let's call it `tmp0`. This is not a type variable at the type level, it is a
variable that names a type! (Which might be a type variable.) Then we typecheck
the right-hand side, and it gives us type `tmp1`. And then we emit a constraint
`tmp1 <: tmp0`.

When we typechecked the right-hand site, we introduced a type variable, `t1`,
and for every field we added a constraint `field-type <: t1`. That is, `t1` is a
supertype of all elements. If we would work with a lattice of concrete types, it
would be the meet (the least supertype) of the elements, but we are not, so we
probably only have a bag of constraints and the type variable.

If we have a point where we need to solve the constraints, and we encounter
a free type variable, then we can instantiate it to Void. If we do the trick
above to typecheck `[]`, we find `List[t1]` where `t1` is free and it has no
other constraints relating to it, we can instantiate to `List[Void]`. But let's
say it was not free. When we typecheck `[user]` with `user: t0`, we find type
`List[t1]; t0 <: t1`.

We were solving the constraint of `List[t1] <: List[User]`, we can discharge the
`List` on both sides, then we find `t1 <: User`, so by transitivity we learn
that `t0 <: User`. Great!

But we cannot just go back and report the type error syntactically at the
location of the `user` literal, because we already visited it in the AST and
we're not going to locate it again. Or should we store it with the contraints?

And although these constraints are very powerful, they can also lead to spooky
action at a distance. Maybe forward-only analysis is more user-friendly afte all
because it has clearer errors, even if it is less powerful? And then it does
mean that putting things in let bindings can change the error?
