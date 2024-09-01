# Numbers

This chapter explains the rationale behind how numbers work in <abbr>RCL</abbr>.
For how to use them, see the reference documentation.
TODO: Add those, link them.
TODO: Ensure syntax docs are up to date.

## Design considerations

The handling of numbers is a subtle topic for a configuration language. The
design space of possible behaviors is large, and it’s a space of trade-offs,
there is no obvious best solution. For <abbr>RCL</abbr>, the following
considerations play a role:

**Evaluating a json document should be lossless.**<br>
Because <abbr>RCL</abbr> is a superset of json, json documents should evaluate
to themselves. Whitespace changes are okay, and changing escape sequences in
string literals too as long as the contents of the string are preserved. For
strings this is acceptable, because all sane applications interpret string
literals in the same way, and the resulting sequence of code points does not
depend on how they are represented in the source. This is not the case for
numbers. Numbers in json are essentially strings, and it is up to the
application to interpret them. Different applications may make different
choices. Most applications would consider `1.00` to be equivalent to `1.0`. But
what about `1` and `1.0`? In languages that have both integers and floats, the
distinction may be important. Because <abbr>RCL</abbr> aims to generate
configuration for any application, and it is not up to <abbr>RCL</abbr> to
specify whether the distinction between `1.0` and `1` is important, this means
that <abbr>RCL</abbr> should not turn `1.0` into `1`.

**Losing decimal precision and formatting are acceptable.**</br>
We saw that we need to preserve the distinction between integers and floats: we
can’t turn `1.0` into `1`. What about Turning `1.00` into `1.0`, or `1e3` into
`1000.0`? Ideally <abbr>RCL</abbr> would avoid that, but if preserving
formatting gets in the way of other requirements, changing the formatting
of a number is acceptable.

What about numbers with many significant digits? In some domains, it is common
to work with a decimal precision that cannot be represented by a 64-bit
<abbr>IEEE</abbr> float. Is it okay to lose precision on parse? On the one hand,
it’s better to fail loudly and reject the input with a range error, than to
silently drop digits that may be essential. On the other hand, documents printed
with many decimals that _don’t_ have important meaning, do occur in the wild as
well, and for <abbr>RCL</abbr> to be usable as a json query language, losing
(fake) precision is more useful than rejecting an input document.

Due to differences in how languages interpret json numbers, it is already
extremely fragile to depend on how numbers are formatted. For applications that
work with numbers that can’t be represented by a <abbr>IEEE</abbr> float, the
safe choice is to encode the number as a json string rather than a number. This
makes the discussion about preserving formatting a mostly hypothetical one. The
pragmatic choice is to prefer silently losing precision over rejecting an input.

**Numeric equality must work as expected.**<br>
If integers and floats have different representations, and the type system
allows comparing them, then the comparsion should respect numeric equality.
Defining `1.0 == 1` as `false` would violate the principle of least surprise.
Making `1.0 == 1` fail with a type error does not fully sidestep the problem,
because we have sets. Is `{1.0, 1}` a set with two elements, or only one? If
it’s a set with one element, how do we explain this?

```rcl
// This is well-typed.
let xs: Set[Int] = {1};

// Having value equality respect numeric equality demands this.
assert 1 == 1.0, "Numeric equality holds.";
assert {1} == {1, 1.0}, "Sets are equal when their elements are equal.";

// If the right-hand side is equal to `xs`, then these must be well-typed.
let ys: Set[Int] = {1, 1.0};
let zs: Set[Int] = {1.0};

// But that leads to the bizarre conclusion that this must be well-typed.
let n: Int = 1.0;
```

If formatting is not a property of numbers, then the above conclusion may be
acceptable. The integers are a subset of the reals, and although `1.0` might
have type `Float`, it is numerically an integer. But then how should formatting
work? If we preserve the original formatting of numbers as much as possible,
then we might print values that are annotated as `Int` with a decimal point.
That seems weird too! Should the type annotation then change the formatting (but
not the numeric value) of the number, so we output `n` as `1` rather than `1.0`?
That violates the unwritten rule that adding a type annotation does not change
the behavior of the program.[^1]

[^1]: There is a small precedent for this though: annotating `{}` with a `Set`
type makes it an empty set rather than an empty dict, which when exported to
json changes the output from `{}` to `[]`. But that really makes it a different
_value_, `{}` as set and `{}` as dict are not equal, whereas `1.0` and `1` are.
