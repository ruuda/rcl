# Numbers

This chapter explains the rationale behind how numbers work in <abbr>RCL</abbr>.
For how to use them, see the reference documentation.
TODO: Add those, link them.
TODO: Explain that we don't support arithmetic on float at this time.

## Design considerations

TODO: Rewrite this section to focus on explaining why numbers work the way they
do in a way that is useful to users. Explored dead ends are not useful as
reference material, maybe it should go at the end in the development documentation.

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
application to interpret them. For example, in Javascript, `1` and `1.0` parse
as the same value, but in Python they do not. Because <abbr>RCL</abbr> aims to
generate configuration for any application, and it is not up to <abbr>RCL</abbr>
to specify whether the distinction between `1.0` and `1` is important, this
means that <abbr>RCL</abbr> should not turn `1.0` into `1`.

**Losing decimal precision and formatting are acceptable.**</br>
We saw that we need to preserve the distinction between integers and floats: we
can’t turn `1.0` into `1`. What about Turning `1.00` into `1.0`, or `1e3` into
`1000.0`? Ideally <abbr>RCL</abbr> would avoid that, but if preserving
formatting gets in the way of other requirements, changing the formatting
of a number is acceptable.

What about numbers with many significant digits? Is it okay to lose precision on
parse? On the one hand, it’s better to fail loudly and reject the input with a
range error, than to silently drop digits that may be essential. On the other
hand, documents formatted with many decimals that _don’t_ have important meaning
occur in the wild, and for <abbr>RCL</abbr> to be usable as a json query
language, losing (fake) precision is more useful than rejecting an input document.

Due to differences in how languages interpret json numbers, it is already
extremely fragile to depend on how numbers are formatted. For applications that
work with numbers that can’t be represented by a <abbr>IEEE</abbr> float, the
safe choice is to encode the number as a json string rather than a number. This
makes the discussion about preserving formatting a mostly hypothetical one. The
pragmatic choice is to prefer silently losing precision over rejecting an input.

**All values can be compared.**<br>
All values in <abbr>RCL</abbr> can be compared for equality or ordering against
any other value. This requirement follows from being a json superset with
support for sets and non-string dict keys: json lists can be heterogeneous, so
we should also support heterogeneous sets. The `==` and `<` operators
furthermore enable explicit comparison.
TODO: Should `<` at least report an error?
Unlike statically typed languages that can reject `1 == 1.0` or `{1, 1.0}` with
a type error, we have to pick a behavior here.

Equality should behave in a sane way with respect to types. If `a == b`, and
`a: A` and `b: B`, then we should have `a: B` and `b: A` as well. However, this
leads to the bizarre conclusion that we have to allow this:

```rcl
let n: Int = 1.0;
```

So instead, <abbr>RCL</abbr> chooses to not implement numeric equality. Floats
and ints are different, just like floats and strings are different.
