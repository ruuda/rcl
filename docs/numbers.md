# Numbers

RCL features a single number type that can represent both integers, and numbers
with a decimal part. This chapter describes how numbers work in <abbr>RCL</abbr>
and what the rationale behind that is. For reference documentation of the
supported methods, see [the `Number` type in the language reference](type_number.md).

<!-- TODO: Link to blog post. -->

 * Numbers in <abbr>RCL</abbr> are decimals with a finite range.
 * Arithmetic is exact or fails, but never silently inexact. <!--
   TODO: That's going to be a challenge with division. -->
 * The number of decimals is preserved. In particular, for numbers that end in
   `.0` in the input, the `.0` will be preserved in the output.

## Syntax

RCL supports the same number formats as json:

 * Decimal integers, e.g. `42`.
 * Decimal floats, e.g. `42.0`, optionally with exponent, e.g. `42.0e10`.

Aside from decimal numbers, <abbr>RCL</abbr> supports integers in other bases:

 * Hexadecimal with `0x` prefix, e.g. `0x2a`.
 * Binary with `0b` prefix, e.g. `0b101010`.

Numbers may contain underscores for readability, e.g. `100_000.000_000`.

## Precision

For arithmetic, when the result cannot be represented exactly, <abbr>RCL</abbr>
will fail with an error. In that case, explicitly [rounding](type_number.md#round)
the number before performing arithmetic can help to bring the result back into
representable range.

For input, if the source file contains more significant decimals than what
<abbr>RCL</abbr> can represent, it will round to the nearest representable
number.[^1]

[^1]: This is the pragmatic choice. If <abbr>RCL</abbr> is used to query json
      documents, accepting an input encountered in the wild is more useful than
      rejecting it. Applications that care about excessive significant digits,
      serialize numbers as strings anyway, because many json parsers will parse
      numbers into 64-bit floats, which have less precision than <abbr>RCL</abbr>
      numbers.

## Representation

Numbers in <abbr>RCL</abbr> are rational numbers of the form
<var>m</var>&nbsp;×&nbsp;10<sup><var>w</var></sup>,
where <var>m</var> (the mantissa) is a signed 64-bit integer,
and <var>w</var> (the exponent) is a signed 16-bit integer.
This means that <abbr>RCL</abbr> can represent all signed 64-bit integers exactly.
This is also how <abbr>RCL</abbr> preserves the number of decimals:
`1` is represented as 1&thinsp;×&thinsp;10<sup>0</sup>,
`1.0` is represented as 10&thinsp;×&thinsp;10<sup>-1</sup>,
`1.00` is represented as 100&thinsp;×&thinsp;10<sup>-2</sup>,
etc.

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
