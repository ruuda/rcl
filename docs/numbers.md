# Numbers

RCL features a single number type that can represent both integers, and numbers
with a decimal part.

 * Numbers are decimals with a finite range.
 * Numbers track the position of the decimal point. In particular,
   <abbr>RCL</abbr> will never turn floats (numbers that have a decimal point
   and/or exponent) into integers in the output.
 * Arithmetic is exact or fails, but never silently inexact. <!--
   TODO: That's going to be a challenge with division. -->

For reference documentation of the supported methods, see [the `Number`
type](type_number.md) in the language reference. For background about why
numbers work the way they do in <abbr>RCL</abbr>, see the blog post [_A float
walks into a gradual type system_][blog-float].

[blog-float]: https://ruudvanasseldonk.com/2025/a-float-walks-into-a-gradual-type-system

## Syntax

RCL supports the same number formats as json:

 * Decimal integers, e.g. `42`.
 * Numbers with decimal point e.g. `42.0`,
   optionally with exponent, e.g. `0.42e2`.

Aside from decimal numbers, <abbr>RCL</abbr> supports integers in other bases:

 * Hexadecimal with `0x` prefix, e.g. `0x2a`.
 * Binary with `0b` prefix, e.g. `0b101010`.

Numbers may contain underscores for readability, e.g. `100_000.000_000`.

## Precision

Numbers support up to 19 significant decimal digits, see
[_representation_](#representation) below for the technical details.

For input, if the source file contains more significant digits than what
<abbr>RCL</abbr> can represent, it will round to the nearest representable
number for numbers with a decimal point or exponent,[^1] and it will report
a range error for integers.

[^1]: This is the pragmatic choice. If <abbr>RCL</abbr> is used to query json
      documents, accepting an input encountered in the wild is more useful than
      rejecting it. Applications that care about excessive significant digits,
      serialize numbers as strings anyway, because many json parsers will parse
      numbers into 64-bit floats, which have less precision than <abbr>RCL</abbr>
      numbers.

For arithmetic, when the result cannot be represented exactly, <abbr>RCL</abbr>
will fail with an error. In that case, explicitly [rounding](type_number.md#round)
the number before performing arithmetic can help to bring the result back into
representable range.

## Representation

Numbers in <abbr>RCL</abbr> are rational numbers of the form
<var>m</var>&nbsp;×&nbsp;10<sup>&thinsp;<var>n</var> – <var>d</var></sup>.

 * **The mantissa**, <var>m</var>, is a signed 64-bit integer.
 * **The exponent**, <var>n</var>, is a signed 16-bit integer.
 * **The number of decimals**, <var>d</var>, is an unsigned 8-bit integer.

This representation enables the following:

 * All signed 64-bit integers can be represented exactly.
 * We can track the position of the decimal point.
   1 is represented as 1&thinsp;×&thinsp;10<sup>&thinsp;0 – 0</sup>,
   1.0 is represented as 10&thinsp;×&thinsp;10<sup>&thinsp;0 – 1</sup>,
   1.00 is represented as 100&thinsp;×&thinsp;10<sup>&thinsp;0 – 2</sup>,
   etc.
 * We can distinguish between `1.0` and `10e-1`, even though both have a
   mantissa of 10.
 * Together, this means that <abbr>RCL</abbr> can preserve the core formatting
   of numbers, which ensures that numbers can be losslessly transferred from
   input to output. In particular, <abbr>RCL</abbr> does not remove the decimal
   point from numbers that happen to be integral.

Note, numbers are _not_ <abbr>IEEE</abbr> floats. In particular, subtleties
such as NaN, infinities, and negative zero do not exist in <abbr>RCL</abbr>.
Where float arithmetic would produce such values, <abbr>RCL</abbr> reports an
error instead.
