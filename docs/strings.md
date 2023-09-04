# Strings

RCL has multiple forms of string literals. String literals in RCL support
string interpolation and are most similar to those in Python, with some
inspiration from Nix and Rust. All json strings are valid in RCL[^1].

Strings can be quoted with double quotes:

    "Hello, world"

Or with triple double quotes:

    """
    Hello, world
    """

In both cases, add an `f` to enable interpolation.

    f"Hello {greetee}"

[^1]: Except for `\u` escape sequences that encode surrogate code points
(U+D800 through U+DFFF). While a pair of such escape sequences may together be
valid, a single one is not, so at this point RCL opts to not implement surrogate
pairs.

## Multiline strings

In all string literals, newlines are preserved verbatim. Inside a `"""`-quoted
string, any shared leading whitespace gets removed, as well as the initial
newline if it directly follows the `"""`. The following strings are identical:

    let a = "Hello\n  World\n";
    let b = """Hello\n  World\n""";
    let c =
        """
        Hello
          World
        """;
    let d =
        """Hello
          World
        """;
    let e = "Hello
      World";

Inside a `"`-quoted string, `"` itself needs to be escaped as `\"`, but inside
a `"""`-quoted string, `"` does not need to be escaped. Inside a `"""`-quoted
string, `"""` itself needs to be escaped, but escaping one of the three quotes
is sufficient.

## Interpolation

When an `f` precedes a string literal, this enables _interpolation_, and the
string is called a _format string_ or _f-string_ for short. Interpolation means
that the string literal should have one or more _holes_, delimited by `{}`. The
hole can contain any expression that evaluates to a string. (TODO: How to type
string formattable types?). Holes can themselves contain format strings, there
is no nesting limitation. (Like in Nix, but unlike Python.)

An example of string interpolation:

    let generations = {
      "Leon Kowalski": 6,
      "Rachael": 7,
      "Roy Batty": 6,
    };
    [
      for name, generation in generations:
      f"{name} was a Nexus-{generation} replicant."
    ]

## Escape sequences

Inside strings, `\` initiates an escape sequence. The same escape sequences as
in json are supported, which includes `\"`, `\\`, `\r`, `\n`, and `\t`.

A `\u` initiates an escape sequence for an arbitrary Unicode scalar value. It
can be followed by either exactly 4 hex digits (like in json and Python), or by
a variable number of hex digits enclosed in `[]` (like in Rust, except enclosed
in `[]` instead of `{}`). The following strings are identical:

    let a = "\n";
    let b = "\u000a";
    let c = "\u[0a]";
    let d = "\u[00000a]";
