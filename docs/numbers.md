# Numbers

This chapter explains the rationale behind how numbers work in <abbr>RCL</abbr>.
For how to use them, see the reference documentation.
TODO: Add those, link them.
TODO: Ensure syntax docs are up to date.

The handling of numbers is a subtle topic for a configuration language. The
design space of possible behaviors is large, and it’s a space of trade-offs,
there is no obvious best solution. For <abbr>RCL</abbr>, the following
considerations play a role:

**Evaluating a json document should be lossless.**<br>
Because <abbr>RCL</abbr> is a superset of json, json documents should evaluate
to themselves. Whitespace changes are okay, and changing escape sequences in
string literals too as long as the contents of the string are preserved. For
strings this is acceptable, because all sane applications interpret string
literals in the same way, and the resulting sequence of code points will be the
same regardless of escape sequences. This is not the case for numbers. Numbers
in json are essentially strings, and it is up to the application to interpret
them. Different applications may make different choices. Most applications would
consider `1.00` to be equivalent to `1.0`. But what about `1` and `1.0`? In
languages that have both integers and floats, the distinction may be important.
Because <abbr>RCL</abbr> aims to generate configuration for any application,
and it is not up to <abbr>RCL</abbr> to specify whether the distinction between
`1.0` and `1` is important, this means that <abbr>RCL</abbr> should not turn
`1.0` into `1`. (Turning `1.00` into `1.0` may be acceptable, but ideally
<abbr>RCL</abbr> would avoid that too.)
