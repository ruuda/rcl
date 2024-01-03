# Multithreading

With a separate typecheck and evaluation phase, the typechecker already finds
all imports, so it can kick off loading, lexing, parsing, and typechecking the
imported file in the background, and the foreground thread can continue,
assuming the typecheck passes. All the way at the end, we can report errors from
all threads (be sure to sort them for reproducibility).

Evaluation is still sequential, but at least some stages can be pipelined. Note,
this assumes that the type is not imported from the other file. But files that
export types would be rare in a big repository where multithreading matters.

It is even possible to optimistically start evaluating imported files, though
that might be wasteful because some of the imports might be conditional. There's
another opportunity there: the typechecker can track whether the import is
conditional or not, and kick off full evaluation of non-conditional imports
already.
