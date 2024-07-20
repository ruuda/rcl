# Roadmap

## In progress

 * Type system.
 * Add an `rcl build` subcommand.

## Near term

 * Accept an expression through `--expr` for `rcl evaluate`.
 * Add a `--follow` for `rcl fmt` that follows imports.
 * Add `to_lower_ascii`, `to_upper_ascii` functions. Maybe even `to_snake_case`.
 * Add integer division and modulo methods (or operators, but probably methods
   are nicer here).

## Mid-term

 * Yaml output mode.
 * Preserve insertion order in dicts and sets, GC'd runtime.
 * Evaluate whether to add a tuple type to describe heterogeneous lists of
   statically known length.
 * Add a spread operator for inside collections, and evaluate whether it should
   supersede the dict/set union operator. It would solve some typing issues, and
   it's easier to format in a pleasant way.
 * Add nested fields to record syntax, i.e. `{x.y = "z"} == {x = {y = "z" }}`.
 * Mitigate Trojan Source pitfall (https://trojansource.codes/). We can't ban
   Bidi code points from strings because it would break json compatibility,
   but we can ban it from comments, and we can make the formatter turn it into
   escape sequences in strings.

## Long-term

 * Add a more efficient runtime system, possibly with a garbage collector.
 * Multithreaded evaluation. Due to strict evaluation and being purely
   functional without side effects, this is actually feasible: when we evaluate
   `f(a, b, c)`, we can evaluate `a`, `b`, and `c` in parallel. Similarly in
   operator chains like `a + b + c`. It might use a work-stealing scheduler,
   where the current thread puts something to evaluate up for grabs, continues
   to evaluate the next thing, and once it cannot continue, it either waits for
   the next result or evaluates it itself.
