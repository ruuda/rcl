# Roadmap

## Near term

 * Accept an expression through `--expr` for `rcl evaluate`.
 * Actually implement the `--in-place` for `rcl fmt`.
 * Add a `--follow` for `rcl fmt` that follows imports.
 * Add a way to output a depfile for Make/Ninja.

## Mid-term

 * Yaml output mode.
 * Preserve insertion order in dicts and sets, GC'd runtime.
 * Absorb the `highlight` command into `fmt` and make it use the same coloring?
 * Evaluate whether to add a tuple type to describe heterogeneous lists of
   statically known length.

## Long-term

 * Type system.
 * Add a more efficient runtime system, possibly with a garbage collector.
 * Multithreaded evaluation. Due to strict evaluation and being purely
   functional without side effects, this is actually feasible: when we evaluate
   `f(a, b, c)`, we can evaluate `a`, `b`, and `c` in parallel. Similarly in
   operator chains like `a + b + c`. It might use a work-stealing scheduler,
   where the current thread puts something to evaluate up for grabs, continues
   to evaluate the next thing, and once it cannot continue, it either waits for
   the next result or evaluates it itself.
