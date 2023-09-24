# Roadmap

## Near term

* [WIP] Colored output for json.
 * The ability to turn coloring on or off, respect <https://no-color.org/>.
 * Add assertions.
 * Absorb the `highlight` command into `fmt` and make it use the same coloring?
 * Different output modes, RCL in addition to json (so we can preserve sets);
   possibly yaml output.
 * Preserve insertion order in dicts and sets.

## Method vs. field access

After playing with this for some time, I think I will give in and just use
different syntax for method calls vs. field access. Field access gets the dot,
method call gets `->`.

    let x = {
      len = 100;
      name = "x";
      contains = (name) => name == "prop";
    };
    // Returns [100, 2]:
    [x.len, x->len()]

    // Returns [true, false]:
    [x->contains("name"), x.contains("name")]

    // Returns [false, true]:
    [x->contains("prop"), x.contains("prop")]

Though it does make me slightly sad, the dot just looks so much friendlier.
Maybe field access should get the `->` instead?

## Trailing semicolons

Mixing `,` and `;` in the same dict is weird. Also, if I want to allow kwargs,
the natural separator would be comma rather than semicolon. Maybe just bite the
bullet?

    let x = {
      len = 100,
      name = "x",
      contains = (name) => name == "prop",
    };

Doesn't look so bad, I can live with this.

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
