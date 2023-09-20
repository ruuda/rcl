# Method vs. field access

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
