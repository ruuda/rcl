# `rcl build`

There is now the possibility to generate a depfile, and generate Ninja, etc.
But the overhead is quite large. For a big repository it's fine, but if I just
want to generate two very similar GitHub Actions files, it's overkill. I can
split out the shared part and have 3 files + a shell script to write. Or I can
have one file + shell script that runs `rcl query`, but I duplicate the keys
between the shell script and the RCL file ...

What if we could have a `build.rcl`, with this schema?

    <top-level>: List[Target]

    type Target = {
      path: String,
      format: String,
      banner:? String,
      value: Any,
    }

And a subcommand `rcl build` that would evaluate such a file, and for every
target, write the value to the given path in the given format. Then for simple
GitHub Actions, I would have only a *single* file, `build.rcl`, and I could
define my targets in there!

Or maybe the schema should be

    <top-level>: Dict[String, Target]
    type Target = {
        format: String,
        banner:? String,
        value: Any,
    }

because then by construction you don't try to define the same file twice.

Although, if we do a list, then we might also define includes, so you could have
small `build.rcl` files scattered throughout a repo. Something to think about.
