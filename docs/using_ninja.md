# Using Ninja

RCL can abstract away repetition, for example in GitHub Actions workflows and
Kubernetes manifests. It also enables sharing configuration between systems
that do not natively share data. For example, you can have one file to define
users and groups, and import it into both your Tailscale configuration and into
your Hashicorp Vault configuration. However, none of these tools natively read
<abbr>RCL</abbr>. You still need to run `rcl` to generate the required `.yml`,
`.tf.json`, `.json`, and `.toml` files that can be consumed by your existing
tools.

If you have just a few files, it’s not so bad to run `rcl` yourself:

    rcl evaluate --format=json policies.rcl --output=policies.json

In a larger repository with many generated files, this gets tedious. It’s not
very discoverable either. Ideally we would have one command to update all
generated files.

## Scripting

We could write a shell script or Python script, and put all those `rcl
evaluate` calls in one place. This is a big step forward, and it’s probably good
enough. It will rebuild all files every time, but unless your configuration is
truly massive, <abbr>RCL</abbr> is probably fast enough that it doesn’t matter.

Still, it’s a bit of a shame to unnecessarily rebuild files.

Also, what if _which files_ we want to generate itself depends on configuation?
We could write the build script in Python and use [the Python module](python_bindings.md),
but this starts to become a home-grown build system, so maybe we should reach
for a proper one instead.

## Make

Updating generated files when inputs change is the role of a build tool.
We could use [Make][gnumake] and write a makefile:

```make
policies.json: policies.rcl
    rcl evaluate --format=json --output=$@ $<
```

Aside from the somewhat arcane syntax, this makefile has one big problem. If
`policies.rcl` imports an <abbr>RCL</abbr> file, say `users.rcl`, then
Make will not rebuild `policies.json` when we change `users.rcl`, because
we haven’t specified the dependency in the makefile. Manually listing all
transitive dependencies is tedious and prone to go out of date.

[Ninja][ninja-build] is a different build tool that can solve this problem by
reading transitive dependencies from a [depfile][depfile], and [<abbr>RCL</abbr>
can write such a depfile][odepfile]. In the remainder of this chapter, we’ll
explore using Ninja as the build tool.

[gnumake]:     https://www.gnu.org/software/make/manual/html_node/index.html
[ninja-build]: https://ninja-build.org/
[depfile]:     https://ninja-build.org/manual.html#_depfile
[odepfile]:    rcl_evaluate.md#-output-depfile-depfile

## Ninja

[Ninja][ninja-build] is a fast and flexible build tool, but its build files are
low-level and intended to be _generated_, not written by hand. Let’s write one
by hand anyway, to better understand what we are working with.

In a Ninja file, we first define [a rule][ninja-rule] that specifies how
to invoke a program. This is also where we can tell Ninja to use a
[depfile][depfile].

```ninja
rule rcl
  description = Generating $out
  command = rcl eval --color=ansi --format=$format --output=$out --output-depfile=$out.d $in
  depfile = $out.d
  deps = gcc
```

Here `$in`, `$out`, and `$format` are variables. Ninja itself sets `$in` and
`$out`, and `$format` is one that we define because it varies per target. The
`deps = gcc` line is not required, but it makes Ninja store the depedency
information in `.ninja_deps` and then delete the generated depfile, instead of
reading it on demand. This is nice to keep the repository clean.

Next, we add a [build statement][ninja-stmt] that specifies how to build a file:

```ninja
build policies.json: rcl policies.rcl
  format = json
```

This is enough for Ninja to work. Save the file to `build.ninja` and then build
`policies.json`:

```console
$ ninja
[1/1] Generating policies.json

$ ninja
ninja: no work to do.

$ touch users.rcl
$ ninja
[1/1] Generating policies.json
```

[ninja-rule]: https://ninja-build.org/manual.html#_rules
[ninja-stmt]: https://ninja-build.org/manual.html#_build_statements

## Generating Ninja files

Okay, so we can write a Ninja file by hand, it’s not even that bad. But at some
point, we’re going to end up with lots of similar build statements, and wish we
had a way to abstract that. If only we had a tool that could abstract away this
repetition …

We can write a `build.rcl` that evaluates to a Ninja build file like so:

```rcl
#!/usr/bin/env -S rcl evaluate --output=build.ninja --format=raw

let ninja_prelude =
  """
  rule rcl
    description = Generating $out
    command = rcl eval --color=ansi --format=$format --output=$out --output-depfile=$out.d $in
    depfile = $out.d
    deps = gcc
  """;

let build_json = basename =>
  f"""
  build {basename}.json: rcl {basename}.rcl
    format = json
  """;

// File basenames that we want to generate build rules for.
// This is the part we need to edit when we add more files.
let basenames_json = ["policies"];

let sections = [
  ninja_prelude,
  for basename in basenames_json: build_json(basename),
];

sections.join("\n")
```

Now we can generate the same build file that we previously wrote by hand, and
when we add more json target files, we only need to add one string to the list.
By adding a `#!`-line and making the file executable, we can even record how the
Ninja file is generated. Unfortunately, even with the `#!`-line we are back to
multiple build steps: first `./build.rcl`, and then `ninja`. Can we do better?

For bootstrapping `build.ninja`, that will always need a manual step. But after
we run `./build.rcl` once, Ninja can keep `build.ninja` up to date for us. We
just need to list it as a build target:

```rcl
let sections = [
  ninja_prelude,
  """
  build build.ninja: rcl build.rcl
    format = raw
  """,
  for basename in basenames_json: build_json(basename),
];
```

## Dynamic targets

Now that we generate our `build.nina` from `build.rcl`, we can import
<abbr>RCL</abbr> documents to dynamically create build tagets. For instance,
we can leverage [`rcl query`](rcl_query.md) to build all the keys of a document
`manifests.rcl` as separate files. We could do that as follows:

```rcl
#!/usr/bin/env -S rcl evaluate --format=raw --output=build.ninja

let command = [
  "rcl",
  "query",
  "--color=ansi",
  "--format=$format",
  "--output=$out",
  "--output-depfile=$out.d",
  "$in",
  "$query",
];
let ninja_prelude =
  f"""
  rule rcl
    description = Generating $out
    command = {command.join(" ")}
    depfile = $out.d
    deps = gcc
  """;

let build_raw = (target, src) =>
  f"""
  build {target}: rcl {src}
    query = input
    format = raw
  """;

let build_json_query = (target, src, query) =>
  f"""
  build {target}: rcl {src}
    format = json
    query = {query}
  """;

let manifests = import "manifests.rcl";

let sections = [
  ninja_prelude,
  build_raw("build.ninja", "build.rcl"),
  for key, _ in manifests:
  // Warning, this assumes that the key is both a valid filename
  // and RCL expression. Currently no built-in functions exist for
  // validating this.
  build_json_query(f"{key}.yml", "manifests.rcl", f"input.{key}"),
];

sections.join("\n")
```

**Warning:** Generating targets dynamically is powerful, but also a sure way
to make your build process intractable quickly! Use sparingly and with good
judgement!

## Conclusion

RCL enables sharing configuration between systems that do not natively share
data. To do so, you will likely need to generate files. Keeping those files up
to date is the job of a build tool. In this chapter we have seen how to use the
Ninja build tool, and how to use <abbr>RCL</abbr> to write Ninja build files.
