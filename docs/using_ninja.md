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

    rcl evaluate --format=json policies.tf.rcl --output=policies.tf.json

In a larger repository with many generated files, this gets tedious. It’s not
very discoverable either. Ideally we would have one command to update all
generated files.

## Scripting

We could write a shell script or Python script, and put all those `rcl
evaluate` calls in one place. This is a big step forward, and it’s probably good
enough. It will rebuild all files every time, but unless your configuration is
truly massive, <abbr>RCL</abbr> is probably fast enough that it doesn’t matter.

Still, it’s a bit of a shame to unnecessarily rebuild files. _Can we do better?_

## Make

Updating generated files when inputs change is the role of a build tool.
We could use [Make][gnumake] and write a makefile:

```make
policies.tf.json: policies.tf.rcl
    rcl --format=json --output=$@ $<
```

Aside from the somewhat arcane syntax, this makefile has one big problem. If
`policies.tf.rcl` imports an <abbr>RCL</abbr> file, say `users.rcl`, then
Make will not rebuild `policies.tf.json` when we change `users.rcl`, because
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
  command = rcl eval --format=$format $in --output=$out --output-depfile=$out.d
  depfile = $out.d
  deps = gcc
```

Here `$in`, `$out`, and `$format` are variables. Ninja itself sets `$in` and
`$out`, and `$format` is one that we define because it varies per target. The
`deps = gcc` line is not required, but it makes Ninja store the depedency
information in `.ninja_deps` and then delete the generated depfile, which is
nice to keep the repository clean.

Next, we add a [build statement][ninja-stmt] that specifies how to build a file:

```ninja
build policy.tf.json: rcl policy.tf.rcl
  format = json
```

This is enough for Ninja to work. Save the file to `build.ninja`, and then build
`policy.tf.json`:

```console
$ ninja
[1/1] Generating policy.tf.json

$ ninja
ninja: no work to do.

$ touch users.rcl
$ ninja
[1/1] Generating policy.tf.json
```

TODO: Put `--color=ansi` in there. But then `--output` should ignore it.

[ninja-rule]: https://ninja-build.org/manual.html#_rules
[ninja-stmt]: https://ninja-build.org/manual.html#_build_statements
