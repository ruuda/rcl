# Tutorial

The main purpose of <abbr>RCL</abbr> is to reduce boilerplate in configuration.
In this tutorial we will explore that use case through an example: defining
cloud storage buckets for backups.

## Setting

In this tutorial we have two databases that need to be backed up to cloud
storage: Alpha and Bravo. For both of them, we want to define three buckets: one
for hourly, one for daily, and one for monthly backups. Each of them should have
a lifecycle policy that deletes objects after 4, 30, and 365 days respectively.

Furthermore, let’s say we have a script or tool that can set up the buckets from
a <abbr>JSON</abbr> configuration file. That tool might be [Terraform][terraform]
in practice, but in this tutorial we’ll assume a simpler schema to avoid
distractions.

[terraform]: https://www.terraform.io/

The configuration file that defines our buckets might look like this:

```yaml
{
  "buckets": [
    {
      "name": "alpha-hourly",
      "region": "eu-west",
      "lifecycle_policy": {
        "delete_after_seconds": 345600
      }
    },
    {
      "name": "alpha-daily",
      "region": "eu-west",
      "lifecycle_policy": {
        "delete_after_seconds": 2592000
      }
    },
    {
      "name": "alpha-monthly",
      "region": "eu-west",
      "lifecycle_policy": {
        "delete_after_seconds": 31536000
      }
    },
    {
      "name": "bravo-hourly",
      "region": "eu-west",
      "lifecycle_policy": {
        "delete_after_seconds": 34560
      }
    },
    {
      "name": "bravo-daily",
      "region": "us-west",
      "lifecycle_policy": {
        "delete_after_seconds": 2592000
      }
    },
    {
      "name": "bravo-monthly",
      "region": "eu-west",
      "lifecycle_policy": {
        "delete_after_seconds": 31536000
      }
    }
  ]
}
```

A configuration file like this is suboptimal in several ways. It is repetitive,
difficult to read, and error-prone to edit. In fact, the above example contains
two bugs that may not be obvious:

 * The `bravo-daily` bucket is located in `us-west` rather than `eu-west` like
   the other buckets.
 * The `delete_after_seconds` of `bravo-hourly` is missing a zero and keeps
   objects for only 10 hours, instead of the intended 4 days.

Switching to a different format such as <abbr>YAML</abbr> or <abbr>TOML</abbr>
may eliminate some of the line noise, but it does not make the file less
repetitive, and therefore not less error-prone to edit. We're going to improve
this by rewriting the configuration in <abbr>RCL</abbr>.

## Installation

Before we can start, follow the [installation instructions](installation.md),
and if you like, [set up syntax highlighting](syntax_highlighting.md) for your
editor. Save the file above as `buckets.json`. Because <abbr>RCL</abbr> is a
superset of <abbr>JSON</abbr>, we can evaluate this file with <abbr>RCL</abbr>,
and it should evaluate to itself.

    rcl evaluate --output=json buckets.json

This prints the document to stdout, formatted and colorized.

## Record syntax

The <abbr>JSON</abbr> format is great for data interchange, but when everything
is quoted, the lack of visual distinction can make the document hard to read.
In <abbr>RCL</abbr>, we can use [_record syntax_](syntax.md#dictionaries) to
omit the quotes on the keys. In addition to writing `"key": value`, we can write
`key = value` when the key is a valid [identifier](syntax.md#identifiers). Two
other additions that <abbr>RCL</abbr> makes to <abbr>JSON</abbr> are allowing
trailing commas, and underscores in numbers. With those changes, our
configuration looks like this:

```rcl
{
  buckets = [
    {
      name = "alpha-hourly",
      region = "eu-west",
      lifecycle_policy = {
        delete_after_seconds = 345_600,
      },
    },
    {
      name = "alpha-daily",
      region = "eu-west",
      lifecycle_policy = {
        delete_after_seconds = 2_592_000,
      },
    },
    {
      name = "alpha-monthly",
      region = "eu-west",
      lifecycle_policy = {
        delete_after_seconds = 31_536_000,
      },
    },
    {
      name = "bravo-hourly",
      region = "eu-west",
      lifecycle_policy = {
        delete_after_seconds = 34_560,
      },
    },
    {
      name = "bravo-daily",
      region = "us-west",
      lifecycle_policy = {
        delete_after_seconds = 2_592_000,
      },
    },
    {
      name = "bravo-monthly",
      region = "eu-west",
      lifecycle_policy = {
        delete_after_seconds = 31_536_000,
      },
    },
  ],
}
```

Evaluating the document should produce the same <abbr>JSON</abbr> output as before:

    rcl evaluate --output=json buckets.rcl

We can also output in <abbr>RCL</abbr> syntax with `--output=rcl`. This is the
default, so when we are inspecting the configuration, and not feeding it into a
tool that expects <abbr>JSON</abbr> or <abbr>YAML</abbr>, we can just run:

    rcl evaluate buckets.rcl

## Variables

Next, let’s try to extract some duplicated values. Our document is an expression,
and in expressions, we can use [let bindings](syntax.md#let-bindings) to bind
values to names. This allows us to reuse them. We can extract the region, and
ensure it’s the same everywhere:

```rcl
let region = "eu-west";
{
  buckets = [
    // Other buckets and some fields omitted for brevity.
    { name = "alpha-hourly", region = region },
    { name = "alpha-daily", region = region },
  ],
}
```

A let-binding is itself an expression of the form `let name = value; expr`,
where in the body `expr`, the variable `name` refers to the bound value. We can
use a let-binding in any place where an expression is allowed. Here we put it at
the top level, but we could put it before the bucket list for example:

```rcl
{
  buckets = let region = "eu-west"; [
    { name = "alpha-hourly", region = region },
    { name = "alpha-daily", region = region },
  ],
}
```

Collections can also contain let bindings. In that case the variable is
available to the element that follows.

```rcl
{
  let region = "eu-west";
  buckets = [
    { name = "alpha-hourly", region = region },
    { name = "alpha-daily", region = region },
  ],
}
```

## Arithmetic

Now that we fixed the region bug, let’s try to eliminate the lifecycle bug.
A number such as 31,536,000 seconds is not easily recognizable by humans, but
most people will recognize 3600 as the number of seconds in an hour, and 24 as
the number of hours in a day. We might write:

```rcl
{
  let region = "eu-west";
  let seconds_per_day = 3600 * 24;
  buckets = [
    // Again, some buckets omitted for brevity.
    {
      name = "alpha-hourly",
      region = region,
      lifecycle_policy = { delete_after_seconds = 4 * seconds_per_day },
    },
    {
      name = "alpha-daily",
      region = region,
      lifecycle_policy = { delete_after_seconds = 30 * seconds_per_day },
    },
  ],
}
```

## List comprehensions

We managed to extract some duplicated values into variables, but the fact
remains that our document consists of almost the same value repeated six times.
We can improve that with a [_list comprehension_](syntax.md#comprehensions) and
[_string interpolation_](strings.md#interpolation):

```rcl
{
  let region = "eu-west";
  let seconds_per_day = 3600 * 24;
  let retention_days = {
    hourly = 4,
    daily = 30,
    monthly = 365,
  };
  buckets = [
    for period, days in retention_days: {
      name = f"alpha-{period}",
      region = region,
      lifecycle_policy = { delete_after_seconds = days * seconds_per_day },
    },
    for period, days in retention_days: {
      name = f"bravo-{period}",
      region = region,
      lifecycle_policy = { delete_after_seconds = days * seconds_per_day },
    },
  ],
}
```

A collection can contain multiple separate loops. In the above example,
`buckets` contains two loops, one for the Alpha database and one for Bravo.
We can deduplicate this further with a nested loop. If we do that, our variables
become single-use, so we can inline them again:

```rcl
{
  buckets = [
    let retention_days = {
      hourly = 4,
      daily = 30,
      monthly = 365,
    };
    for database in ["alpha", "bravo"]:
    for period, days in retention_days: {
      name = f"{database}-{period}",
      region = "eu-west",
      lifecycle_policy = { delete_after_seconds = days * 24 * 3600 },
    }
  ],
}
```

## Conclusion

In this tutorial we replaced an error-prone repetitive <abbr>JSON</abbr>
configuration file with an <abbr>RCL</abbr> file that avoids duplicating values
by using loops, so the configuration is distilled down to its essence. This is a
good introduction to <abbr>RCL</abbr> and highlights one of its use cases, but
we haven’t explored the full language yet. While <abbr>RCL</abbr> is a simple
language with comparatively few features, there are a few constructs we haven’t
touched upon. In particular, assertions, imports, and functions. To learn more,
continue on to [the language guide](syntax.md).
