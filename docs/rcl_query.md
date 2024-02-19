# rcl query

    rcl query [-f | --format <format>] [--] [<file>] <expr> [--output <outfile>]

Shorthands:

    rcl q   (uses default --format=rcl)
    rcl jq  (sets --format=json)

## Description

Evaluate an expression against an input.

 * Read an <abbr>RCL</abbr> expression from `<file>`. When `<file>` is `-`,
   read from stdin. When no file is specified, the input defaults to stdin.
 * Evaluate the expression `<expr>`, in a context where the variable `input`
   is bound to the result of the input document.

This can be used to peek into a sub-element of a document that evaluates to a
large expression, but it can also be used for ad-hoc data querying, as an
alternative to [`jq`](https://jqlang.github.io/jq/). For example:

    echo '[12, 42, 33]' | rcl q '[for x in input: f"Double {x} is {x * 2}."]'
    ["Double 12 is 24.","Double 42 is 84.","Double 33 is 66."]

## Options

`rcl query` accepts the same options
as [`rcl evaluate`](rcl_evaluate.md#options).
