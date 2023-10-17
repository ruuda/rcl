# Imports

Documents can import other <abbr>RCL</abbr> documents — including
<abbr>JSON</abbr> documents, which are valid <abbr>RCL</abbr> documents.
An `import` expression evaluates to the contents of the imported document:

```rcl
let inventory = import "inventory.rcl";
[for server in inventory: server.name]
```

## Scope

Every document is independent, and gets its own clean environment for
evaluation. The following example is an error:

```rcl
// a.rcl:
let x = 42;
import "b.rcl"

// b.rcl:
x
```

Evaluating `a.rcl` would fail, because in the evaluation of `b.rcl`, the
variable `x` is undefined, even though in the context of the `import "b.rcl"`
expression, `x` _is_ defined.

## Import argument

The `import` keyword must be followed by a regular string literal. Format
strings or arbitrary expressions are not allowed.

```rcl
// Not allowed, import path must be a string literal.
let documents = [for i in [1, 2, 3]: import f"doc{i}.json"];
```

Restricting imports to string literals ensures that:

 * Imports of a particular file can be found using grep, there is no dynamic
   construction of import paths.
 * To construct an import graph, it is sufficient to parse a file, no evaluation
   is necessary.

## Import location

The import location can be _relative_ or _workdir-relative_.

 * A _relative_ path is relative to the file that contains the `import`
   expression. For example, if `/etc/a.rcl` contains `import "b.rcl"`, then this
   would import `/etc/b.rcl`.
 * A _workdir-relative_ path starts with `//` and is relative to the working
   directory that `rcl` executes from. For example, if `rcl` is being executed
   in `/home/user/exprs`, then `import "//a.rcl"` would import
   `/home/user/exprs/a.rcl`.
 * An _absolute_ path that starts with a single `/` is not allowed.

## Security

RCL is a pure language without side effects. In particular, <abbr>RCL</abbr>
documents have no ability to write to files, access the network, access clocks,
or make syscalls. This makes evaluating untrusted <abbr>RCL</abbr> documents
safe: evaluation does not modify your system.

However, due to imports, <abbr>RCL</abbr> documents _do_ have the ability to
read arbitrary files from your filesystem, and include those in the evaluation
result. In some contexts this is undesirable, which is why <abbr>RCL</abbr> has
three [sandboxing modes](rcl_evaluate.md#-sandbox-mode) to place additonal
restrictions on what files can be imported.
