# Python bindings

RCL includes a Python native module that can be used to load documents, similar
to Python’s built-in `json` module. Although it is possible to export an
<abbr>RCL</abbr> document to <abbr>JSON</abbr> and load it using `json.load`,
there are a few reasons for using the module:

 * Avoiding an intermediate file or spawning an additional process.
 * Supporting a wider range of types. The module preserves set values and
   dictionaries with non-string keys.

The name of the module is `rcl`. See the [installation instructions][install]
for how to get the Python module.

[install]: installation.md#python-module-from-source

## load_file

    rcl.load_file(path: str) -> Any

Evaluate the <abbr>RCL</abbr> expression in the file at the given file path.

## loads

    rcl.loads(src: str) -> Any

Evaluate the <abbr>RCL</abbr> expression `src`, return the result. This is
analogous to `json.loads`. TODO: Add a way to control the sandbox policy and
tracer.
