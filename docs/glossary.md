# Glossary

Many concepts are universal across formats and languages, but are known by
various names nonetheless. RCL adopts the following terminology.

### Comprehension
A list, dict, or set [comprehension](syntax.md#comprehensions) is a way to
construct a collection by iterating an existing collection. It is related to the
idea of a _for loop_ in imperative languages.

### Dictionary
A [dictionary](syntax.md#Dictionaries), or _dict_ for short, is a collection of
key-value pairs where the keys are unique. A dict is called _object_ in json,
_dict_ in Python, _attrset_ in Nix, _table_ in Lua, and _associative array_ or
_map_ in many other languages.

In most places, using a dict in <abbr>RCL</abbr> looks the same as using a type
with statically defined fields in other languages. In this sense, dicts also
act as the analog of a _struct_ or _record_ in statically typed languages.

### Function
[Functions](functions.md) in <abbr>RCL</abbr> are _anonymous_: function
values have no inherent name, but like any other value they can be bound to a
name with a let binding. Functions are _closures_: they can capture values
defined outside of the function body. Such functions are sometimes called
_lambda functions_ or _lambdas_ in other languages.

### If-else expression

Like everything in <abbr>RCL</abbr>, if-else is an _expression_, not control
flow for statements. Some languages have a _ternary operator_ that plays the
same role. For example, `cond ? x : y` in C, or `x if cond else y` in Python,
would be written `if cond: x else y` in <abbr>RCL</abbr>.

### List
A [list](syntax.md#lists) is an ordered collection of values, not necessarily
unique. A list is called _array_ in json.

### Set
A [set](syntax.md#sets) is a collection of values where each value occurs at
most once.
