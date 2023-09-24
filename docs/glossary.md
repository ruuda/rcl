# Glossary

Many concepts are universal across formats and languages, but are known by
various names nonetheless. RCL adopts the following terminology.

### Dictionary
A [dictionary](syntax.md#Dictionaries), or _dict_ for short, is a collection of
key-value pairs where the keys are unique. A dict is called _object_ in json,
_dict_ in Python, _attrset_ in Nix, _table_ in Lua, and _associative array_ or
_map_ in many other languages.

In most places, using a dict in RCL looks the same as using a type with
statically defined fields in other languages. In this sense, RCL’s dicts also
act as the analog of a _struct_ or _record_ in statically typed languages.

### List
A [list](syntax.md#lists) is an ordered collection of values, not necessarily
unique. A list is called _array_ in json.

### Set
A [set](syntax.md#sets) is a collection of values where each value occurs at
most once.

### Comprehension
A list, dict, or set [comprehension](syntax.md#comprehensions) is a way to
construct a collection by iterating an existing collection. It is related to the
idea of a _for loop_ in imperative languages.
