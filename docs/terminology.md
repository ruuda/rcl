# Terminology

Many concepts are universal across formats and languages, but are known by
various names nonetheless. RCL adopts the following terminology.

### Record
A record is a collection of key-value pairs where the keys are
unique. A record is called _object_ in <abbr>JSON</abbr>, _dict_ in Python,
_attrset_ in Nix, _table_ in Lua, and _associative array_ or _map_ in many
other languages. RCL’s records are closely related to the idea of a _struct_
or _product type_ in statically typed languages.

### List
A list is an ordered collection of values, not necessarily unique.
A list is called _array_ in <abbr>JSON</abbr>.

### Set
A set is a collection of values where each value occurs at most once.

### Comprehension
TODO: What do I call them? Comprehensions? Loops? Sequences?
