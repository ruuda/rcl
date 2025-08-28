The RCL configuration language is a domain-specific language that extends
json into a simple, gradually typed, functional language that resembles Python
and Nix. It reduces configuration boilerplate by enabling abstraction and reuse.

* [Website and online playground][rcl]
* [General documentation](https://docs.ruuda.nl/rcl/)

This package contains Python bindings to RCL. Basic usage is similar to the
`json` module, import `rcl` to get started:

```python
import rcl
assert rcl.loads("20 + 22") == 42
```

See the [Python module reference](https://docs.ruuda.nl/rcl/python_bindings/)
for the full module documentation.

[rcl]: https://rcl-lang.org
