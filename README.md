# Ruud’s Configuration Language

**Vaporware warning:** This is a proof-of-concept toy project. I will probably
lose interest in it before it reaches a point where it is usable and useful.

Why another config language? Because:

 * HCL is too ad-hoc to be suitable for any serious abstraction (`setunion` is
   variadic so it only works with a statically known number of sets; `flatten`
   recursively flattens so it can’t be typed properly and breaks generic code,
   for comprehensions can’t do nested loops, `for_each` syntax is bolted on,
   etc.)

 * Nix-the-language is great but forces the entire Nix store on you when all I
   want is to evaluate expressions.

 * Python is great but requires some boilerplate for doing the IO if you want
   to use it as a configuration language. Also the syntactic order of list
   comprehensions prevents autocomplete in editors.

 * Dhall has the right ideas but the syntax and heavy use of Unicode symbols
   make it look ugly.

 * CUE and Nickel were not invented here.

## License

RCL is licensed under the [Apache 2.0][apache2] license. It may be used in
free software as well as closed-source applications, both for commercial and
non-commercial use under the conditions given in the license. If you want to
use RCL in your GPLv2-licensed software, you can add an [exception][except]
to your copyright notice. Please do not open an issue if you disagree with the
choice of license.

[apache2]: https://www.apache.org/licenses/LICENSE-2.0
[except]:  https://www.gnu.org/licenses/gpl-faq.html#GPLIncompatibleLibs
