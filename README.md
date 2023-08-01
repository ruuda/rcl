# Ruud’s Configuration Language

Why another config language? Because:

 * HCL is too ad-hoc to be suitable for any serious abstraction (`setunion` is
   variadic so it only works with a statically known number of sets; `flatten`
   recursively flattens so it can’t be typed properly and breaks generic code,
   for comprehensions can’t do nested loops, `for_each` syntax is bolted on,
   etc.)

 * Nix-the-language is great but forces the entire Nix store on you when all I
   want is to evaluate expressions.

 * Python is great but requires some boilerplate for doing the IO if you want
   to use it as a configuration language.

 * Dhall has the right ideas but the syntax and heavy use of Unicode symbols
   make it look ugly.

 * CUE and Nickel were not invented here.
