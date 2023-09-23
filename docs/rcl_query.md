# rcl query

    rcl query <input> <expr>

Shorthands:

    rcl q

## Description

Evaluate an expression against an input.

 * Read an RCL expression from the file `<input>`. When `<input>` is `-`,
   read from stdin instead.
 * Evaluate the expression `<expr>`, in a context where the variable `input`
   is bound to the result of the input document.

This can be used to peek into a sub-element of a document that evaluates to a
large expression, but it can also be used for ad-hoc data querying, as an
alternative to jq. For example:

    echo '[12, 42, 33]' | rcl q - '[for x in input: f"Double {x} is {x * 2}."]'
    ["Double 12 is 24.","Double 42 is 84.","Double 33 is 66."]
