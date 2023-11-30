# Functions

Aside from built-in methods and functions, <abbr>RCL</abbr> supports
user-defined functions. Functions are _anonymous_, in the sense that the name
you use to call them is not an inherent property of the function, but of the
variable that it is assigned to. Functions are _closures_, in the sense that the
function body can reference variables defined outside of the body. Such
functions are sometimes called _lambda functions_ or _lambdas_ in other
languages.

## Defining functions

A `=>` arrow creates a function, with the arguments on the left and the body
on the right.

```rcl
let add = (x, y) => x + y;
// Evaluates to 42.
add(22, 20)
```

A trailing comma in the argument list is optional.

```rcl
let sub = (
  x,
  y,
) => x - y;
```

When a function takes a single argument, the parentheses may be omitted.

```rcl
let double_input = x => x * 2;
```

## Closures

A function can capture variables defined outside the function body. The names
remain bound to the values they had when the function was defined. If a later
let-binding shadows an earlier one, this does not affect the function.

```rcl
let x = 42;
let get_x = () => x;
let x = 0;
// Evaluates to 42, not to 0, because x was bound to 42 when get_x was defined.
get_x()
```

Variables in <abbr>RCL</abbr> bind names to immutable values, not to mutable
memory locations. This means that in a loop, functions capture the value that
a variable has in that iteration of the loop.

```rcl
let fs = [for k in [1, 2, 3]: x => x * k];
// Evaluates to [10, 20, 30].
[for f in fs: f(10)]
```

## First-class functions

Functions are values and can be passed to functions.

```rcl
let apply_twice = (f, x) => f(f(x));
// Evaluates to 256 = (4 * 4) * (4 * 4);
apply_twice(x => x * x, 4)
```

TODO: Add `group_by` example once I implement `group_by`.
