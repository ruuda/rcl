# Number

`Number` is the type of all numbers in <abbr>RCL</abbr>.
This page documents the supported methods and operators.
For an overview of the syntax and representation,
[see the _numbers_ chapter in the language guide](numbers.md).

## Addition and subtraction

The `+` and `-` operators add and subtract numbers.

```rcl
// Evaluates to 42.
21 + 21

// Evaluates to 42.0.
45.5 - 3.5
```

The result will have as many decimals as the input with the most decimals.
In particular, when both inputs are integer, the result is also integer.

## Multiplication

The `*` operator multiplies numbers.

```rcl
// Evaluates to 42.
2 * 21

// Evaluates to 42.0.
4 * 10.5
```

The number of decimals in the result is the sum of the number of decimals of
the inputs. For repeated multiplication of numbers with a decimal part, this
can grow quickly. Use `round` to limit the number of decimals in the result.

## Division

The `/` operator divides two numbers.

```rcl
// Evaluates to 42.
84 / 2
```

Currently, division is supported only when the result can be represented exactly.
This limitation should be lifted in a future version.

## round

```rcl
Number.round: (self: Number, n_decimals: Number) -> Number
```

Round the number to `n_decimals` decimal places. The result is not in scientific
notation (it has no exponent), even when the input is. When `n_decimals` is 0,
this means that the result is an integer. This rounds away from 0.

```rcl
// Evaluates to 0.4.
(0.42).round(1)

// Evaluates to [42, -42].
[(41.5).round(0), (-41.5).round(0)]

// Evaluates to 4200.
(42e2).round(0)
```
