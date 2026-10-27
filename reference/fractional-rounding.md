# Rounding to the nearest fraction

These functions round to the nearest fraction of a denominator rather
than to a decimal place. They follow `janitor::round_to_fraction()` but
take all of
[`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md)'s
options:

- `reround_to_fraction()` rounds to fractions of a whole number, then
  optionally rounds the result to `digits` decimal places.

- `reround_to_fraction_level()` rounds to the nearest fraction at the
  decimal level given by `digits`, with no further rounding.

## Usage

``` r
reround_to_fraction(
  x = NULL,
  denominator = 1,
  digits = Inf,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)

reround_to_fraction_level(
  x = NULL,
  denominator = 1,
  digits = 0L,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
```

## Arguments

- x:

  Numeric. Values to round.

- denominator:

  Numeric (at least 1). `x` is rounded to the nearest fraction of
  `denominator`. Default is `1`.

- digits:

  Integer. In `reround_to_fraction()`, the number of decimal places to
  round the result to. `"auto"` means `ceiling(log10(denominator)) + 1`,
  as in janitor, and the default `Inf` means no further rounding. In
  `reround_to_fraction_level()`, the decimal level at which to round to
  a fraction; default is `0`.

- rounding, threshold, symmetric:

  Passed on to
  [`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md).

## Value

Numeric, as long as `x`, except for the compound rounding methods, which
return two values per element of `x` as in
[`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md).

## See also

[`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md)
and `janitor::round_to_fraction()`.

## Examples

``` r
# With `denominator = 1`, this is ordinary rounding...
reround_to_fraction(0.4, denominator = 1, rounding = "even")
#> [1] 0

# ...with `denominator = 2`, it rounds to the nearest half:
reround_to_fraction(0.4, denominator = 2, rounding = "even")
#> [1] 0.5
reround_to_fraction(0.25, denominator = 3, rounding = "even")
#> [1] 0.3333333

# The default `rounding` returns both directions, as in `reround()`:
reround_to_fraction(0.25, denominator = 2)
#> [1] 0.5 0.0

# `reround_to_fraction_level()` rounds to a fraction at a decimal level:
reround_to_fraction_level(0.12345, denominator = 2, digits = 0)
#> [1] 0 0
reround_to_fraction_level(0.12345, denominator = 2, digits = 1)
#> [1] 0.1 0.1
reround_to_fraction_level(0.12345, denominator = 2, digits = 2)
#> [1] 0.125 0.125
```
