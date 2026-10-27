# Reconstruct rounding bounds

`unround()` takes a rounded number and returns the range of values that
would have been rounded to it: a lower and an upper bound, and whether
each is inclusive. The default rounding method is `"up_or_down"`; see
*Rounding* for the others.

## Usage

``` r
unround(
  x,
  rounding = "up_or_down",
  threshold = 5,
  digits = NULL,
  symmetric = FALSE
)
```

## Arguments

- x:

  String or numeric. The rounded number. Must be a string unless
  `digits` is specified. A missing or infinite `x` has `NA` bounds.

- rounding:

  String. The rounding method assumed to have produced `x`. Default is
  `"up_or_down"`. See *Rounding*.

- threshold:

  Numeric. For the `"*_from"` methods, the point within a step at which
  rounding switches direction, in tenths of a step; greater than `0` and
  less than `10`. Default is `5`. See
  [`round_up_from()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md).

- digits:

  Integer. The number of decimal places in `x`. Default is `NULL`, in
  which case it is counted from the string `x`. Specify it if the number
  is already known, as a function that uses `unround()` as a helper
  would.

- symmetric:

  Logical. If `TRUE`, negative numbers are assumed to have been rounded
  like their absolute values under `"up"`, `"down"`, and the `"*_from"`
  methods. Default is `FALSE`. Must not be `TRUE` with a `"ties_*"`
  method; see
  [`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md).

## Value

A tibble with the columns `range`, `rounding`, `lower`, `incl_lower`,
`x`, `incl_upper`, and `upper`. `range` summarizes the columns from
`lower` to `upper`.

## Details

`x` is a string by default so that its trailing zeros can be counted,
since a numeric value loses them. If the number of decimal places is
known, pass it as `digits`, and `x` may be numeric.
[`bound_numerators()`](https://lhdjung.github.io/roundwork/reference/bound_numerators.md)
and
[`sum_range()`](https://lhdjung.github.io/roundwork/reference/sum_range.md)
derive their bounds from the same internal functions as `unround()`, so
they always agree with it.

All arguments are vectorized. Those of length greater than 1 must have
the same length and are paired element by element. Pairing `x` with
`rounding` in this way gives a warning.

## Rounding

Whether a bound is inclusive depends on the rounding method. Under
`"up"`, a value on the lower bound rounds up to `x`, but a value on the
upper bound rounds up past it. The `range` column shows the bounds with
the appropriate signs:

|                                         |                               |
|-----------------------------------------|-------------------------------|
| **Value of `rounding`**                 | **Corresponding range**       |
| `"up_or_down"` (default)                | `lower <= x <= upper`         |
| `"up"`, `"ties_up"`                     | `lower <= x < upper`          |
| `"down"`, `"ties_down"`                 | `lower < x <= upper`          |
| `"ties_away"` (positive `x`)            | `lower <= x < upper`          |
| `"ties_away"` (negative `x`)            | `lower < x <= upper`          |
| `"ties_zero"` (positive `x`)            | `lower < x <= upper`          |
| `"ties_zero"` (negative `x`)            | `lower <= x < upper`          |
| `"ties_away"`, `"ties_zero"` (zero `x`) | `lower < x < upper`           |
| `"even"`, `"ties_even"`                 | `lower <= x <= upper`         |
| `"ceiling"`                             | `lower < x = upper`           |
| `"floor"`                               | `lower = x < upper`           |
| `"ceiling_or_floor"`                    | `lower < x < upper`           |
| `"trunc"` (positive `x`)                | `lower = x < upper`           |
| `"trunc"` (negative `x`)                | `lower < x = upper`           |
| `"trunc"` (zero `x`)                    | `lower < x < upper`           |
| `"anti_trunc"` (positive `x`)           | `lower < x = upper`           |
| `"anti_trunc"` (negative `x`)           | `lower = x < upper`           |
| `"anti_trunc"` (zero `x`)               | `lower = x = upper` (all `0`) |
| `"up_from"`                             | `lower <= x < upper`          |
| `"down_from"`                           | `lower < x <= upper`          |
| `"up_from_or_down_from"`                | `lower <= x <= upper`         |

`threshold` only applies to the three `"*_from"` methods. It moves their
range without widening it; each spans exactly one step at every
threshold.

The `"ties_*"` methods each name a complete tie-breaking procedure, so
giving `symmetric = TRUE` with them is an error; see
[`round_ties_up()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md).

Under `"even"`, both bounds are reported as inclusive.
[`base::round()`](https://rdrr.io/r/base/Round.html) decides a tie on
the binary double, so whether a value on a bound rounds to `x` cannot be
predicted from its decimal digits. Treating both bounds as inclusive can
only widen the range, which is the safe direction for error detection.

## See also

[`round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
for rounding up or down from 5, and
[`round_ceiling()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
for the one-directional methods.

## Examples

``` r
# By default, `x` may have been rounded up or down:
unround(x = "2.7")
#> # A tibble: 1 × 7
#>   range                  rounding   lower incl_lower x     incl_upper upper
#>   <chr>                  <chr>      <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 2.65 <= x(2.7) <= 2.75 up_or_down  2.65 TRUE       2.7   TRUE        2.75

# If it was rounded up, the upper bound is exclusive:
unround(x = "2.7", rounding = "up")
#> # A tibble: 1 × 7
#>   range                 rounding lower incl_lower x     incl_upper upper
#>   <chr>                 <chr>    <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 2.65 <= x(2.7) < 2.75 up        2.65 TRUE       2.7   FALSE       2.75

# Likewise with rounding down...
unround(x = "2.7", rounding = "down")
#> # A tibble: 1 × 7
#>   range                 rounding lower incl_lower x     incl_upper upper
#>   <chr>                 <chr>    <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 2.65 < x(2.7) <= 2.75 down      2.65 FALSE      2.7   TRUE        2.75

# ...and with `base::round()`:
unround(x = "2.7", rounding = "even")
#> # A tibble: 1 × 7
#>   range                  rounding lower incl_lower x     incl_upper upper
#>   <chr>                  <chr>    <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 2.65 <= x(2.7) <= 2.75 even      2.65 TRUE       2.7   TRUE        2.75

# One row per input value:
unround(x = c(3.6, "5.20", 5.174))
#> # A tibble: 3 × 7
#>   range                        rounding  lower incl_lower x     incl_upper upper
#>   <chr>                        <chr>     <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 3.55 <= x(3.6) <= 3.65       up_or_do…  3.55 TRUE       3.6   TRUE        3.65
#> 2 5.195 <= x(5.20) <= 5.205    up_or_do…  5.20 TRUE       5.20  TRUE        5.20
#> 3 5.1735 <= x(5.174) <= 5.1745 up_or_do…  5.17 TRUE       5.174 TRUE        5.17
```
