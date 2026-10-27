# Whole-number sums behind a rounded mean

`sum_range()` returns the least and the greatest whole number `s` for
which `s / n_items`, rounded as specified, would be reported as `x_num`.
Every whole number between the two qualifies as well.

This is the arithmetic behind the GRIM test (Brown and Heathers 2017): a
mean of `n_items` whole numbers is GRIM-consistent if the range is not
empty. The same goes for any other share of a whole, such as a
percentage of a sample.

## Usage

``` r
sum_range(x_num, n_items, digits, rounding, threshold, symmetric = FALSE)
```

## Arguments

- x_num:

  Numeric (length 1). The rounded number. Not checked.

- n_items:

  Numeric (length 1). The number of values averaged; a positive whole
  number. Not checked.

- digits:

  Integer (length 1). Number of decimal places in `x_num`. Not checked.

- rounding:

  String (length 1). Rounding method presumably used to create `x_num`.
  See the `Rounding` section of
  [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md).

- threshold:

  Numeric (length 1). The point within a step at which rounding switches
  direction, in tenths of a step, for the `"up_from"`, `"down_from"`,
  and `"up_from_or_down_from"` methods. See
  [`round_up_from()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md).

- symmetric:

  Logical (length 1). Whether the rounding of negative numbers mirrored
  that of positive numbers. See
  [`round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md).

## Value

A numeric vector of length 2, `c(lower, upper)`. If `lower` is greater
than `upper`, no whole-number sum is consistent with `x_num`. Both are
`NA` if `x_num` is missing or infinite, or if `n_items` is missing,
infinite, or not positive.

## Details

Like
[`bound_numerators()`](https://lhdjung.github.io/roundwork/reference/bound_numerators.md),
on which it builds, `sum_range()` works in exact integer arithmetic.
This is exact as long as `n_items * 10^(digits + 1)` times the integer
part of `x_num` stays below `2^53`, which is far beyond any realistic
input.

`sum_range()` is a low-level primitive, meant to be called once per
value set inside a consistency test, so it does not check its input. The
caller is responsible for passing single numbers, a whole-number
`digits` that fits `x_num`, and a whole-number `n_items`. A fractional
`n_items` returns a range for that fraction rather than an error or
`NA`. Only `rounding`, `threshold`, and `symmetric` are checked, by
[`bound_numerators()`](https://lhdjung.github.io/roundwork/reference/bound_numerators.md).

## References

Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
Technique Detects Numerous Anomalies in the Reporting of Results in
Psychology. *Social Psychological and Personality Science*, 8(4),
363-369.
[doi:10.1177/1948550616673876](https://doi.org/10.1177/1948550616673876)

## Examples

``` r
# A mean of 5.18 from 28 whole numbers requires their sum to be 145:
sum_range(5.18, 28, digits = 2, rounding = "up_or_down", threshold = 5)
#> [1] 145 145

# No sum gives 5.19: 145 / 28 is 5.18, and 146 / 28 is 5.21:
sum_range(5.19, 28, digits = 2, rounding = "up_or_down", threshold = 5)
#> [1] 146 145

# 34.5% of a sample of 58 is 20 people (or 0.345 as a share):
sum_range(0.345, 58, digits = 3, rounding = "up_or_down", threshold = 5)
#> [1] 20 20
```
