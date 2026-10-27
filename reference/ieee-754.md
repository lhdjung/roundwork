# IEEE 754 rounding standard

These functions implement the industry standard IEEE 754:

- `round_ties_to_even()` rounds to the nearest value. If both are at
  equal distance, it tends to round to the even number (see
  [`base::round()`](https://rdrr.io/r/base/Round.html) for details).

- `round_ties_to_away()` rounds to the nearest value. If both are at
  equal distance, it rounds to the number with the greater absolute
  value, i.e., the number that is further away from zero.

- `round_toward_positive()` always rounds to the greater of the two
  nearest values. This is like ceiling at a given number of decimal
  places.

- `round_toward_negative()` always rounds to the lesser of the two
  nearest values. This is like flooring at a given number of decimal
  places.

- `round_toward_zero()` always rounds to the number with the lower
  absolute value, i.e., the number that is closer to zero. This is like
  truncation at a given number of decimal places.

## Usage

``` r
round_ties_to_even(x, digits = 0, ...)

round_ties_to_away(x, digits = 0)

round_toward_positive(x, digits = 0)

round_toward_negative(x, digits = 0)

round_toward_zero(x, digits = 0)
```

## Arguments

- x:

  Numeric. The number to round.

- digits:

  Integer. Number of decimal places to round to. Default is `0`.
  Negative values round to powers of ten: `round_up(1250, digits = -2)`
  is `1300`.

- ...:

  Only in `round_ties_to_even()`. Passed down to
  [`base::round()`](https://rdrr.io/r/base/Round.html).

## Value

Numeric. `x` rounded to `digits`.

## Details

The function names follow the official standard except for case
conventions (IEEE 2019, pp. 27f.; the [Wikipedia
page](https://en.wikipedia.org/wiki/IEEE_754#Rounding_rules) is more
accessible but uses slightly different names).

Internally, these functions are just wrappers around other roundwork
functions as well as
[`base::round()`](https://rdrr.io/r/base/Round.html). They are presented
here for easy compliance with the IEEE 754 standard in R. Each of them
has an equivalent elsewhere in the package, and the results are
identical:

|  |  |
|----|----|
| **IEEE 754 name** | **Equivalent** |
| `round_ties_to_even()` | [`base::round()`](https://rdrr.io/r/base/Round.html) |
| `round_ties_to_away()` | [`round_ties_away()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md) |
| `round_toward_positive()` | [`round_ceiling()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md) |
| `round_toward_negative()` | [`round_floor()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md) |
| `round_toward_zero()` | [`round_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md) |

Note that IEEE 754 has no name for the other three tie-breaking
procedures in
[`round_ties_up()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md)
and friends, nor for rounding away from zero
([`round_anti_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)),
nor for the custom thresholds of
[`round_up_from()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md).

## References

IEEE (2019). *IEEE Standard for Floating-Point Arithmetic.*
https://doi.org/10.1109/IEEESTD.2019.8766229

## See also

[`round_ties_up()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md)
and friends for the same tie-breaking procedures under names that do not
presuppose the standard, and
[`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md)
for reaching any of them through a `rounding` string.

## Examples

``` r
# Round to the nearest value. In case of a tie,
# the result is hard to predict but tends to be even:
round_ties_to_even(1.25, digits = 1)
#> [1] 1.2
round_ties_to_even(-1.25, digits = 1)
#> [1] -1.2

# Round to the nearest value. In case of a tie,
# round away from zero:
round_ties_to_away(1.25, digits = 1)
#> [1] 1.3
round_ties_to_away(-1.25, digits = 1)
#> [1] -1.3

# Always round to the greater value:
round_toward_positive(0.721, digits = 2)
#> [1] 0.73
round_toward_positive(-0.721, digits = 2)
#> [1] -0.72

# Always round to the lesser value:
round_toward_negative(3.249, digits = 2)
#> [1] 3.24
round_toward_negative(-3.249, digits = 2)
#> [1] -3.25

# Always round toward zero:
round_toward_zero(6.38, digits = 1)
#> [1] 6.3
round_toward_zero(-6.38, digits = 1)
#> [1] -6.3
```
