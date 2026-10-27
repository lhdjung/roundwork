# Rounding procedures named by their tie-breaking rule

Each of these functions rounds to the nearest number with `digits`
decimal places. They differ only at a tie, a value exactly halfway
between two such numbers:

- `round_ties_up()` sends a tie to the higher number.

- `round_ties_down()` sends a tie to the lower number.

- `round_ties_away()` sends a tie away from zero.

- `round_ties_zero()` sends a tie toward zero.

- `round_ties_even()` sends a tie to the even neighbor.

The first four are
[`round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
and
[`round_down()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
with and without `symmetric = TRUE`, under names that state the
procedure. They take no `symmetric` argument, since the name already
fixes the direction on both sides of zero. `round_ties_even()` is
[`base::round()`](https://rdrr.io/r/base/Round.html).

The matching `rounding` strings, `"ties_up"` and so on, are accepted
wherever `rounding` is, and giving `symmetric = TRUE` alongside one of
them is an error.

## Usage

``` r
round_ties_up(x, digits = 0L)

round_ties_down(x, digits = 0L)

round_ties_away(x, digits = 0L)

round_ties_zero(x, digits = 0L)

round_ties_even(x, digits = 0L)
```

## Arguments

- x:

  Numeric. The number to round.

- digits:

  Integer. Number of decimal places to round to. Default is `0`.

## Value

Numeric. `x` rounded to `digits`.

## Details

`round_ties_away()` is IEEE 754's *roundTiesToAway*, which Excel's
`ROUND()`, SPSS's `RND()`, SAS's `ROUND()`, and Matlab's
[`round()`](https://rdrr.io/r/base/Round.html) implement.
`round_ties_up()` is what Stata's
[`round()`](https://rdrr.io/r/base/Round.html) does. `round_ties_even()`
is *roundTiesToEven*, the standard's default and what R, Python, and
NumPy do. See
[`vignette("rounding-options")`](https://lhdjung.github.io/roundwork/articles/rounding-options.md).

"Up" and "down" mean up and down on the number line, not the
away-from-zero sense that "round up" carries in Excel, Java, and Python.
For rounding that is always away from zero or toward it, see
[`round_anti_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
and
[`round_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md).

Unlike the other four, `round_ties_even()` cannot be predicted from the
printed decimal: [`base::round()`](https://rdrr.io/r/base/Round.html)
decides on the binary double, and most decimal fractions are not exact
ties in binary. Consistency tests treat both bounds under
`rounding = "even"` as inclusive for that reason; see
[`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md).

## See also

[`round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
for the same procedures spelled with `symmetric`, and for the shared
details on thresholds and floating-point tolerance.

## Examples

``` r
# All five agree except at ties:
round_ties_up(x = 1.24, digits = 1)
#> [1] 1.2
round_ties_down(x = 1.24, digits = 1)
#> [1] 1.2

# At a tie, they differ, but only by sign below zero:
round_ties_up(x = c(-2.5, 2.5))
#> [1] -2  3
round_ties_down(x = c(-2.5, 2.5))
#> [1] -3  2
round_ties_away(x = c(-2.5, 2.5))    # Excel, SPSS, SAS, Matlab
#> [1] -3  3
round_ties_zero(x = c(-2.5, 2.5))
#> [1] -2  2

# Each of the four is a spelling of `round_up()` or `round_down()`:
round_ties_away(x = -2.5)
#> [1] -3
round_up(x = -2.5, symmetric = TRUE)
#> [1] -3

# `round_ties_even()` is `base::round()`:
round_ties_even(x = c(0.5, 1.5, 2.5))
#> [1] 0 2 2
```
