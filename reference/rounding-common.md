# Common rounding procedures

`round_up()` rounds ties upward, and `round_down()` rounds them
downward. Otherwise, both agree with
[`base::round()`](https://rdrr.io/r/base/Round.html).

They are special cases of `round_up_from()` and `round_down_from()`,
which switch direction at a `threshold` other than 5.

## Usage

``` r
round_up_from(x, digits = 0L, threshold, symmetric = FALSE)

round_down_from(x, digits = 0L, threshold, symmetric = FALSE)

round_up(x, digits = 0L, symmetric = FALSE)

round_down(x, digits = 0L, symmetric = FALSE)
```

## Arguments

- x:

  Numeric. The number to round.

- digits:

  Integer. Number of decimal places to round to. Default is `0`.
  Negative values round to powers of ten: `round_up(1250, digits = -2)`
  is `1300`.

- threshold:

  Numeric. Only in `round_up_from()` and `round_down_from()`. The point
  within a step at which rounding switches direction, in tenths of a
  step; greater than `0` and less than `10`. `round_up_from()` rounds up
  when the part cut off is at least `threshold` tenths of a step, and
  `round_down_from()` rounds down when it is at most that many, so the
  two only differ for a value exactly on the threshold. `round_up()` and
  `round_down()` are the pair at `5`.

- symmetric:

  Logical. If `TRUE`, negative numbers are rounded like their absolute
  values. Default is `FALSE`. See *Negative numbers*.

## Value

Numeric. `x` rounded to `digits`.

## Details

These functions decide a tie by a fixed rule.
[`base::round()`](https://rdrr.io/r/base/Round.html) rounds ties to even
instead, and decides on the binary double rather than the printed
decimal. They exist to reconstruct the output of software that rounds
this way; see
[`vignette("rounding-options")`](https://lhdjung.github.io/roundwork/articles/rounding-options.md)
for which program uses which procedure. They are not meant to replace
[`base::round()`](https://rdrr.io/r/base/Round.html) in original
computations.

## Negative numbers

By default (`symmetric = FALSE`), `round_up()` moves a tie to the higher
number on the number line, so `round_up(-2.5)` is `-2`, and
`round_down(-2.5)` is `-3`. This is what Stata does.

With `symmetric = TRUE`, a negative number is rounded like its absolute
value, so a tie moves away from zero: `round_up(-2.5, symmetric = TRUE)`
is `-3`. This is what Excel, SAS, SPSS, and Matlab do; IEEE 754 calls it
*roundTiesToAway*.

For `round_up()` and `round_down()`, `symmetric` only affects ties among
negative numbers. For `round_up_from()` and `round_down_from()` with a
`threshold` other than 5, it affects every negative number whose cut-off
part lies between `threshold` and `10 - threshold`:
`round_up_from(-4.28, 1, threshold = 9)` is `-4.3`, and `-4.2` with
`symmetric = TRUE`.

Under the package-wide default `rounding = "up_or_down"`, both results
are accepted, so `symmetric` cannot change a consistency verdict there.

## Floating-point tolerance

Shifting a number by `digits` decimal places is not exact: `0.145 * 100`
is stored as `14.499999999999998`, just below the tie it is meant to be.
All of these functions therefore nudge the shifted value by about
`1.5e-9` before rounding, so that `round_up(0.145, 2)` is `0.15`, as in
the software being reconstructed.
[`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
reports bounds that assume the same tolerance.

The nudge is fixed, whereas representation error grows with
`x * 10^digits`. Up to about `1e7` for that product, far beyond any
reported summary statistic, the nudge dominates. Past it, a value
exactly on a boundary may be rounded in either direction.

## See also

[`round_ties_up()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md)
and friends are the same procedures under names that state the tie rule,
so that `symmetric` need not be given.
[`round_ceiling()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md),
[`round_floor()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md),
[`round_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md),
and
[`round_anti_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
always round in one direction.

## Examples

``` r
# Except at a tie, all of these agree:
   round_up(x = 9.273, digits = 1)
#> [1] 9.3
 round_down(x = 9.273, digits = 1)
#> [1] 9.3
base::round(x = 9.273, digits = 1)
#> [1] 9.3

# At ties, they differ:
original <- c(
  0.05, 0.15, 0.25, 0.35, 0.45,
  0.55, 0.65, 0.75, 0.85, 0.95
)
tibble::tibble(
  original,
  round_up = round_up(x = original, digits = 1),
  round_down = round_down(x = original, digits = 1),
  base_round = base::round(x = original, digits = 1)
)
#> # A tibble: 10 × 4
#>    original round_up round_down base_round
#>       <dbl>    <dbl>      <dbl>      <dbl>
#>  1     0.05      0.1        0          0  
#>  2     0.15      0.2        0.1        0.1
#>  3     0.25      0.3        0.2        0.2
#>  4     0.35      0.4        0.3        0.3
#>  5     0.45      0.5        0.4        0.4
#>  6     0.55      0.6        0.5        0.6
#>  7     0.65      0.7        0.6        0.7
#>  8     0.75      0.8        0.7        0.8
#>  9     0.85      0.9        0.8        0.8
#> 10     0.95      1          0.9        0.9

# Ties in negative numbers go up on the number line
# by default, and away from zero with `symmetric`:
round_up(x = -2.5)                     # Stata
#> [1] -2
round_up(x = -2.5, symmetric = TRUE)   # Excel, SAS, SPSS, Matlab
#> [1] -3

# A custom threshold moves the point at which
# rounding switches direction:
round_up_from(x = 4.28, digits = 1, threshold = 9)   # 8 < 9, so down
#> [1] 4.2
round_up_from(x = 4.28, digits = 1, threshold = 1)   # 8 >= 1, so up
#> [1] 4.3
```
