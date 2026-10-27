# Rounding in one direction

These functions ignore which neighbor is nearer:

- `round_ceiling()` always rounds up.

- `round_floor()` always rounds down.

- `round_trunc()` always rounds toward zero.

- `round_anti_trunc()` always rounds away from zero. A value that
  already sits on the rounding grid stays where it is, so `0` stays `0`.
  This is what Excel's `ROUNDUP()`, Java's `RoundingMode.UP`, and
  Python's `decimal.ROUND_UP` do.

- `anti_trunc()` is the counterpart of
  [`trunc()`](https://rdrr.io/r/base/Round.html): it rounds a number to
  the nearest whole number away from zero. Whole numbers stay unchanged.

## Usage

``` r
round_ceiling(x, digits = 0L)

round_floor(x, digits = 0L)

round_trunc(x, digits = 0L)

anti_trunc(x)

round_anti_trunc(x, digits = 0L)
```

## Arguments

- x:

  Numeric. The number to round.

- digits:

  Integer. Number of decimal places to round to. Default is `0`.
  Negative values round to powers of ten.

## Value

Numeric. `x` rounded to `digits` (`anti_trunc()` has no `digits`
argument).

## Details

`round_ceiling()`, `round_floor()`, and `round_trunc()` generalize
[`ceiling()`](https://rdrr.io/r/base/Round.html),
[`floor()`](https://rdrr.io/r/base/Round.html), and
[`trunc()`](https://rdrr.io/r/base/Round.html) to any number of decimal
places, and `round_anti_trunc()` generalizes `anti_trunc()`. With the
default `digits = 0`, each equals its base counterpart.

`round_trunc()` is `round_floor()` for positive and `round_ceiling()`
for negative numbers; `round_anti_trunc()` is the reverse.

All of these apply the floating-point tolerance described in
[`round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md),
so that `round_ceiling(0.28, 2)` is `0.28` although `0.28 * 100` is
stored as `28.000000000000004`.

## See also

[`round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
and
[`round_down()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
round to the nearest number and decide ties by a fixed direction.

## Examples

``` r
round_ceiling(x = 4.52, digits = 1)
#> [1] 4.6
round_floor(x = 4.67, digits = 1)
#> [1] 4.6

round_trunc(8.439, digits = 2)
#> [1] 8.43
round_trunc(-8.439, digits = 2)
#> [1] -8.43

round_anti_trunc(x = 8.421, digits = 2)
#> [1] 8.43
round_anti_trunc(x = -8.421, digits = 2)
#> [1] -8.43
```
