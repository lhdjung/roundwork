# General interface to reconstructing rounded numbers

`reround()` rounds a vector of reconstructed values the way they are
assumed to have been rounded originally. It is the common interface to
all of roundwork's rounding functions and to
[`base::round()`](https://rdrr.io/r/base/Round.html).

## Usage

``` r
reround(
  x,
  digits = 0L,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
```

## Arguments

- x:

  Numeric. Values to round.

- digits:

  Integer. Number of decimal places to round to, i.e., the number in the
  reported values. Length 1 or the length of `x`. Must be whole numbers;
  a missing value propagates to a missing result.

- rounding:

  String (length 1). The rounding method. Default is `"up_or_down"`,
  which returns two values per element of `x`: rounded up and rounded
  down.

- threshold:

  Numeric (length 1). For `"up_from"`, `"down_from"`, and
  `"up_from_or_down_from"`, the point within a step at which rounding
  switches direction, in tenths of a step; greater than `0` and less
  than `10`. Default is `5`, which makes these methods the same as
  `"up"`, `"down"`, and `"up_or_down"`. See
  [`round_up_from()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md).

- symmetric:

  Logical (length 1). If `TRUE`, negative numbers are rounded like their
  absolute values under `"up"`, `"down"`, `"up_or_down"`, and the
  `"*_from"` methods. Default is `FALSE`. Must not be `TRUE` with a
  `"ties_*"` method, which already fixes the direction: `"ties_up"` with
  `symmetric = TRUE` would be `"ties_away"`, so write that instead.

## Value

Numeric. One value per element of `x`, except for the compound methods
`"up_or_down"`, `"up_from_or_down_from"`, and `"ceiling_or_floor"`,
which return two: the results of their two constituent procedures, next
to each other, as `c(up_1, down_1, up_2, down_2, ...)`. Each pair
belongs to one element of `x`, so the pairs must not be pooled across
elements.

## Details

`reround()` calls the rounding function that `rounding` names. See
[`vignette("rounding-options")`](https://lhdjung.github.io/roundwork/articles/rounding-options.md)
for the possible values, and
[`round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
and
[`round_ceiling()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
for the functions themselves.
