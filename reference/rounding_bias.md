# Compute rounding bias

Rounding changes the mean of a distribution. `rounding_bias()` computes
this bias as the mean of the rounded values minus the mean of the
original ones, or, with `mean = FALSE`, the individual differences.

## Usage

``` r
rounding_bias(
  x,
  digits,
  rounding = "up",
  threshold = 5,
  symmetric = FALSE,
  mean = TRUE
)
```

## Arguments

- x:

  Numeric, or a string coercible to numeric.

- digits:

  Integer. Number of decimal places to round to.

- rounding:

  String. The rounding method. Default is `"up"`. See
  [`vignette("rounding-options")`](https://lhdjung.github.io/roundwork/articles/rounding-options.md).

- threshold, symmetric:

  Passed on to
  [`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md).

- mean:

  Logical. If `TRUE` (the default), return the mean bias; otherwise, the
  individual biases.

## Value

Numeric. Length 1 by default, or the length of `x` with `mean = FALSE`.

## Details

All arguments except `mean` are passed on to
[`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md).
Unlike there, `rounding` defaults to `"up"` and cannot be a compound
method (`"up_or_down"`, `"up_from_or_down_from"`, `"ceiling_or_floor"`),
since those return two rounded values per input and have no single bias.

## Examples

``` r
vec <- 0.01 |> seq(0.1, by = 0.01) |> round(2)
vec
#>  [1] 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10

rounding_bias(x = vec, digits = 1)
#> [1] 0.005
rounding_bias(x = vec, digits = 1, rounding = "down")
#> [1] -0.005
rounding_bias(x = vec, digits = 1, rounding = "even")
#> [1] -0.005
```
