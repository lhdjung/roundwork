# Rounding bounds as exact integers

`bound_numerators()` returns the same rounding bounds as
[`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md),
but as exact integer numerators over a common denominator rather than as
decimal numbers. It is the low-level counterpart to
[`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md),
meant for building consistency tests on top of.

Most users want
[`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
instead.

## Usage

``` r
bound_numerators(x_num, digits, rounding, threshold, symmetric)
```

## Arguments

- x_num:

  Numeric (length 1). The rounded number. Not checked.

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

A list of five elements, or `NULL` if the bounds are undefined, which is
the case only for a missing or infinite `x_num`:

- `lower`, `upper`: integer numerators of the two bounds.

- `denom`: the common denominator.

- `incl_lower`, `incl_upper`: whether each bound is inclusive.

Throws an error if `rounding` is not a known method.

## Details

Deriving a set of candidate values from floating-point bounds is unsafe.
If a product such as `floor(upper * n)` is mathematically an exact
integer, its `double` representation can fall on either side of it, so a
legitimate candidate may be silently dropped or a phantom one admitted.
Either way, a consistency verdict can flip. See
<https://github.com/lhdjung/scrutiny/issues/86>.

Every bound that
[`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
can return is `x` plus a whole number of units of `1 / 10^(digits + 1)`,
and `x` itself is a whole number of such units because it has `digits`
decimal places. Both bounds therefore have exact integer numerators over
`10^(digits + 1)`, and a comparison of some value to a bound becomes a
comparison between integers, which is exact in double precision as long
as every integer involved stays below `2^53`.

A consistency test that derives its candidate range from here can never
disagree with
[`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
about what the bounds of a rounded number are.

If `threshold` is fractional, the numerators and the denominator are
both scaled up by a power of ten until they are whole numbers again.

`bound_numerators()` is a low-level primitive, meant to be called once
per value set inside a consistency test, so it checks only `rounding`,
`threshold`, and `symmetric`. The caller is responsible for passing a
single number as `x_num` and a whole number of decimal places that fits
it as `digits`. Other input may return meaningless numerators or fail
with an uninformative error.
[`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
checks all of its input.

## See also

[`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md),
which presents the same bounds as decimal numbers in a tibble.

## Examples

``` r
# The bounds of 2.7, rounded up or down from 5,
# are 2.65 and 2.75 -- here as 265/100 and 275/100:
bound_numerators(
  x_num = 2.7,
  digits = 1,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
#> $lower
#> [1] 265
#> 
#> $upper
#> [1] 275
#> 
#> $denom
#> [1] 100
#> 
#> $incl_lower
#> [1] TRUE
#> 
#> $incl_upper
#> [1] TRUE
#> 

# `unround()` shows the same bounds as decimals:
unround("2.7")
#> # A tibble: 1 × 7
#>   range                  rounding   lower incl_lower x     incl_upper upper
#>   <chr>                  <chr>      <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 2.65 <= x(2.7) <= 2.75 up_or_down  2.65 TRUE       2.7   TRUE        2.75
```
