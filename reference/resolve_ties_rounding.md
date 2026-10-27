# Resolve a `"ties_*"` rounding string

`resolve_ties_rounding()` translates one of the `"ties_*"` rounding
strings into the `rounding` and `symmetric` values it stands for, such
as `"ties_away"` into `rounding = "up"` and `symmetric = TRUE`. Any
other `rounding` is returned unchanged, along with `symmetric`.

[`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md)
and
[`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
resolve these strings the same way, so a function that branches on
`rounding` itself can call this first to agree with them.

## Usage

``` r
resolve_ties_rounding(rounding, symmetric)
```

## Arguments

- rounding:

  String (length 1). A rounding method. Not checked.

- symmetric:

  Logical (length 1). See
  [`round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md).
  Not checked.

## Value

A list with the elements `rounding` and `symmetric`.

## Details

`resolve_ties_rounding()` is a low-level primitive and does not check
its input. The caller is responsible for passing a single string as
`rounding` and a single `TRUE` or `FALSE` as `symmetric`. In particular,
a number as `rounding` would pick a `"ties_*"` method by position.

Combining a `"ties_*"` string with `symmetric = TRUE` is an error,
because the string already determines how negative ties are broken.

## Examples

``` r
resolve_ties_rounding("ties_away", symmetric = FALSE)
#> $rounding
#> [1] "up"
#> 
#> $symmetric
#> [1] TRUE
#> 

# Other methods pass through:
resolve_ties_rounding("up", symmetric = TRUE)
#> $rounding
#> [1] "up"
#> 
#> $symmetric
#> [1] TRUE
#> 
```
