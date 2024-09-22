
<!-- README.md is generated from README.Rmd. Please edit that file -->

# roundwork

<!-- badges: start -->

[![R-CMD-check](https://github.com/lhdjung/roundwork/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lhdjung/roundwork/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of roundwork is to enable flexible rounding beyond
`base::round()`. Its main API is `reround()`, from which all implemented
rounding procedures can be accessed. This is useful to reconstruct how
others may have rounded their data.

Given a rounded number, call `unround()` to get the lower and upper
bounds of the original number. This also takes rounding options into
account, just like `reround()` does.

Many roundwork functions used to be part of
[scrutiny](https://lhdjung.github.io/scrutiny/). They were outsourced
here for better modularity within the errorverse.

## Installation

Install the package from CRAN:

    install.packages("roundwork")

Alternatively, install the development version from GitHub:

``` r
remotes::install_github("lhdjung/roundwork")
```

## Get started

Round in some specific way, e.g., up from 5:

``` r
round_up(4.1679, digits = 2)
#> [1] 4.17
```

Use `reround()` to round more flexibly, especially in your own
functions. You can specify a rounding procedure:

``` r
reround(4.1679, digits = 2, rounding = "up")
#> [1] 4.17
```

Ceil or floor numbers if certain strong assumptions are permitted:

``` r
reround(4.1679, digits = 2, rounding = "floor")
#> [1] 4.16
```

The default is to return two numbers — one rounded up, one rounded down.
This is best for conservative assumptions, so that your conclusions
won’t hinge on edge cases where these rounding procedures differ.

``` r
reround(1.275, digits = 2)
#> [1] 1.28 1.27
```

### Bounds on the unrounded number

`unround()` computes the range of an unknown original number, given its
rounded version. The rounding procedure determines whether the bounds
are inclusive or not:

``` r
unround("3.60")
#> # A tibble: 1 × 7
#>   range                     rounding   lower incl_lower x     incl_upper upper
#>   <chr>                     <chr>      <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 3.595 <= x(3.60) <= 3.605 up_or_down  3.60 TRUE       3.60  TRUE        3.60

unround("3.60", rounding = "up")
#> # A tibble: 1 × 7
#>   range                    rounding lower incl_lower x     incl_upper upper
#>   <chr>                    <chr>    <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 3.595 <= x(3.60) < 3.605 up        3.60 TRUE       3.60  FALSE       3.60

unround("3.60", rounding = "even")
#> # A tibble: 1 × 7
#>   range                     rounding lower incl_lower x     incl_upper upper
#>   <chr>                     <chr>    <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 3.595 NA x(3.60) NA 3.605 even      3.60 NA         3.60  NA          3.60
```
