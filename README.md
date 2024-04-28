
<!-- README.md is generated from README.Rmd. Please edit that file -->

# roundwork: Choose rounding options

<!-- badges: start -->

[![R-CMD-check](https://github.com/lhdjung/roundwork/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lhdjung/roundwork/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of roundwork is to allow for flexible rounding beyond
`base::round()`. Its main API is `reround()`, from which all implemented
rounding procedures can be used.

All roundwork functions used to be part of
[scrutiny](https://lhdjung.github.io/scrutiny/). They were outsourced
here for better modularity within the errorverse.

## Installation

You can install the development version of roundwork like so:

``` r
remotes::install_github("lhdjung/roundwork")
```
