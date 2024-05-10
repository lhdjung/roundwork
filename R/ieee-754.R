#' IEEE 754 rounding standard
#'
#' @description These functions implement the industry standard IEEE 754:
#'
#' - `round_ties_to_even()` rounds to the nearest value. If both are at equal
#'   distance, rounds to the even number (but see details in [`base::round()`]).
#' - `round_ties_to_away()` rounds to the nearest value. If both are at equal
#'   distance, rounds to the number with the greater absolute value, i.e., the
#'   number that is further away from zero.
#' - `round_toward_positive()` always rounds to the greater of the two nearest
#'   values. This is like ceiling for the given number of decimal places.
#' - `round_toward_negative()` always rounds to the lesser of the two nearest
#'   values. This is like flooring for the given number of decimal places.
#' - `round_toward_zero()` always rounds to the number with the lower absolute
#'   value, i.e., the number that is closer to zero.
#'
#' @param ... Only in `round_ties_to_even()`. Passed down to [`base::round()`].
#' @inheritParams rounding-common
#'
#' @details The function names follow the official standard (IEEE 2019, pp.
#'   27f.; but the [Wikipedia
#'   page](https://en.m.wikipedia.org/wiki/IEEE_754#Rounding_rules) is more
#'   accessible). Internally, these functions are just wrappers around other
#'   roundwork functions and [`base::round()`]. They are presented here for easy
#'   compliance with the IEEE 754 standard in R.
#'
#' @return Numeric. `x` rounded to `digits`.
#'
#' @include round.R
#'
#' @name ieee-754
#' @export
#'
#' @references IEEE (2019). *IEEE Standard for Floating-Point Arithmetic.*
#'   [https://doi.org/10.1109/IEEESTD.2019.8766229]

# @examples

round_ties_to_even <- function(x, digits = 0, ...) {
  round(x = x, digits = digits, ...)
}

#' @rdname ieee-754
#' @export
round_ties_to_away <- function(x, digits = 0) {
  round_up(x = x, digits = digits, symmetric = TRUE)
}

#' @rdname ieee-754
#' @export
round_toward_positive <- function(x, digits = 0) {
  round_ceiling(x = x, digits = digits)
}

#' @rdname ieee-754
#' @export
round_toward_negative <- function(x, digits = 0) {
  round_floor(x = x, digits = digits)
}

#' @rdname ieee-754
#' @export
round_toward_zero <- function(x, digits = 0) {
  round_trunc(x = x, digits = digits)
}
