#' Remove the integer part, keeping the decimal part
#'
#' `trunc_reverse()` reduces a number to its decimal portion. It is the opposite
#' of `trunc()`: Whereas `trunc(3.45)` returns `3,` `trunc_reverse(3.45)`
#' returns `0.45`.
#'
#' This is used in some unit tests.
#'
#' @param x Decimal number.
#'
#' @return Decimal part of `x`.
#'
#' @noRd
trunc_reverse <- function(x) {
  x - trunc(x)
}
