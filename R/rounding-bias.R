#' Compute rounding bias
#'
#' @description Rounding often leads to bias, such that the mean of a rounded
#'   distribution is different from the mean of the original distribution. Call
#'   `rounding_bias()` to compute the amount of this bias.
#'
#' @details Bias is calculated by subtracting the original vector, `x`, from a
#'   vector rounded in the specified way.
#'
#'   The function passes all arguments except for `mean` down to [`reround()`].
#'   Other than there, however, `rounding` is `"up"` by default, and it can't be
#'   set to `"up_or_down"`, `"up_from_or_down_from"`, or `"ceiling_or_floor"`.
#'   Bias is a property of one rounding procedure, and each of those three names
#'   two: they would leave two rounded values per element of `x`, with no one
#'   value to subtract `x` from. Call `rounding_bias()` once per procedure to
#'   compare them.
#'
#' @param x Numeric or string coercible to numeric.
#' @param digits Integer. Number of decimal digits to which `x` will be rounded.
#' @param rounding String. Rounding procedure that will be applied to `x`. See
#'   `vignette("rounding-options")`. Default is `"up"`.
#' @param threshold,symmetric Further arguments passed down to [`reround()`].
#' @param mean Logical. If `TRUE` (the default), the mean total of bias will be
#'   returned. Set `mean` to `FALSE` to get a vector of individual biases the
#'   length of `x`.
#'
#' @include reround.R
#'
#' @return Numeric. By default of `mean`, the length is 1; otherwise, it is the
#'   same length as `x`.
#'
#' @export
#'
#' @examples
#' # Define example vector:
#' vec <- seq(from = 0.01, to = 0.1, by = 0.01)
#' vec
#'
#' # The default rounds `x` up from 5:
#' rounding_bias(x = vec, digits = 1)
#'
#' # Other rounding procedures are supported,
#' # such as rounding down from 5...
#' rounding_bias(x = vec, digits = 1, rounding = "down")
#'
#' # ...or rounding to even with `base::round()`:
#' rounding_bias(x = vec, digits = 1, rounding = "even")

rounding_bias <- function(
  x,
  digits,
  rounding = "up",
  threshold = 5,
  symmetric = FALSE,
  mean = TRUE
) {
  # Checks ---

  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(
    x,
    digits,
    rounding,
    threshold,
    symmetric
  ))

  # A compound method names two procedures, so `reround()` returns two values
  # per element of `x`. Subtracting `x` from that then recycles `x` across the
  # pairs, pairing each value with whichever element happens to line up, and the
  # result is twice as long as `x` and misaligned throughout. The documentation
  # has always said these three are unsupported; without this check, they went
  # through silently and returned nonsense.
  #
  # `rounding_constituents()` is the same table `reround_to_fraction()` uses to
  # tell the branches of a compound method apart, so the check cannot come to
  # disagree with `reround()` about which methods are compound. Length > 1 is
  # left to `reround()`, which rejects it with a message about `rounding`
  # describing a single procedure:
  if (length(rounding) == 1L) {
    procedures <- rounding_constituents(rounding)
    if (length(procedures) > 1L) {
      cli::cli_abort(c(
        "`rounding` must name a single rounding procedure.",
        "x" = "It is {wrong_spec_string(rounding)}, which names \\
        {length(procedures)}: {.val {procedures}}.",
        "i" = "Bias is a property of one procedure: it is the amount by which \\
        that procedure shifts `x`.",
        ">" = "Call `rounding_bias()` once per procedure to compare them, \\
        e.g. with `rounding = \"{procedures[1L]}\"`."
      ))
    }
  }

  # Main part ---

  bias <- reround(x, digits, rounding, threshold, symmetric) - x

  if (mean) {
    mean(bias)
  } else {
    bias
  }
}

# # Proof that it works (this is for a previous version that didn't compute the
# # mean but only subtracted `x` from `x_rounded`) --
#
# # Define example vector:
# x <- seq_distance(0.01, string_output = FALSE)
# decimals <- 1
# rounding <- "up"
# threshold <- 5
# # These are all `TRUE`, so `x` can be reconstructed from first rounding it in
# # the specified way, then subtracting the bias:
# dplyr::near(
#   (reround(x, digits, rounding, threshold) - rounding_bias(x, 1)), x
# )
