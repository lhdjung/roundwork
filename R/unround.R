# Exact candidate-sum arithmetic ------------------------------------------

# Consistency tests such as GRIM need the set of integer sums `s` for which
# `s / n_items` would have been rounded to the reported mean. Deriving it from
# floating-point products such as `floor(upper * n_items)` is unsafe: where such
# a product is mathematically an exact integer, its `double` can fall on either
# side of it, dropping a legitimate sum or admitting a phantom one, and the
# verdict flips (scrutiny issue #86).
#
# The functions below therefore work in integer arithmetic. Every bound that
# `unround()` can return is `x` plus a whole number of units of
# `1 / 10^(digits + 1)`, and `x` itself is such a whole number because it has
# `digits` decimal places -- so both bounds have exact integer numerators over
# `10^(digits + 1)`, and `s / n_items` vs. `numerator / 10^(digits + 1)` becomes
# a comparison of the integers `s * 10^(digits + 1)` and `n_items * numerator`.
#
# This is exact while every integer involved stays below `2^53`. The products
# are on the order of `n * 10^(2 * digits)`, far below it for any realistic
# input; beyond it the arithmetic degrades to plain floating point.
#
# Dividing two such integers is exact as well, so `floor(a / b)` and
# `ceiling(a / b)` need no correction. IEEE 754 division returns the double
# nearest the true quotient, and a quotient that is not an integer lies at least
# `1 / b` from every integer -- more than half the spacing of doubles there, as
# long as `|a| < 2^53`. Don't add a check that multiplies the result back out:
# below that limit it never corrects anything, and above it, where the product
# rounds, it turns correct results into wrong ones more often than the reverse.


# Integer offsets of the lower and upper rounding bounds from `x_num`, in units
# of `1 / 10^(digits + 1)`, plus each bound's inclusivity. This is the single
# source of truth for rounding bounds: `unround()`, the consistency tests, and
# every other function that needs such bounds derive them from it. The offsets
# follow the table in the `Rounding` section of `unround()`'s documentation,
# plus the compound methods, whose bounds are the union of their constituents'
# (a single interval, since both constituents include `x_num`).
#
# Each bound is inclusive exactly as the corresponding function in reround.R
# behaves there -- e.g. `"up"` excludes its upper bound, where a value rounds
# away from `x_num`. `"even"` is the exception: `base::round()` breaks ties by
# the parity of the preceding digit, and whether a tie occurs at all depends on
# the binary representation, so both its bounds are treated as inclusive. That
# makes a test too permissive rather than too strict -- the safe direction.
#
# `threshold` deliberately plays no role for `"up_or_down"`, `"up"`, and
# `"down"`, matching `round_up()` and `round_down()`, which round from a fixed
# 5. The `"*_from"` methods are the parameterized ones.
#
# Returns a list of lower offset, upper offset, `incl_lower`, `incl_upper` --
# all `NA` if `x_num` is missing or infinite, or `NULL` if `rounding` is
# unknown.

rounding_offsets <- function(rounding, threshold, x_num, symmetric = FALSE) {
  check_rounding_spec_singular(rounding, threshold, symmetric)

  # The branches below need a sign, and a missing value has none. A positive
  # stand-in keeps `rounding` validated as for any other value; its offsets are
  # discarded at the end. An infinity counts as missing, as in the tests: its
  # "bounds" would be `Inf` on both sides, which is no range at all.
  x_missing <- !is.finite(x_num)
  if (x_missing) {
    x_num <- 1
  }

  # `reround()` resolves `"ties_*"` strings through the same helper, so the
  # bounds stay in step with the rounding functions they invert:
  spec <- resolve_ties_rounding(rounding, symmetric)
  rounding <- spec$rounding
  symmetric <- spec$symmetric

  # Only the parameterized methods validate `threshold`, as in `reround()`: a
  # threshold outside `(0, 10)` makes one direction unreachable, and the offsets
  # below would encode that silently:
  if (rounding %in% c("up_from", "down_from", "up_from_or_down_from")) {
    check_threshold_valid(threshold)
  }

  # With `symmetric`, a negative number rounds like its absolute value -- which
  # is what the opposite method does to it anyway, so swapping the method
  # suffices. A threshold is measured from the lower end of the step, so
  # mirroring the step mirrors it too; at the fixed `5` of `"up"`/`"down"`,
  # `10 - threshold` is `threshold` again:
  if (symmetric && x_num < 0) {
    if (rounding %in% c("up_from", "down_from", "up_from_or_down_from")) {
      threshold <- 10 - threshold
    }
    # fmt: skip
    rounding <- switch(
      rounding,
      "up"        = "down",
      "down"      = "up",
      "up_from"   = "down_from",
      "down_from" = "up_from",
      rounding
    )
  }

  # Truncation and "anti-truncation" depend on the sign of `x_num`:

  # fmt: skip
  if (rounding == "trunc") {
    offsets <- if (x_num > 0) {
      list(0,   10,  TRUE,  FALSE)
    } else if (x_num < 0) {
      list(-10, 0,   FALSE, TRUE)
    } else {
      list(-10, 10,  FALSE, FALSE)
    }
  } else if (rounding == "anti_trunc") {
    # `anti_trunc()` is `round_ceiling()` above zero and `round_floor()` below
    # it. At zero it is neither: every non-zero value is taken away from zero,
    # so only zero itself is reported as zero. That single-point range is a real
    # answer, not a missing one.
    offsets <- if (x_num > 0) {
      list(-10, 0,   FALSE, TRUE)
    } else if (x_num < 0) {
      list(0,   10,  TRUE,  FALSE)
    } else {
      list(0,   0,   TRUE,  TRUE)
    }
  } else {
    # fmt: skip
    offsets <- switch(
      rounding,              #     lower            upper           incl_lower  incl_upper
      "up_or_down"           = list(-5,             5,              TRUE,       TRUE),
      "up"                   = list(-5,             5,              TRUE,       FALSE),
      "down"                 = list(-5,             5,              FALSE,      TRUE),
      "even"                 = list(-5,             5,              TRUE,       TRUE),
      "ceiling"              = list(-10,            0,              FALSE,      TRUE),
      "floor"                = list(0,              10,             TRUE,       FALSE),
      "ceiling_or_floor"     = list(-10,            10,             FALSE,      FALSE),
      "up_from"              = list(threshold - 10, threshold,      TRUE,       FALSE),
      "down_from"            = list(threshold - 10, threshold,      FALSE,      TRUE),
      "up_from_or_down_from" = list(threshold - 10, threshold,      TRUE,       TRUE),
      return(NULL)
    )
  }

  # At zero the mirroring happens inside the interval: its negative half is the
  # reflection of its positive half, so both ends behave like the upper end
  # does for a positive number.

  # fmt: skip
  if (
    symmetric &&
      x_num == 0 &&
      rounding %in% c(
        "up_or_down", "up", "down",
        "up_from_or_down_from", "up_from", "down_from"
      )
  ) {
    offsets[[1L]] <- -offsets[[2L]]
    offsets[[3L]] <- offsets[[4L]]
  }

  if (x_missing) {
    # `bound_numerators()` turns these into `NULL`, which every caller already
    # reports as an undecidable case:
    return(list(NA, NA, NA, NA))
  }

  offsets
}


#' Rounding bounds as exact integers
#'
#' @description `bound_numerators()` returns the same rounding bounds as
#'   [`unround()`], but as exact integer numerators over a common denominator
#'   rather than as decimal numbers. It is the low-level counterpart to
#'   `unround()`, meant for building consistency tests on top of.
#'
#'   Most users want [`unround()`] instead.
#'
#' @details Deriving a set of candidate values from floating-point bounds is
#'   unsafe. If a product such as `floor(upper * n)` is mathematically an exact
#'   integer, its `double` representation can fall on either side of it, so a
#'   legitimate candidate may be silently dropped or a phantom one admitted.
#'   Either way, a consistency verdict can flip. See
#'   <https://github.com/lhdjung/scrutiny/issues/86>.
#'
#'   Every bound that `unround()` can return is `x` plus a whole number of units
#'   of `1 / 10^(digits + 1)`, and `x` itself is a whole number of such units
#'   because it has `digits` decimal places. Both bounds therefore have exact
#'   integer numerators over `10^(digits + 1)`, and a comparison of some value
#'   to a bound becomes a comparison between integers, which is exact in double
#'   precision as long as every integer involved stays below `2^53`.
#'
#'   A consistency test that derives its candidate range from here can never
#'   disagree with `unround()` about what the bounds of a rounded number are.
#'
#'   If `threshold` is fractional, the numerators and the denominator are both
#'   scaled up by a power of ten until they are whole numbers again.
#'
#'   `bound_numerators()` is a low-level primitive, meant to be called once per
#'   value set inside a consistency test, so it checks only `rounding`,
#'   `threshold`, and `symmetric`. The caller is responsible for passing a
#'   single number as `x_num` and a whole number of decimal places that fits
#'   it as `digits`. Other input may return meaningless numerators or fail with
#'   an uninformative error. [`unround()`] checks all of its input.
#'
#' @param x_num Numeric (length 1). The rounded number. Not checked.
#' @param digits Integer (length 1). Number of decimal places in `x_num`. Not
#'   checked.
#' @param rounding String (length 1). Rounding method presumably used to create
#'   `x_num`. See the `Rounding` section of [`unround()`].
#' @param threshold Numeric (length 1). The point within a step at which
#'   rounding switches direction, in tenths of a step, for the `"up_from"`,
#'   `"down_from"`, and `"up_from_or_down_from"` methods. See
#'   [`round_up_from()`].
#' @param symmetric Logical (length 1). Whether the rounding of negative numbers
#'   mirrored that of positive numbers. See [`round_up()`].
#'
#' @return A list of five elements, or `NULL` if the bounds are undefined, which
#'   is the case only for a missing or infinite `x_num`:
#'   - `lower`, `upper`: integer numerators of the two bounds.
#'   - `denom`: the common denominator.
#'   - `incl_lower`, `incl_upper`: whether each bound is inclusive.
#'
#'   Throws an error if `rounding` is not a known method.
#'
#' @seealso [`unround()`], which presents the same bounds as decimal numbers in
#'   a tibble.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # The bounds of 2.7, rounded up or down from 5,
#' # are 2.65 and 2.75 -- here as 265/100 and 275/100:
#' bound_numerators(
#'   x_num = 2.7,
#'   digits = 1,
#'   rounding = "up_or_down",
#'   threshold = 5,
#'   symmetric = FALSE
#' )
#'
#' # `unround()` shows the same bounds as decimals:
#' unround("2.7")

bound_numerators <- function(x_num, digits, rounding, threshold, symmetric) {
  offsets <- rounding_offsets(rounding, threshold, x_num, symmetric)

  if (is.null(offsets)) {
    abort_in_export(
      "`rounding` must be one of the designated string values.",
      "x" = "It is {wrong_spec_string(rounding)}.",
      "i" = "See `vignette(\"rounding-options\", package = \"roundwork\")`."
    )
  }

  if (anyNA(offsets)) {
    return(NULL)
  }

  # A fractional `threshold` is scaled up by a power of ten, along with the
  # denominator, until the offsets are whole again; failing that, the
  # arithmetic degrades to floating point:
  bounds <- c(offsets[[1L]], offsets[[2L]])
  scale <- 1
  while (scale < 1e6 && any(bounds * scale != round(bounds * scale))) {
    scale <- scale * 10
  }
  bounds <- bounds * scale

  denom <- 10^(digits + 1L) * scale
  x_shifted <- round(x_num * denom)

  lower <- x_shifted + bounds[1L]
  upper <- x_shifted + bounds[2L]
  incl_lower <- offsets[[3L]]
  incl_upper <- offsets[[4L]]

  list(
    lower = lower,
    upper = upper,
    denom = denom,
    incl_lower = incl_lower,
    incl_upper = incl_upper
  )
}


#' Whole-number sums behind a rounded mean
#'
#' @description `sum_range()` returns the least and the greatest whole number
#'   `s` for which `s / n_items`, rounded as specified, would be reported as
#'   `x_num`. Every whole number between the two qualifies as well.
#'
#'   This is the arithmetic behind the GRIM test (Brown and Heathers 2017): a
#'   mean of `n_items` whole numbers is GRIM-consistent if the range is not
#'   empty. The same goes for any other share of a whole, such as a percentage
#'   of a sample.
#'
#' @details Like [`bound_numerators()`], on which it builds, `sum_range()`
#'   works in exact integer arithmetic. This is exact as long as
#'   `n_items * 10^(digits + 1)` times the integer part of `x_num` stays below
#'   `2^53`, which is far beyond any realistic input.
#'
#'   `sum_range()` is a low-level primitive, meant to be called once per value
#'   set inside a consistency test, so it does not check its input. The caller
#'   is responsible for passing single numbers, a whole-number `digits` that
#'   fits `x_num`, and a whole-number `n_items`. A fractional `n_items` returns
#'   a range for that fraction rather than an error or `NA`. Only `rounding`,
#'   `threshold`, and `symmetric` are checked, by [`bound_numerators()`].
#'
#' @inheritParams bound_numerators
#' @param n_items Numeric (length 1). The number of values averaged; a positive
#'   whole number. Not checked.
#'
#' @return A numeric vector of length 2, `c(lower, upper)`. If `lower` is
#'   greater than `upper`, no whole-number sum is consistent with `x_num`. Both
#'   are `NA` if `x_num` is missing or infinite, or if `n_items` is missing,
#'   infinite, or not positive.
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4),
#'   363-369. \doi{10.1177/1948550616673876}
#'
#' @export
#'
#' @examples
#' # A mean of 5.18 from 28 whole numbers requires their sum to be 145:
#' sum_range(5.18, 28, digits = 2, rounding = "up_or_down", threshold = 5)
#'
#' # No sum gives 5.19: 145 / 28 is 5.18, and 146 / 28 is 5.21:
#' sum_range(5.19, 28, digits = 2, rounding = "up_or_down", threshold = 5)
#'
#' # 34.5% of a sample of 58 is 20 people (or 0.345 as a share):
#' sum_range(0.345, 58, digits = 3, rounding = "up_or_down", threshold = 5)

sum_range <- function(
  x_num,
  n_items,
  digits,
  rounding,
  threshold,
  symmetric = FALSE
) {
  bounds <- bound_numerators(x_num, digits, rounding, threshold, symmetric)

  if (is.null(bounds) || !is.finite(n_items) || n_items <= 0) {
    return(c(NA_real_, NA_real_))
  }

  denom <- bounds$denom

  # `s / n_items >= lower / denom`  <==>  `s * denom >= n_items * lower`
  lower <- ceiling(n_items * bounds$lower / denom)
  if (!bounds$incl_lower && lower * denom == n_items * bounds$lower) {
    lower <- lower + 1
  }

  # `s / n_items <= upper / denom`  <==>  `s * denom <= n_items * upper`
  upper <- floor(n_items * bounds$upper / denom)
  if (!bounds$incl_upper && upper * denom == n_items * bounds$upper) {
    upper <- upper - 1
  }

  c(lower, upper)
}


#' Reconstruct rounding bounds
#'
#' @description `unround()` takes a rounded number and returns the range of
#'   values that would have been rounded to it: a lower and an upper bound,
#'   and whether each is inclusive. The default rounding method is
#'   `"up_or_down"`; see *Rounding* for the others.
#'
#' @details `x` is a string by default so that its trailing zeros can be
#'   counted, since a numeric value loses them. If the number of decimal places
#'   is known, pass it as `digits`, and `x` may be numeric.
#'   [`bound_numerators()`] and [`sum_range()`] derive their bounds from the
#'   same internal functions as `unround()`, so they always agree with it.
#'
#'   All arguments are vectorized. Those of length greater than 1 must have the
#'   same length and are paired element by element. Pairing `x` with
#'   `rounding` in this way gives a warning.
#'
#' @section Rounding: Whether a bound is inclusive depends on the rounding
#'   method. Under `"up"`, a value on the lower bound rounds up to `x`, but a
#'   value on the upper bound rounds up past it. The `range` column shows the
#'   bounds with the appropriate signs:
#'
#'   | \strong{Value of `rounding`}           | \strong{Corresponding range} |
#'   | ---                                    | ---                          |
#'   | `"up_or_down"` (default)               | `lower <= x <= upper`        |
#'   | `"up"`, `"ties_up"`                    | `lower <= x < upper`         |
#'   | `"down"`, `"ties_down"`                | `lower < x <= upper`         |
#'   | `"ties_away"` (positive `x`)           | `lower <= x < upper`         |
#'   | `"ties_away"` (negative `x`)           | `lower < x <= upper`         |
#'   | `"ties_zero"` (positive `x`)           | `lower < x <= upper`         |
#'   | `"ties_zero"` (negative `x`)           | `lower <= x < upper`         |
#'   | `"ties_away"`, `"ties_zero"` (zero `x`) | `lower < x < upper`          |
#'   | `"even"`, `"ties_even"`                | `lower <= x <= upper`        |
#'   | `"ceiling"`                            | `lower < x = upper`          |
#'   | `"floor"`                              | `lower = x < upper`          |
#'   | `"ceiling_or_floor"`                   | `lower < x < upper`          |
#'   | `"trunc"` (positive `x`)               | `lower = x < upper`          |
#'   | `"trunc"` (negative `x`)               | `lower < x = upper`          |
#'   | `"trunc"` (zero `x`)                   | `lower < x < upper`          |
#'   | `"anti_trunc"` (positive `x`)          | `lower < x = upper`          |
#'   | `"anti_trunc"` (negative `x`)          | `lower = x < upper`          |
#'   | `"anti_trunc"` (zero `x`)              | `lower = x = upper` (all `0`)|
#'   | `"up_from"`                            | `lower <= x < upper`         |
#'   | `"down_from"`                          | `lower < x <= upper`         |
#'   | `"up_from_or_down_from"`               | `lower <= x <= upper`        |
#'
#'   `threshold` only applies to the three `"*_from"` methods. It moves their
#'   range without widening it; each spans exactly one step at every
#'   threshold.
#'
#'   The `"ties_*"` methods each name a complete tie-breaking procedure, so
#'   giving `symmetric = TRUE` with them is an error; see [`round_ties_up()`].
#'
#'   Under `"even"`, both bounds are reported as inclusive. [`base::round()`]
#'   decides a tie on the binary double, so whether a value on a bound rounds
#'   to `x` cannot be predicted from its decimal digits. Treating both bounds
#'   as inclusive can only widen the range, which is the safe direction for
#'   error detection.
#'
#' @param x String or numeric. The rounded number. Must be a string unless
#'   `digits` is specified. A missing or infinite `x` has `NA` bounds.
#' @param rounding String. The rounding method assumed to have produced `x`.
#'   Default is `"up_or_down"`. See *Rounding*.
#' @param threshold Numeric. For the `"*_from"` methods, the point within a
#'   step at which rounding switches direction, in tenths of a step; greater
#'   than `0` and less than `10`. Default is `5`. See [`round_up_from()`].
#' @param digits Integer. The number of decimal places in `x`. Default is
#'   `NULL`, in which case it is counted from the string `x`. Specify it if
#'   the number is already known, as a function that uses `unround()` as a
#'   helper would.
#' @param symmetric Logical. If `TRUE`, negative numbers are assumed to have
#'   been rounded like their absolute values under `"up"`, `"down"`, and the
#'   `"*_from"` methods. Default is `FALSE`. Must not be `TRUE` with a `"ties_*"`
#'   method; see [`reround()`].
#'
#' @return A tibble with the columns `range`, `rounding`, `lower`,
#'   `incl_lower`, `x`, `incl_upper`, and `upper`. `range` summarizes the
#'   columns from `lower` to `upper`.
#'
#' @seealso [`round_up()`] for rounding up or down from 5, and
#'   [`round_ceiling()`] for the one-directional methods.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # By default, `x` may have been rounded up or down:
#' unround(x = "2.7")
#'
#' # If it was rounded up, the upper bound is exclusive:
#' unround(x = "2.7", rounding = "up")
#'
#' # Likewise with rounding down...
#' unround(x = "2.7", rounding = "down")
#'
#' # ...and with `base::round()`:
#' unround(x = "2.7", rounding = "even")
#'
#' # One row per input value:
#' unround(x = c(3.6, "5.20", 5.174))

unround <- function(
  x,
  rounding = "up_or_down",
  threshold = 5,
  digits = NULL,
  symmetric = FALSE
) {
  # Pairing values of `x` with values of `rounding` is confusing enough to warn
  # about:
  check_lengths_congruent(list(x, rounding))

  # No warning for the others: one `digits` per `x` is the ordinary way to call
  # this from a helper.
  check_lengths_congruent(
    list(x, rounding, digits, threshold, symmetric),
    warn = FALSE
  )

  digits_given <- !is.null(digits)
  if (digits_given) {
    check_digits_whole(digits)

    # `as.numeric()` below would read a factor's level codes and `TRUE` as 1:
    if (
      !is.numeric(x) &&
        !is.character(x) &&
        !(is.logical(x) && all(is.na(x)))
    ) {
      abort_in_export(
        "`x` must be a string or a number.",
        "x" = "It is {.obj_type_friendly {x}}."
      )
    }
  } else {
    # Only a string keeps trailing zeros to count:
    if (!is.character(x)) {
      abort_in_export(
        "`x` is {an_a_type(x)}.",
        "x" = "If `digits` is not specified, `x` must be a string."
      )
    }
    digits <- decimal_places(x)
  }

  x_num <- as.numeric(x)

  # Recycle explicitly: `paste0()` could make the columns longer than `x`,
  # i.e. a malformed tibble. An empty argument means no rows at all:
  lengths_in <- c(
    length(x_num),
    length(rounding),
    length(digits),
    length(threshold),
    length(symmetric)
  )
  n_out <- if (any(lengths_in == 0L)) 0L else max(lengths_in)

  recycle <- function(value) rep_len(value, n_out)
  x_out <- recycle(x)
  x_num <- recycle(x_num)
  rounding <- recycle(rounding)
  digits <- recycle(digits)
  threshold <- recycle(threshold)
  symmetric <- recycle(symmetric)

  # With `digits` given, `x` must fit into it: the bounds are `x` plus whole
  # units of the next decimal place, so `unround(1.234, digits = 1)` would give
  # the bounds of nothing at all. Trailing zeros are fine (`"4.50"` at one
  # decimal place is 4.5), so the test is the two-step one of
  # `check_newly_numeric()`: `round()` decides, and the expensive decimal count
  # runs only on what it leaves open. `round()` rejects a zero-length `digits`,
  # hence the guard.
  too_many <- rep(FALSE, n_out)
  if (digits_given && n_out > 0L) {
    unsettled <-
      !is.na(x_num) &
      !is.na(digits) &
      x_num != round(x_num, digits)
    if (any(unsettled)) {
      too_many[unsettled] <-
        decimal_places(x_out[unsettled]) > digits[unsettled]
    }
  }

  if (any(too_many)) {
    decimals_x <- decimal_places(x_out[too_many])
    abort_in_export(
      "`x` has more decimal places than `digits` states.",
      "x" = "`digits` is {utils::head(digits[too_many], 3L)} for \\
      {utils::head(x_out[too_many], 3L)}, which {?has/have} \\
      {utils::head(decimals_x, 3L)}.",
      "i" = "`digits` is meant for helper use, where the decimal count is \\
      known. Leave it `NULL` to count the decimal places from a string `x`."
    )
  }

  # `sum_range()` and the consistency tests built on it use the same helper, so
  # they all agree on the bounds and on what each setting means:
  bounds <- lapply(seq_len(n_out), function(i) {
    bound_numerators(
      x_num = x_num[i],
      digits = digits[i],
      rounding = rounding[i],
      threshold = threshold[i],
      symmetric = symmetric[i]
    )
  })

  # `NULL` where the bounds are undefined, i.e. for a missing `x`:
  extract <- function(name, na_value) {
    vapply(
      bounds,
      function(b) if (is.null(b)) na_value else b[[name]],
      vector(mode = typeof(na_value), length = 1L),
      USE.NAMES = FALSE
    )
  }

  denom <- extract("denom", NA_real_)
  lower <- extract("lower", NA_real_) / denom
  upper <- extract("upper", NA_real_) / denom
  incl_lower <- extract("incl_lower", NA)
  incl_upper <- extract("incl_upper", NA)

  sign_lower <- dplyr::if_else(incl_lower, "<=", "<")
  sign_upper <- dplyr::if_else(incl_upper, "<=", "<")

  tibble::new_tibble(
    list(
      # fmt: skip
      range = paste0(
        lower, " ", sign_lower, " x(", x_out, ") ", sign_upper, " ", upper
      ),
      rounding = rounding,
      lower = lower,
      incl_lower = incl_lower,
      x = x_out,
      incl_upper = incl_upper,
      upper = upper
    ),
    nrow = n_out,
    class = NULL
  )
}
