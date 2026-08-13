# Integer offsets of the lower and upper rounding bounds from `x_num`, measured
# in units of `1 / 10^(digits + 1)`, plus the inclusivity of each bound. This is
# the single source of truth for rounding bounds in the package: `unround()` and
# `bound_numerators()` both derive their ranges from it, and so do the
# consistency tests in scrutiny that call `bound_numerators()`. The offsets
# follow the table in the `Rounding` section of `unround()`'s documentation,
# extended by the three compound rounding methods: their bounds are the union of
# the bounds of the two constituent methods, and since both constituents include
# `x_num` itself, that union is again a single interval.
#
# Each bound is inclusive or exclusive exactly as the corresponding rounding
# function in reround.R behaves at that bound -- e.g. `"up"` excludes its upper
# bound because a value at the midpoint rounds up, i.e. away from `x_num`, and
# `"ceiling"` excludes its lower bound because a value there ceilings to `x_num
# - 1` unit.
#
# `"even"` is the one method whose bounds cannot be pinned down: `base::round()`
# breaks midpoint ties by the parity of the preceding digit, and whether a tie
# occurs at all depends on the binary representation of the value. Both of its
# bounds are therefore treated as inclusive, which can only make a consistency
# test too permissive, never too strict -- the safe direction for an
# error-detection tool.
#
# `threshold` deliberately plays no role for `"up_or_down"`, `"up"`, and
# `"down"`, matching `round_up()` and `round_down()`, which round from a fixed
# 5. The `"*_from"` methods are the parameterized ones.
#
# Returns a list of four elements -- lower offset, upper offset, `incl_lower`,
# `incl_upper` -- all four of them `NA` if `x_num` is missing, or `NULL` if
# `rounding` is not a known method.

rounding_offsets <- function(rounding, threshold, x_num, symmetric = FALSE) {
  # A missing value has no sign, and the branches below need one: `"trunc"` and
  # `"anti_trunc"` have different bounds on either side of zero, and `symmetric`
  # mirrors the methods it applies to. Standing in a positive number keeps
  # `rounding` validated the way it is for any other value -- an unknown method
  # is an input error whatever `x_num` is -- and the offsets it yields are
  # discarded at the end. A missing value is undecidable, not a value whose
  # bounds are known:
  x_missing <- is.na(x_num)
  if (x_missing) {
    x_num <- 1
  }

  # A `"ties_*"` string names a complete tie-breaking procedure, so it stands in
  # for a `rounding` and a `symmetric` together. `reround()` resolves it through
  # the same table, so the bounds below stay in step with the rounding functions
  # they invert:
  spec <- resolve_ties_rounding(rounding, symmetric)
  rounding <- spec$rounding
  symmetric <- spec$symmetric

  # The parameterized methods are the ones that `threshold` applies to, so they
  # are the ones that validate it -- as in `reround()`, and for the same reason:
  # a threshold outside `(0, 10)` makes one of the two directions unreachable,
  # and the offsets below would encode that silently:
  if (rounding %in% c("up_from", "down_from", "up_from_or_down_from")) {
    check_threshold_valid(threshold)
  }

  # With `symmetric`, the rounding of a negative number mirrors that of its
  # absolute value, which is precisely what the opposite method does to a
  # negative number anyway. Swapping the method here is therefore enough:
  if (symmetric && x_num < 0) {
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

  # Rounding with truncation and "anti-truncation" depends on the sign of the
  # input number:

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
    # it, so it takes those bounds. At zero it is neither: every non-zero value,
    # however small, is taken away from zero to the next step out, so the only
    # value reported as zero is zero itself. That degenerate range is a real
    # answer rather than a missing one -- a mean reported as 0.00 under this
    # method really does pin the sum to exactly 0.
    offsets <- if (x_num > 0) {
      list(-10, 0,   FALSE, TRUE)
    } else if (x_num < 0) {
      list(0,   10,  TRUE,  FALSE)
    } else {
      list(0,   0,   TRUE,  TRUE)
    }
  } else if (rounding == "up_from_or_down_from") {
    # The union of the two constituent intervals. Which one supplies each
    # endpoint -- and hence whether that endpoint is inclusive -- depends on
    # `threshold`; on a tie, the inclusive constituent wins:
    lower_up   <- threshold - 10
    lower_down <- -threshold
    upper_up   <- threshold
    upper_down <- 10 - threshold
    offsets <- list(
      min(lower_up, lower_down),
      max(upper_up, upper_down),
      lower_up <= lower_down,   # `"up_from"` includes its lower bound
      upper_down >= upper_up    # `"down_from"` includes its upper bound
    )
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
      "down_from"            = list(-threshold,     10 - threshold, FALSE,      TRUE),
      return(NULL)
    )
  }

  # At zero, the mirroring happens inside the interval rather than beside it:
  # the negative half of the interval is the reflection of the positive half, so
  # both ends behave like the upper end does for a positive number.

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
#'   This is what [`scrutiny::grim()`] and [`scrutiny::grimmer()`] derive their
#'   candidate ranges from, which is why those tests and `unround()` always
#'   agree on what the bounds of a rounded number are.
#'
#'   If `threshold` is fractional, the numerators and the denominator are both
#'   scaled up by a power of ten until they are whole numbers again.
#'
#' @param x_num Numeric (length 1). The rounded number.
#' @param digits Integer (length 1). Number of decimal places in `x_num`.
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
#'   is the case only for a missing `x_num`:
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
    cli::cli_abort(c(
      "`rounding` must be one of the designated string values.",
      "x" = "It is {wrong_spec_string(rounding)}.",
      "i" = "See `vignette(\"rounding-options\")`."
    ))
  }

  if (anyNA(offsets)) {
    return(NULL)
  }

  # `threshold` is documented as an integer but not enforced to be one. If it is
  # fractional, the offsets are scaled up by a power of ten (along with the
  # denominator) until they are whole numbers again. If no such power is found
  # within a sensible range, the arithmetic downstream silently degrades to
  # floating point:
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


#' Reconstruct rounding bounds
#'
#' @description `unround()` takes a rounded number and returns the range of the
#'   original value: lower and upper bounds for the hypothetical earlier number
#'   that was later rounded to the input number. It also displays a range with
#'   inequation signs, showing whether the bounds are inclusive or not.
#'
#'   By default, the presumed rounding method is rounding up (or down) from 5.
#'   See the `Rounding` section for other methods.

#' @details The function is vectorized over `x` and `rounding`. This can be
#'   useful to unround multiple numbers at once, or to check how a single number
#'   is unrounded with different assumed rounding methods.
#'
#'   If both vectors have a length greater than 1, it must be the same
#'   length. However, this will pair numbers with rounding methods, which can be
#'   confusing. It is recommended that at least one of these input vectors has
#'   length 1.
#'
#'   Why does `x` need to be a string if `digits` is not specified? In that
#'   case, `unround()` must count decimal places by itself. If `x` then was
#'   numeric, it wouldn't have any trailing zeros because these get dropped from
#'   numerics.
#'
#'   Trailing zeros are as important for reconstructing boundary values as any
#'   other trailing digits would be. Strings don't drop trailing zeros, so they
#'   are used instead.

#' @section Rounding: Depending on how `x` was rounded, the boundary values can
#'   be inclusive or exclusive. The `incl_lower` and `incl_upper` columns in the
#'   resulting tibble are `TRUE` in the first case and `FALSE` in the second.
#'   The `range` column reflects this with equation and inequation signs.
#'
#'   However, these ranges are based on assumptions about the way `x` was
#'   rounded. Set `rounding` to the rounding method that hypothetically lead to
#'   `x`:
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
#'   | `"even"`                               | `lower <= x <= upper`        |
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
#'   | `"up_from_or_down_from"`               | (depends on `threshold`)     |
#'
#'   The bounds come from the same internal machinery that [`scrutiny::grim()`]
#'   and [`scrutiny::grimmer()`] use to derive their candidate ranges, so
#'   `unround()` accepts exactly the rounding methods those tests do, and
#'   `threshold` and `symmetric` mean the same thing everywhere. See
#'   [`bound_numerators()`] for the exact-integer form of the same bounds.
#'
#'   The four `"ties_*"` methods each name a complete tie-breaking procedure,
#'   so they say by themselves what `rounding` and `symmetric` say together:
#'   `"ties_up"` is `"up"` with `symmetric = FALSE`, `"ties_away"` is `"up"`
#'   with `symmetric = TRUE`, and likewise for `"ties_down"` and `"ties_zero"`.
#'   `symmetric` is not consulted for them. See [`round_ties_up()`].
#'
#'   Note that `threshold` applies only to `"up_from"`, `"down_from"`, and
#'   `"up_from_or_down_from"`. The plain `"up"`, `"down"`, and `"up_or_down"`
#'   methods round from a fixed 5 -- see [`round_up()`] -- so their bounds do
#'   not depend on it.
#'
#' Base R's own `round()` (R version >= 4.0.0), referenced by `rounding =
#' "even"`, is reconstructed in the same way as `"up_or_down"`. Whether its
#' boundary values are really inclusive is hard to predict: `round()` breaks
#' midpoint ties by the parity of the preceding digit, and whether a tie occurs
#' at all depends on the binary representation of the value. Both bounds are
#' therefore reported as inclusive, which can only make a reconstructed range
#' too wide, never too narrow. That is the safe direction for error detection.

#' @param x String or numeric. Rounded number. `x` must be a string unless
#'   `digits` is specified (most likely by a function that uses `unround()` as a
#'   helper).
#' @param rounding String. Rounding method presumably used to create `x`.
#'   Default is `"up_or_down"`. For more, see section `Rounding`.
#' @param threshold Numeric. The point within a step at which rounding switches
#'   direction, in tenths of a step, for the `"up_from"`, `"down_from"`, and
#'   `"up_from_or_down_from"` methods; it must be greater than `0` and less than
#'   `10`. Other rounding methods are not affected. Default is `5`, which makes
#'   those three methods the same as `"up"`, `"down"`, and `"up_or_down"`. See
#'   [`round_up_from()`], which spells out how `round_down_from()` mirrors the
#'   threshold.
#' @param digits Integer. This argument is meant to make `unround()` more
#'   efficient to use as a helper function so that it doesn't need to
#'   redundantly count decimal places. Don't specify it otherwise. Default is
#'   `NULL`, in which case decimal places really are counted internally and `x`
#'   must be a string.
#' @param symmetric Logical. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers with `"up"`, `"down"`, `"up_from"`, or `"down_from"`
#'   mirrored that of positive numbers, so that their absolute values were
#'   always equal. Default is `FALSE`. It only ever affects ties in negative
#'   numbers, but `TRUE` is what reconstructs Excel, SAS, SPSS, and Matlab; see
#'   `vignette("rounding-options")`.
#'
#' @return A tibble with seven columns: `range`, `rounding`, `lower`,
#'   `incl_lower`, `x`, `incl_upper`, and `upper`. The `range` column is a handy
#'   representation of the information stored in the columns from `lower` to
#'   `upper`, in the same order.
#'
#' @seealso For more about rounding `"up"`, `"down"`, or to `"even"`, see
#'   [`round_up()`].
#'
#'   For more about the less likely `rounding` methods, `"ceiling"`, `"floor"`,
#'   `"trunc"`, and `"anti_trunc"`, see [`round_ceiling()`].
#'
#'   For the bounds as exact integers, see [`bound_numerators()`].
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # By default, the function assumes that `x`
#' # was either rounded up or down:
#' unround(x = "2.7")
#'
#' # If `x` was rounded up, run this:
#' unround(x = "2.7", rounding = "up")
#'
#' # Likewise with rounding down...
#' unround(x = "2.7", rounding = "down")
#'
#' # ...and with `base::round()` which, broadly
#' # speaking, rounds to the nearest even number:
#' unround(x = "2.7", rounding = "even")
#'
#' # Multiple input number-strings return
#' # multiple rows in the output data frame:
#' unround(x = c(3.6, "5.20", 5.174))

# # Full example inputs:
# x <- "2.37"
# rounding <- "up_or_down"
# threshold <- 5
# digits <- NULL

unround <- function(
  x,
  rounding = "up_or_down",
  threshold = 5,
  digits = NULL,
  symmetric = FALSE
) {
  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(x, rounding))

  # The number of decimal places might be given from within another function via
  # the `digits` argument. Otherwise -- if `digits` is not specified, and
  # therefore `NULL` -- the `x` argument must be a string so that decimal places
  # can be counted accurately (cf. trailing zeros), which is then done:
  if (is.null(digits)) {
    if (!is.character(x)) {
      cli::cli_abort(c(
        "`x` is {an_a_type(x)}.",
        "x" = "If `digits` is not specified, `x` must be a string."
      ))
    }
    digits <- decimal_places(x)
  }

  # The bound helpers operate on the numeric value of `x`:
  x_num <- as.numeric(x)

  # Every argument is vectorized, and they may have different lengths -- a
  # single `x` with five `digits` values is as meaningful as the reverse.
  # Recycle them all to a common length so that each row of the output describes
  # one complete combination. (Before this was done explicitly, the output
  # tibble kept the length of `x` as its row count while its columns took
  # whatever length `paste0()` recycling produced, which could yield a malformed
  # tibble.)
  n_out <- max(
    length(x_num),
    length(rounding),
    length(digits),
    length(threshold),
    length(symmetric)
  )
  recycle <- function(value) rep_len(value, n_out)
  x_out <- recycle(x)
  x_num <- recycle(x_num)
  rounding <- recycle(rounding)
  digits <- recycle(digits)
  threshold <- recycle(threshold)
  symmetric <- recycle(symmetric)

  # Determine the boundary values and whether they are inclusive, going by the
  # `rounding` argument. `bound_numerators()` is the same helper that scrutiny's
  # GRIM and GRIMMER derive their candidate ranges from, so all of them agree on
  # what the bounds of a rounded number are, on which rounding methods exist,
  # and on what `threshold` and `symmetric` mean. It expresses each bound as an
  # exact integer numerator over a common denominator; dividing recovers the
  # boundary value itself:
  bounds <- lapply(seq_len(n_out), function(i) {
    bound_numerators(
      x_num = x_num[i],
      digits = digits[i],
      rounding = rounding[i],
      threshold = threshold[i],
      symmetric = symmetric[i]
    )
  })

  # `bound_numerators()` returns `NULL` where the bounds are undefined, which is
  # the case for a missing `x`:
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

  sign_lower <- ifelse(incl_lower, "<=", "<")
  sign_upper <- ifelse(incl_upper, "<=", "<")

  # Return a tibble that displays the range with its appropriate signs and
  # includes all the results that constitute the range
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
