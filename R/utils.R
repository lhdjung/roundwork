# Do NOT export any of these! ---------------------------------------------

#' Write "an" or "a", depending on the next word
#'
#' @param x String. A string value that ends on a vowel letter returns `"an"`;
#'   else, it returns `"a"`.
#'
#' @return String.
#'
#' @noRd
an_a <- function(x) {
  dplyr::if_else(stringr::str_detect(x, "^[aeiou]"), "an", "a")
}


#' Prefix an object's type with "an" or "a"
#'
#' This uses `an_a()` to prepend the type of `x` with "an" or "a". Because the
#' function meant to be used in messages, it replaces "double" by "double
#' (numeric value)" and "character" by "string".
#'
#' @param x Any object.
#'
#' @return String.
#'
#' @noRd
an_a_type <- function(x) {
  type <- typeof(x)
  if (type == "double") {
    type <- "double (numeric value)"
  } else if (type == "character") {
    type <- "string"
  }
  paste(an_a(typeof(x)), type)
}


#' Mark a string as wrong
#'
#' @param x Object that should have been a string (it isn't; that's why the
#'   function is called.)
#'
#' @return String.
#'
#' @noRd
wrong_spec_string <- function(x) {
  if (is.character(x)) {
    paste0("\"", x, "\"")
  } else {
    paste0("`", x, "` (not a string)")
  }
}


#' Wrap into backticks
#'
#' For error messages and similar.
#'
#' @param x String (or coercible to string).
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_backticks <- function(x) {
  paste0("`", x, "`")
}


#' Check whether numbers are whole
#'
#' @description For each element of a numeric vector `x`, `is_whole_number()`
#'   checks whether that element is a whole number.
#'
#'   This is not the same as the integer data type, so doubles and integers are
#'   tested the same way. See the note in `?integer`. To test if R itself
#'   considers a vector integer-like, use `rlang::is_integerish()` instead.
#'
#' @param x Numeric.
#' @param tolerance Numeric. Any difference between `x` and a truncated version
#'   of `x` less than `tolerance` (in the absolute value) will be ignored. The
#'   default is close to `1 / (10 ^ 8)`. This avoids errors due to spurious
#'   precision in floating-point arithmetic.
#'
#' @return Logical vector of the same length as `x`.
#'
#' @details This function was adapted (with naming modifications) from the
#'   examples of `?integer`, where a very similar function is called
#'   `is.wholenumber()`.
#'
#' @author R Core Team, Lukas Jung
#'
#' @noRd
is_whole_number <- function(x, tolerance = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tolerance
}


#' Check whether lengths are congruent
#'
#' `check_lengths_congruent()` is called within a function `f()` and takes a
#' list of arguments to `f()` supplied by the user (`var_list`). It checks if
#' two or more of those arguments have lengths that are greater than 1.
#'
#' If at least two of these lengths are also different from each other and the
#' `error` argument is `TRUE` (the default), the function will throw a precisely
#' informative error. If they have the same > 1 length and the `warn` argument
#' is `TRUE` (the default), there will be an informative warning.
#'
#' The only dependencies of this function are {rlang} and {cli}. As these are
#' tidyverse backend packages that most users have installed already, the
#' function might conceivably be used more widely.
#'
#' @param var_list List of variables that were passed to the enclosing function
#'   as arguments.
#' @param error Logical (length 1). Should an error be thrown if lengths are not
#'   congruent? Default is `TRUE`.
#' @param warn Logical (length 1). If no error is thrown, should a warning be
#'   issued if appropriate (see description)? Default is `TRUE`.
#'
#' @return No return value; might throw error or warning.
#'
#' @noRd
check_lengths_congruent <- function(var_list, error = TRUE, warn = TRUE) {
  var_names <- rlang::enexprs(var_list)
  var_lengths <- lengths(var_list)
  var_list_gt1 <- var_list[var_lengths > 1L]

  # Condition of checking for error and warning:
  if (length(var_list_gt1) > 1L) {
    var_names <- var_names[[1L]][-1L]
    var_names <- as.character(var_names)
    var_names_gt1 <- var_names[var_lengths > 1L]
    vnames_gt1_all <- var_names_gt1 # for the warning

    # Two arguments of the same length are congruent, so only one of each
    # distinct length needs to survive into the error condition below. The
    # duplicates have to be found among the lengths greater than 1, not among
    # all of them: `duplicated(var_lengths)` is as long as `var_list`, and
    # indexing the shorter `var_list_gt1` with it dropped whichever elements
    # happened to line up with a repeated length-1 argument -- usually none of
    # them, so the deduplication did nothing at all. Two arguments that were
    # both length 2 then counted as two distinct lengths and raised an error
    # about having to be the same length, which they already were.
    length_dup <- duplicated(var_lengths[var_lengths > 1L])
    var_list_gt1 <- var_list_gt1[!length_dup]
    var_names_gt1 <- var_names_gt1[!length_dup]

    # Error condition, checking if there is more than one element of `var_list`
    # with a unique length greater than one (the duplicated lengths were
    # filtered out from `var_list_gt1` right above):
    if (error && (length(var_list_gt1) > 1L)) {
      x <- var_list_gt1[[1L]]
      y <- var_list_gt1[[2L]]
      x_name <- var_names_gt1[[1L]]
      y_name <- var_names_gt1[[2L]]

      residues_names <- var_names[!var_names %in% c(x_name, y_name)]

      msg_error <- c(
        "`{x_name}` and `{y_name}` must have the same length \\
        unless either has length 1.",
        "*" = "`{x_name}` has length {length(x)}.",
        "*" = "`{y_name}` has length {length(y)}."
      )

      # Append-to-error-message condition:
      if (length(residues_names) > 0L) {
        residues_names <- paste0("`", residues_names, "`")
        msg_error <- append(
          msg_error,
          c("i" = "This also applies to {residues_names}.")
        )
      }

      # Throw error:
      cli::cli_abort(msg_error, call = rlang::caller_env())
    }

    # Warning condition, triggered if more than one element of `var_list` has
    # length > 1, it's the same length for all (hence no error), and the `warn`
    # argument is `TRUE` (the default):
    if (warn) {
      x_name <- vnames_gt1_all[[1L]]
      y_name <- vnames_gt1_all[[2L]]

      l_vnames <- length(vnames_gt1_all)

      if (l_vnames > 2L) {
        msg_example <- ", for example,"
      } else {
        msg_example <- ""
      }

      if (l_vnames == 2L) {
        one_both_all <- "one or both"
        var_count <- ""
      } else {
        one_both_all <- "all (or all but one)"
        var_count <- l_vnames
      }

      vnames_gt1_all <- paste0("`", vnames_gt1_all, "`")

      # Throw warning:
      cli::cli_warn(c(
        "Values of {vnames_gt1_all} get paired.",
        "!" = "Are you sure that{msg_example} each `{x_name}` value \\
        should correspond to a different `{y_name}` value?",
        ">" = "It might be better if {one_both_all} of these {var_count} \\
        variables have length 1."
      ))
    }
  }
}


#' Check that a rounding threshold is usable
#'
#' @description `check_threshold_valid()` is called within curly braces inside
#'   of the switch statement in `reconstruct_rounded_numbers_scalar()`, and from
#'   `rounding_offsets()`, if `rounding` includes `"_from"` and therefore
#'   depends on `threshold`.
#'
#'   A threshold is the point within a step at which rounding switches
#'   direction, so it has to lie strictly inside the step: at `0` or `10`, one
#'   of the two directions can never be taken, which silently turns the method
#'   into `"ceiling"`-like or `"floor"`-like behavior.
#'
#'   Up to roundwork 0.0.1, the check here was a different one
#'   (`check_threshold_specified()`): it threw an error if `threshold` was `5`,
#'   on the theory that a threshold of `5` must be the argument's default value
#'   showing through, and that the user meant to specify something else. That
#'   conflated "unspecified" with "specified as 5" -- any caller computing a
#'   threshold and passing it on failed spuriously at exactly the most common
#'   value -- and `"up_from"` with a threshold of `5` is simply `"up"`, which is
#'   a correct answer rather than an error.
#'
#' @param threshold The `threshold` argument of the calling function.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_threshold_valid <- function(threshold) {
  if (
    length(threshold) != 1L ||
      !is.numeric(threshold) ||
      !is.finite(threshold) ||
      threshold <= 0 ||
      threshold >= 10
  ) {
    cli::cli_abort(
      message = c(
        "`threshold` must be a single number greater than 0 and less than 10.",
        "x" = "It is {wrong_spec_string(threshold)}.",
        "i" = "It is the point within a step at which rounding switches \\
        direction, so both directions have to remain possible.",
        "i" = "With `rounding` set to \"up_from\", \"down_from\", or \\
        \"up_from_or_down_from\", `x` is rounded up or down from `threshold` \\
        instead of from 5."
      ),
      call = rlang::caller_env()
    )
  }
}


# Shifting a number by `digits` decimal places is not exact in floating point:
# `0.28 * 100` is 28.000000000000004, and `0.29 * 100` is 28.999999999999996.
# Rounding the shifted value away from the number it is meant to be would then
# move it a whole step -- `ceiling(0.28 * 100) / 100` would be 0.29 rather than
# 0.28. Every rounding function in round.R and round-ceil-floor.R therefore
# nudges the shifted value by this tolerance before rounding it: the `round_*()`
# functions of round-ceil-floor.R add or subtract it directly, and
# `round_up_from()` and `round_down_from()` fold it into `tie_offset()`. It is
# far smaller than any difference a reported value could meaningfully express,
# so it only ever absorbs representation error.
#
# `unround()` reports bounds that assume exactly this tolerance, and the
# property test in test-unround.R checks that the two agree, so all three files
# have to stay with the one constant.
#
# The tolerance is absolute, so it has a domain of validity: representation
# error in `x * 10^digits` grows with the magnitude of that product (roughly
# `|x| * 10^digits * 2.2e-16`), whereas the nudge is fixed. Up to about
# `|x * 10^digits| = 1e7` the nudge dominates by orders of magnitude; far beyond
# that, a value sitting exactly on a rounding boundary may go either way. Means,
# SDs, and percentages with a few decimal places are nowhere near that.

rounding_tolerance <- .Machine$double.eps^0.5 / 10


# `round_up_from()` and `round_down_from()` both shift the scaled value so that
# `floor()` or `ceiling()` cuts it at `threshold` rather than at 5, and both
# nudge it by `rounding_tolerance` beforehand. This is the amount they add or
# subtract.
#
# Before roundwork 0.0.1 the nudge was written there as `threshold -
# .Machine$double.eps^0.5`, which the `/ 10` below turns into the very same
# additive `rounding_tolerance`. Everything depended on that equality, since
# `unround()` reports bounds that assume one shared tolerance, but it was not
# stated anywhere.

tie_offset <- function(threshold) {
  1 - (threshold / 10) + rounding_tolerance
}


# The `"ties_*"` rounding strings each name a complete tie-breaking procedure,
# so one of them says by itself what `rounding` plus `symmetric` says together.
# `reround()` and `rounding_offsets()` both resolve them through this one table,
# so the forward functions and the bounds can't come to disagree about what a
# name means.
#
# `symmetric` is deliberately not consulted for them. The procedure is already
# fully determined by the name, and a `"ties_away"` that a separate argument
# could turn into something else would defeat the point of naming it.

# fmt: skip
ties_methods <- list(
  ties_up   = list(rounding = "up",   symmetric = FALSE),  # toward +Inf
  ties_down = list(rounding = "down", symmetric = FALSE),  # toward -Inf
  ties_away = list(rounding = "up",   symmetric = TRUE),   # roundTiesToAway
  ties_zero = list(rounding = "down", symmetric = TRUE)    # toward zero
)


resolve_ties_rounding <- function(rounding, symmetric) {
  # `[[` on a list matches exactly, so a `rounding` of "up" is not caught by
  # "ties_up" here:
  spec <- ties_methods[[rounding]]
  if (is.null(spec)) {
    list(rounding = rounding, symmetric = symmetric)
  } else {
    spec
  }
}


# The two procedures that a compound rounding method is made of, or the method
# itself if it is not a compound one. `reround()` returns one value per input
# value for a single procedure and two -- interleaved -- for a compound one, so
# a caller that wants to keep working on each of those branches separately needs
# to know which procedure produced it.

rounding_constituents <- function(rounding) {
  # fmt: skip
  switch(
    rounding,
    "up_or_down"           = c("up", "down"),
    "up_from_or_down_from" = c("up_from", "down_from"),
    "ceiling_or_floor"     = c("ceiling", "floor"),
    rounding
  )
}


# Give `value` -- typically derived from `abs(x)` -- the sign of `x`, so that
# rounding a negative number mirrors the rounding of its absolute value. Zero
# and positive values keep `value` as it is; `NA` and `NaN` pass through.
#
# `dplyr::if_else()` would say the same thing, but these are the package's
# innermost primitives: `round_trunc()`, `anti_trunc()`, and the `symmetric`
# branches of `round_up_from()` and `round_down_from()` run once per candidate
# value inside GRIMMER's loop over sums of squares, which scrutiny's seq mappers
# multiply by hundreds of rows.

restore_sign <- function(value, x) {
  value * (1 - 2 * (x < 0))
}


# Counts decimal places from the string representation, so that trailing zeros
# are not lost: `decimal_places("3.70")` is 2, whereas the numeric `3.70` has
# already dropped its trailing zero and gives 1. This is why `unround()`'s `x`
# must be a string unless `digits` is given.
#
# Copied from scrutiny, where it is exported as `decimal_places()`. It is just a
# helper here, not part of roundwork's API.

decimal_places <- function(x, sep = "\\.") {
  pieces <- stringr::str_split(stringr::str_trim(x), sep, n = 2L)
  vapply(
    pieces,
    function(p) {
      if (anyNA(p)) {
        NA_integer_
      } else if (length(p) == 1L) {
        0L
      } else {
        stringr::str_length(p[[2L]])
      }
    },
    integer(1L)
  )
}
