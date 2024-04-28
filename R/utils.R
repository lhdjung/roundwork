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
  var_lengths <- vapply(var_list, length, integer(1L), USE.NAMES = FALSE)
  var_list_gt1 <- var_list[var_lengths > 1L]

  # Condition of checking for error and warning:
  if (length(var_list_gt1) > 1L) {
    var_names <- var_names[[1L]][-1L]
    var_names <- as.character(var_names)
    var_names_gt1 <- var_names[var_lengths > 1L]
    vnames_gt1_all <- var_names_gt1   # for the warning

    length_dup <- duplicated(var_lengths)
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
          msg_error, c("i" = "This also applies to {residues_names}.")
        )
      }

      # Throw error:
      cli::cli_abort(msg_error)
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



#' Check that `rounding` values for two procedures are not mixed
#'
#' @description In `reround()` and the many functions that call it internally,
#'   valid specifications of the `rounding` argument include the following:
#'
#' - `"up_or_down"` (the default)
#' - `"up_from_or_down_from"`
#' - `"ceiling_or_floor"`
#'
#'   If `rounding` includes any of these, it must not include any other values.
#'   `check_rounding_singular()` is called within `reround()` if `rounding` has
#'   length > 1 and throws an error if any of these strings are part of it.
#'
#' @param rounding String (length > 1).
#' @param bad String (length 1). Any of `"up_or_down"` etc.
#' @param good1,good2 String (length 1). Two singlular rounding procedures that
#'   are combined in `bad`, and that can instead be specified individually;
#'   like, e.g., `rounding = c("up", "down")`.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_rounding_singular <- function(rounding, bad, good1, good2) {
  if (any(bad == rounding)) {
    cli::cli_abort(c(
      "!" = "If `rounding` has length > 1, only single rounding procedures \\
      are supported, such as \"{good1}\" and \"{good2}\".",
      "x" = "`rounding` was given as \"{bad}\" plus others.",
      "i" = "You can still concatenate multiple of them; just leave out \\
      those with \"_or_\"."
    ))
  }
}



#' Check whether a rounding threshold was specified
#'
#' @description `check_threshold_specified()` is called within curly braces
#'   inside of the switch statement in `reconstruct_rounded_numbers_scalar()` if
#'   `rounding` includes `"_from"` and therefore requires specification of a
#'   threshold.
#'
#'   It should always be followed by the respective rounding function.
#'
#' @param rounding_threshold
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_threshold_specified <- function(threshold) {
  if (threshold == 5) {
    cli::cli_abort(c(
      "You need to specify `threshold`.",
      "x" = "If `rounding` is \"up_from\", \"down_from\", or \\
      \"up_from_or_down_from\", set `threshold` to a number \\
      other than 5. The `x` argument will then be rounded up or down from \\
      that number.",
      "i" = "To round up or down from 5, just set `rounding` to \\
      \"up\", \"down\", or \"up_or_down\" instead."
    ))
  }
}



# Just used as a helper here. Copied from scrutiny now, but it might move to
# another package in the future.
decimal_places <- function(x, sep = "\\.") {
  out <- stringr::str_split(stringr::str_trim(x), sep, 2L)
  out <- purrr::modify_if(out, !is.na(out), stringr::str_length)
  out <- purrr::modify_if(
    out, function(x) length(x) == 1L && !is.na(x), function(x) 0L
  )

  as.integer(unlist(
    purrr::map_if(out, function(x) length(x) > 1L, `[`, 2L),
    use.names = FALSE
  ))
}

