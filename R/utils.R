# Do NOT export any of these! ---------------------------------------------

wrong_spec_string <- function(x) {
  if (is.character(x)) {
    wrap_in_quotes_or_backticks(x)
  } else {
    paste0("`", x, "` (not a string)")
  }
}


an_a <- function(x) {
  dplyr::if_else(
    stringr::str_detect(x, "^[aeiou]"),
    "an",
    "a"
  )
}


an_a_type <- function(x) {
  type <- typeof(x)

  if (is.factor(x)) {
    type <- "factor"
  } else if (type == "double") {
    type <- "double (numeric value)"
  } else if (type == "character") {
    type <- "string"
  }

  article <- an_a(type)
  paste(article, type)
}


# `is_decidable_n_items()` and `check_newly_numeric()` write this test out
# rather than calling it, because they run once per row; keep them in step.
is_whole_number <- function(x, tolerance = WHOLE_NUMBER_TOLERANCE) {
  abs(x - round(x)) < tolerance
}


check_lengths_congruent <- function(var_list, error = TRUE, warn = TRUE) {
  var_lengths <- lengths(var_list)
  var_list_gt1 <- var_list[var_lengths > 1L]

  if (length(var_list_gt1) > 1L) {
    # Capturing the argument names costs some thirty times everything else here,
    # and only this branch needs them. `reround()` calls this every time, so it
    # must not happen unconditionally.
    #
    # `substitute()` rather than `enexprs()`: `lengths()` above has forced the
    # promise, and `enexprs()` then returns the *values*, losing the names.
    # `substitute()` reads `PRCODE`, which survives forcing.
    var_names <- as.list(substitute(var_list))[-1L]
    var_names <- as.character(var_names)
    var_names_gt1 <- var_names[var_lengths > 1L]
    vnames_gt1_all <- var_names_gt1 # for the warning

    # Only one argument of each distinct length needs to reach the error
    # condition. Deduplicate among the lengths greater than 1, so that the
    # index lines up with `var_list_gt1`:
    length_dup <- duplicated(var_lengths[var_lengths > 1L])
    var_list_gt1 <- var_list_gt1[!length_dup]
    var_names_gt1 <- var_names_gt1[!length_dup]

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

      if (length(residues_names) > 0L) {
        residues_names <- paste0("`", residues_names, "`")
        msg_error <- append(
          msg_error,
          c("i" = "This also applies to {residues_names}.")
        )
      }

      abort_in_export(msg_error)
    }

    # Same length greater than 1 for all of them:
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


check_type <- function(x, t) {
  # A factor's type is `"integer"`, but its values are level codes, not the
  # numbers it shows:
  if (is.factor(x) || !any(typeof(x) == t)) {
    msg_name <- deparse(substitute(x))
    if (length(t) == 1L) {
      msg_object <- "be of type"
    } else {
      msg_object <- "be one of these types:"
    }

    abort_in_export(
      "!" = "`{msg_name}` must {msg_object} {t}.",
      "x" = "It is {an_a_type(x)}."
    )
  }
}


check_digits_whole <- function(digits, name = "digits") {
  if (!is.numeric(digits) && !(is.logical(digits) && all(is.na(digits)))) {
    abort_in_export(
      "`{name}` must be whole numbers.",
      "x" = "It is {.obj_type_friendly {digits}}."
    )
  }

  # The cheap, normal case. `NaN` passes with the other missing values. 308 is
  # the greatest power of ten a double can hold:
  ok <-
    is.na(digits) |
    (abs(digits) <= 308 &
      abs(digits - round(digits)) < WHOLE_NUMBER_TOLERANCE)

  if (all(ok)) {
    return(NULL)
  }

  # Failure path. Not being whole is reported first; an infinity counts as not
  # whole:
  offenders <- digits[!ok]
  not_whole <- offenders[
    !is.finite(offenders) |
      abs(offenders - round(offenders)) >= WHOLE_NUMBER_TOLERANCE
  ]

  if (length(not_whole) > 0L) {
    abort_in_export(
      "`{name}` must be whole numbers.",
      "x" = "It has {length(not_whole)} value{?s} that {?is/are} not: \\
        {not_whole}.",
      "i" = "Each value is a number of decimal places, so a fractional one \\
        would scale `x` by a non-power of ten and return a number on no \\
        decimal grid."
    )
  }

  abort_in_export(
    "`{name}` must be between -308 and 308.",
    "x" = "It has {length(offenders)} value{?s} that {?is/are} not: \\
      {offenders}.",
    "i" = "Beyond that, `10^digits` is not a finite, non-zero number, so \\
      `x` could not be scaled by it."
  )
}


check_threshold_valid <- function(threshold) {
  if (
    length(threshold) != 1L ||
      !is.numeric(threshold) ||
      !is.finite(threshold) ||
      threshold <= 0 ||
      threshold >= 10
  ) {
    msg_what_it_is <- if (is.numeric(threshold) && length(threshold) == 1L) {
      "It is {threshold}."
    } else {
      "It is {.obj_type_friendly {threshold}}."
    }
    abort_in_export(
      "`threshold` must be a single number greater than 0 and less than 10.",
      "x" = msg_what_it_is,
      "i" = "It is the point within a step at which rounding switches \\
        direction, so both directions have to remain possible.",
      "i" = "With `rounding` set to \"up_from\", \"down_from\", or \\
        \"up_from_or_down_from\", `x` is rounded up or down from `threshold` \\
        instead of from 5."
    )
  }
}


wrap_in_backticks <- function(x) {
  paste0("`", x, "`")
}


wrap_in_quotes_or_backticks <- function(x) {
  if (is.null(x)) {
    "`NULL`"
  } else if (is.character(x)) {
    # A missing string is no string:
    ifelse(is.na(x), "`NA`", paste0("\"", x, "\""))
  } else {
    paste0("`", x, "`")
  }
}


tie_offset_up <- function(threshold) {
  1 - (threshold / 10) + ROUNDING_TOLERANCE
}


tie_offset_down <- function(threshold) {
  (threshold / 10) + ROUNDING_TOLERANCE
}


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


check_rounding_spec_singular <- function(rounding, threshold, symmetric) {
  if (
    length(rounding) != 1L ||
      length(threshold) != 1L ||
      length(symmetric) != 1L
  ) {
    abort_in_export(
      "`rounding`, `threshold`, and `symmetric` must each have length 1.",
      "x" = "They have lengths {length(rounding)}, {length(threshold)}, \\
        and {length(symmetric)}.",
      "i" = "They describe a single rounding procedure, which is then \\
        applied to all values.",
      "i" = "To compare procedures, test once per procedure. `unround()` \\
        is vectorized over `rounding` if you need the bounds."
    )
  }

  # `resolve_ties_rounding()` indexes `TIES_METHODS` by `rounding`, and a number
  # or `TRUE` would pick an element by position, i.e., a method the caller
  # never named:
  if (!is.character(rounding) || is.na(rounding)) {
    abort_in_export(
      "`rounding` must be a string.",
      "x" = "It is {.obj_type_friendly {rounding}}.",
      "i" = "See `vignette(\"rounding-options\")` for the strings it \\
        can take."
    )
  }

  if (!is.logical(symmetric) || is.na(symmetric)) {
    abort_symmetric_invalid(symmetric)
  }
}


restore_sign <- function(value, x) {
  value * (1 - 2 * (x < 0))
}


decimal_places <- function(x, sep = ".") {
  sep <- sep_literal(sep)

  # Strings are matched against the tokens rather than run through
  # `as.numeric()`, which would also reject strings like `"5.30%"`. Values
  # decided here are blanked out so that nothing below meets a missing value:
  if (is.numeric(x)) {
    non_finite <- !is.finite(x)
    x <- number_to_string(x)
    # R writes a number with a point, whatever `sep` the strings use:
    sep <- "."
  } else {
    x <- trim_space(x)
    non_finite <- x %in% c(NA, "NA") | is_non_finite_token(x)
  }
  x[non_finite] <- ""

  # An exponent moves the decimal point (`"1e-05"` has five decimal places,
  # `"1.5e3"` none), so it is split off and applied to the count below. One
  # beyond the integer range makes the count `NA`:
  pos_exponent <- regexpr("[eE][+-]?[0-9]+$", x)
  has_exponent <- pos_exponent > 0L
  exponent <- integer(length(x))
  exponent[has_exponent] <- x[has_exponent] |>
    substring(pos_exponent[has_exponent] + 1L) |>
    as.integer() |>
    suppressWarnings()
  x[has_exponent] <- substr(
    x[has_exponent],
    1L,
    pos_exponent[has_exponent] - 1L
  )

  # Only the digit run right after the first separator counts: `"1.2.3"` has
  # one decimal place. `decimal_places_scalar()` takes the same steps, and a
  # generated corpus in test-decimal-places.R holds the two together:
  pos_sep <- regexpr(sep, x, fixed = TRUE)
  mantissa <- substring(x, pos_sep + nchar(sep))
  mantissa[pos_sep < 0L] <- ""
  out <- mantissa |>
    regexpr(pattern = "^[0-9]*") |>
    attr("match.length")

  out <- pmax(out - exponent, 0L)
  out[non_finite] <- NA_integer_
  out
}


# `as.character()` uses scientific notation when it is shorter (`100000` is
# `"1e+05"`), so such numbers are written out in full here, to be counted and
# compared to other spellings. Near the ends of the double range it can use all
# 15 digits where fewer stand for the same number (`1e-300` gives
# `"9.99999999999999e-301"`), so the fewest digits that round-trip are searched
# for. The common case costs one fixed-string match:
number_to_string <- function(x) {
  out <- as.character(x)
  for (i in grep("e", out, fixed = TRUE)) {
    digits <- nchar(gsub("[^0-9]", "", sub("e.*$", "", out[i])))
    if (digits >= 15L) {
      for (digits in 1:15) {
        if (as.numeric(format(x[i], digits = digits)) == x[i]) {
          break
        }
      }
    }
    out[i] <- format(
      x[i],
      digits = digits,
      scientific = FALSE,
      decimal.mark = "."
    )
  }
  out
}


# `sep` and its relatives in `restore_zeros()` are literal strings. An escaped
# character such as `"\\."`, valid when they were regular expressions, is read
# as that character.
sep_literal <- function(sep) {
  if (!is.character(sep) || length(sep) != 1L || is.na(sep)) {
    name <- deparse(substitute(sep))
    abort_in_export(
      "`{name}` must be a single string.",
      "x" = "It is {wrong_spec_string(sep)}."
    )
  }
  sub("^\\\\(.)$", "\\1", sep)
}


# `decimal_places()` and `decimal_places_scalar()` read strings through these
# two, so that they cannot drift apart.
#
# `\s` is ASCII whitespace, `"\v"` and `"\f"` included. `\p{Z}` adds Unicode
# spaces such as the no-break space, common in tables copied from PDFs.
trim_space <- function(x) {
  gsub("^[\\s\\p{Z}]+|[\\s\\p{Z}]+$", "", x, perl = TRUE)
}


# The strings `as.numeric()` reads as an infinity or `NaN`, in any letter case.
# `chartr()` folds the ASCII letters of these tokens and nothing else, so unlike
# `tolower()` it does not depend on the locale. A number too large for a double,
# such as `"1e400"`, is read as an infinity as well:
is_non_finite_token <- function(x) {
  chartr("AFINTY", "afinty", x) %in%
    NON_FINITE_TOKENS |
    is.infinite(suppressWarnings(as.numeric(x)))
}


