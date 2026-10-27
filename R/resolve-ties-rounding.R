#' Resolve a `"ties_*"` rounding string
#'
#' @description `resolve_ties_rounding()` translates one of the `"ties_*"`
#'   rounding strings into the `rounding` and `symmetric` values it stands for,
#'   such as `"ties_away"` into `rounding = "up"` and `symmetric = TRUE`. Any
#'   other `rounding` is returned unchanged, along with `symmetric`.
#'
#'   [`reround()`] and [`unround()`] resolve these strings the same way, so a
#'   function that branches on `rounding` itself can call this first to agree
#'   with them.
#'
#' @details `resolve_ties_rounding()` is a low-level primitive and does not
#'   check its input. The caller is responsible for passing a single string as
#'   `rounding` and a single `TRUE` or `FALSE` as `symmetric`. In particular, a
#'   number as `rounding` would pick a `"ties_*"` method by position.
#'
#'   Combining a `"ties_*"` string with `symmetric = TRUE` is an error, because
#'   the string already determines how negative ties are broken.
#'
#' @param rounding String (length 1). A rounding method. Not checked.
#' @param symmetric Logical (length 1). See [`round_up()`]. Not checked.
#'
#' @return A list with the elements `rounding` and `symmetric`.
#'
#' @export
#'
#' @examples
#' resolve_ties_rounding("ties_away", symmetric = FALSE)
#'
#' # Other methods pass through:
#' resolve_ties_rounding("up", symmetric = TRUE)

resolve_ties_rounding <- function(rounding, symmetric) {
  # `[[` on a list matches exactly, so a `rounding` of "up" is not caught by
  # "ties_up" here:
  spec <- TIES_METHODS[[rounding]]

  if (is.null(spec)) {
    return(list(rounding = rounding, symmetric = symmetric))
  }

  # A `"ties_*"` string already fixes the tie direction in both signs. Ignoring
  # `symmetric` would silently drop half of the request:
  # `reround(-2.5, 0, "ties_up", symmetric = TRUE)` would be `-2`, but `-3`
  # when spelled `rounding = "up"`. So it is an error naming the string the
  # caller meant.
  #
  # The test is on the value, not on `missing()`: callers such as scrutiny's
  # mappers pass `symmetric` down explicitly, so its default is the only thing
  # that can mean "not given".
  if (!isFALSE(symmetric)) {
    # fmt: skip
    msg_instead <- switch(
      rounding,
      "ties_up"   = "For ties away from zero, set `rounding` to \"ties_away\".",
      "ties_down" = "For ties toward zero, set `rounding` to \"ties_zero\".",
      "ties_away" = "\"ties_away\" is already what `symmetric = TRUE` makes \\
        \"up\" do, so drop `symmetric`.",
      "ties_zero" = "\"ties_zero\" is already what `symmetric = TRUE` makes \\
        \"down\" do, so drop `symmetric`.",
      "ties_even" = "Rounding to even is its own mirror image, so there is \\
        no tie direction left to set. Drop `symmetric`.",
      "Drop `symmetric`."
    )
    abort_in_export(
      "`symmetric` must not be `TRUE` with `rounding = \"{rounding}\"`.",
      "x" = "It is {symmetric}.",
      "i" = "A \"ties_*\" string names a complete tie-breaking procedure, \\
        so it already determines which way ties go for negative numbers.",
      "i" = msg_instead
    )
  }

  spec
}
