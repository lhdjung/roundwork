# `bound_numerators()` is the exact-integer counterpart of `unround()`. The
# sweep in test-unround.R pins the *position* and *inclusivity* of every bound
# through `unround()`; these tests pin the integer representation itself, which
# is what `sum_range()` and the consistency tests in downstream packages compute
# with.

bn <- function(x_num, digits, rounding, threshold = 5, symmetric = FALSE) {
  bound_numerators(x_num, digits, rounding, threshold, symmetric)
}

test_that("`bound_numerators()` returns exact numerators over 10^(digits + 1)", {
  b <- bn(2.7, 1L, "up_or_down")
  b |> expect_named(c("lower", "upper", "denom", "incl_lower", "incl_upper"))
  b$lower |> expect_identical(265)
  b$upper |> expect_identical(275)
  b$denom |> expect_identical(100)
  b$incl_lower |> expect_true()
  b$incl_upper |> expect_true()

  # Whole numbers everywhere, including where the floating-point product is
  # not: `0.29 * 1000` is `290.00000000000006`.
  b <- bn(0.29, 2L, "up")
  b$lower |> expect_identical(285)
  b$upper |> expect_identical(295)
  b$denom |> expect_identical(1000)

  # Large values stay exact:
  b <- bn(123456.78, 2L, "up_or_down")
  b$lower |> expect_identical(123456775)
  b$upper |> expect_identical(123456785)

  # Negative `digits` round to powers of ten; the denominator is then a
  # fraction, and the numerators are still whole:
  b <- bn(1200, -2L, "up_or_down")
  b$denom |> expect_equal(0.1)
  b$lower |> expect_identical(115)
  b$upper |> expect_identical(125)
  (b$lower / b$denom) |> expect_equal(1150)
  (b$upper / b$denom) |> expect_equal(1250)
})


test_that("`bound_numerators()` encodes the sign-dependent methods", {
  # `"up"` without `symmetric` is the same offsets in both signs:
  b <- bn(-2.7, 1L, "up")
  c(b$lower, b$upper) |> expect_identical(c(-275, -265))
  c(b$incl_lower, b$incl_upper) |> expect_identical(c(TRUE, FALSE))

  # With `symmetric`, a negative number rounds like its absolute value, so the
  # inclusivity flips:
  b <- bn(-2.7, 1L, "up", symmetric = TRUE)
  c(b$lower, b$upper) |> expect_identical(c(-275, -265))
  c(b$incl_lower, b$incl_upper) |> expect_identical(c(FALSE, TRUE))

  # At zero with `symmetric`, both ends behave like the upper end:
  b <- bn(0, 1L, "up", symmetric = TRUE)
  c(b$lower, b$upper) |> expect_identical(c(-5, 5))
  c(b$incl_lower, b$incl_upper) |> expect_identical(c(FALSE, FALSE))

  # Truncation depends on the sign, and is two-sided at zero:
  bn(2.7, 1L, "trunc")  |> call_on(\(b) c(b$lower, b$upper)) |> expect_identical(c(270, 280))
  bn(-2.7, 1L, "trunc") |> call_on(\(b) c(b$lower, b$upper)) |> expect_identical(c(-280, -270))
  bn(0, 1L, "trunc")    |> call_on(\(b) c(b$lower, b$upper)) |> expect_identical(c(-10, 10))

  # `"anti_trunc"` at zero is the single point zero, both ends inclusive:
  b <- bn(0, 2L, "anti_trunc")
  c(b$lower, b$upper) |> expect_identical(c(0, 0))
  c(b$incl_lower, b$incl_upper) |> expect_identical(c(TRUE, TRUE))
})


test_that("`bound_numerators()` scales a fractional threshold to whole numbers", {
  # `"up_from"`'s offsets are `threshold - 10` and `threshold`; at 4.5 they are
  # fractional, so numerators and denominator are scaled by 10 together:
  b <- bn(0.53, 2L, "up_from", threshold = 4.5)
  b$denom |> expect_identical(10000)
  b$lower |> expect_identical(5245)
  b$upper |> expect_identical(5345)
  (b$lower / b$denom) |> expect_equal(0.53 - 0.0055)

  # Two decimal places in the threshold need two more powers of ten:
  b <- bn(0.53, 2L, "down_from", threshold = 4.25)
  b$denom |> expect_identical(100000)
  b$lower |> expect_identical(52425)
  b$upper |> expect_identical(53425)

  # `threshold` is ignored by the fixed-5 methods, even when it is out of range:
  bn(0.53, 2L, "up", threshold = 12) |> expect_identical(bn(0.53, 2L, "up"))
  bn(0.53, 2L, "up", threshold = 0.5) |> expect_identical(bn(0.53, 2L, "up"))
})


test_that("`bound_numerators()` resolves `\"ties_*\"` strings like `reround()`", {
  for (x_num in c(2.5, -2.5, 0)) {
    bn(x_num, 0L, "ties_up")   |> expect_identical(bn(x_num, 0L, "up"))
    bn(x_num, 0L, "ties_down") |> expect_identical(bn(x_num, 0L, "down"))
    bn(x_num, 0L, "ties_away") |> expect_identical(bn(x_num, 0L, "up", symmetric = TRUE))
    bn(x_num, 0L, "ties_zero") |> expect_identical(bn(x_num, 0L, "down", symmetric = TRUE))
    bn(x_num, 0L, "ties_even") |> expect_identical(bn(x_num, 0L, "even"))
  }
})


test_that("`bound_numerators()` agrees with `unround()` everywhere (sweep)", {
  methods <- c(
    "up_or_down", "up", "down", "even", "ceiling", "floor", "ceiling_or_floor",
    "trunc", "anti_trunc", "up_from", "down_from", "up_from_or_down_from",
    "ties_up", "ties_down", "ties_away", "ties_zero", "ties_even"
  )
  n_checked <- 0L
  for (digits in c(0L, 2L)) {
    for (x_num in c(0, 1, -7, 253) / 10^digits) {
      x_str <- formatC(x_num, format = "f", digits = digits)
      for (symmetric in c(FALSE, TRUE)) {
        for (threshold in c(2.5, 5)) {
          for (m in methods) {
            if (symmetric && m %in% names(TIES_METHODS)) {
              next
            }
            label <- paste(
              m, "| x =", x_str, "| digits =", digits,
              "| symmetric =", symmetric, "| threshold =", threshold
            )
            b <- bn(x_num, digits, m, threshold, symmetric)
            u <- unround(x_str, rounding = m, threshold = threshold, digits = digits, symmetric = symmetric)
            n_checked <- n_checked + 1L

            # Whole numerators and a power-of-ten denominator:
            b$lower |> expect_identical(round(b$lower), label = paste(label, "- lower whole"))
            b$upper |> expect_identical(round(b$upper), label = paste(label, "- upper whole"))
            log10(b$denom) |> expect_equal(round(log10(b$denom)), label = paste(label, "- denom"))

            # `x` itself is a whole number of units, strictly inside or on the
            # bounds:
            x_units <- round(x_num * b$denom)
            (x_num * b$denom) |> expect_equal(x_units, label = paste(label, "- x on grid"))
            (b$lower <= x_units && x_units <= b$upper) |> expect_true(label = paste(label, "- x inside"))

            # Same bounds as `unround()`, as decimals:
            (b$lower / b$denom) |> expect_equal(u$lower, label = paste(label, "- lower"))
            (b$upper / b$denom) |> expect_equal(u$upper, label = paste(label, "- upper"))
            b$incl_lower |> expect_identical(u$incl_lower, label = paste(label, "- incl_lower"))
            b$incl_upper |> expect_identical(u$incl_upper, label = paste(label, "- incl_upper"))
          }
        }
      }
    }
  }
  n_checked |> expect_gt(400L)
})


test_that("`bound_numerators()` returns `NULL` for undefined bounds", {
  for (x_num in c(NA_real_, NaN, Inf, -Inf)) {
    bn(x_num, 2L, "up_or_down") |> expect_null()
    bn(x_num, 2L, "trunc", symmetric = TRUE) |> expect_null()
  }
  # ...but still validates the rounding specification first:
  bn(NA_real_, 2L, "nonsense") |> expect_error("designated string values")
  bn(Inf, 2L, "up_from", threshold = 0) |> expect_error("`threshold` must be")
})


test_that("`bound_numerators()` checks `rounding`, `threshold`, and `symmetric`", {
  bn(2.7, 1L, "nonsense")   |> expect_error("designated string values")
  bn(2.7, 1L, 2)            |> expect_error("must be a string")
  bn(2.7, 1L, NA_character_) |> expect_error("must be a string")
  bn(2.7, 1L, c("up", "down")) |> expect_error("must each have length 1")
  bn(2.7, 1L, "up", threshold = c(5, 6)) |> expect_error("must each have length 1")
  bn(2.7, 1L, "up", symmetric = NA) |> expect_error()
  bn(2.7, 1L, "up", symmetric = "yes") |> expect_error()
  bn(2.7, 1L, "ties_up", symmetric = TRUE) |> expect_error()
  # The parameterized methods reject a threshold at or beyond either end:
  for (m in c("up_from", "down_from", "up_from_or_down_from")) {
    bn(2.7, 1L, m, threshold = 0)  |> expect_error("`threshold` must be")
    bn(2.7, 1L, m, threshold = 10) |> expect_error("`threshold` must be")
    bn(2.7, 1L, m, threshold = -3) |> expect_error("`threshold` must be")
    bn(2.7, 1L, m, threshold = "5") |> expect_error("`threshold` must be")
    bn(2.7, 1L, m, threshold = 5) |> expect_no_error()
  }
})
