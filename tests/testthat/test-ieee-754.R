x <- runif(100000, min = 1900, max = 2100)
x_up <- x |> round_ties_to_away(2) |> trunc_reverse()
x_down <- x |> round_down(2) |> trunc_reverse()

# Avoid whole numbers that will be rounded up, artificially leading to a
# difference of 1:
x_up <- x_up[x_up != 0]
x_down <- x_down[x_down != 0]


# Threshold 5 (via the wrappers) ------------------------------------------

test_that("`round_ties_to_away()` works correctly", {
  expect_equal(
    x_up,
    x_up |> trunc_reverse() |> round_ties_to_away(2)
  )
})

test_that("`round_down()` works correctly", {
  expect_equal(
    x_down,
    x_down |> trunc_reverse() |> round_down(2)
  )
})


# Each IEEE 754 name is a wrapper -----------------------------------------

# The standard's five rounding-direction attributes all have counterparts under
# roundwork's own names. Both spellings are exported, so the tests below pin
# them to each other; `test-round.R` pins the behavior itself.

test_that("the IEEE 754 functions round as the standard says", {
  # Hand-computed from the definition of each attribute:
  expect_equal(round_ties_to_away(c(-2.5, 2.5)), c(-3, 3))
  expect_equal(round_toward_positive(c(-2.7, 2.1)), c(-2, 3))
  expect_equal(round_toward_negative(c(-2.1, 2.7)), c(-3, 2))
  expect_equal(round_toward_zero(c(-2.7, 2.7)), c(-2, 2))
  # *roundTiesToEven* is `base::round()`, ties broken by parity:
  expect_equal(round_ties_to_even(c(0.5, 1.5, 2.5)), c(0, 2, 2))
})

test_that("`round_ties_to_even()` passes `...` down to `base::round()`", {
  expect_equal(round_ties_to_even(1.2345, digits = 2), round(1.2345, 2))
})
