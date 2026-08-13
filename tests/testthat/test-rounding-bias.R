vec <- seq(from = 0.01, to = 0.1, by = 0.01)


test_that("`rounding_bias()` is the mean shift that rounding applies", {
  # Bias is `mean(x_rounded) - mean(x)`, computed here the long way round so
  # that the expectation does not go through `reround()` a second time:
  expect_equal(
    rounding_bias(vec, digits = 1, rounding = "up"),
    mean(round_up(vec, 1)) - mean(vec)
  )
  expect_equal(
    rounding_bias(vec, digits = 1, rounding = "down"),
    mean(round_down(vec, 1)) - mean(vec)
  )
  expect_equal(
    rounding_bias(vec, digits = 1, rounding = "even"),
    mean(round(vec, 1)) - mean(vec)
  )
})


test_that("`mean = FALSE` returns one bias per element of `x`", {
  bias <- rounding_bias(vec, digits = 1, rounding = "up", mean = FALSE)
  expect_length(bias, length(vec))
  expect_equal(bias, round_up(vec, 1) - vec)
  # ...and the default reduces exactly that vector to its mean:
  expect_equal(rounding_bias(vec, digits = 1, rounding = "up"), mean(bias))
})


test_that("`rounding_bias()` rejects the compound rounding methods", {
  # Each of these names two procedures, so `reround()` returns two values per
  # element of `x`. Subtracting `x` then recycled it across the pairs and
  # returned twice as many biases as there are inputs, misaligned throughout.
  # The documentation had always ruled these out; nothing enforced it.
  for (rounding in c(
    "up_or_down",
    "up_from_or_down_from",
    "ceiling_or_floor"
  )) {
    expect_error(
      rounding_bias(vec, digits = 1, rounding = rounding, threshold = 4),
      regexp = "single rounding procedure"
    )
  }
  # The error names a constituent to use instead:
  expect_error(
    rounding_bias(vec, digits = 1, rounding = "up_or_down"),
    regexp = "up"
  )
})


test_that("`rounding_bias()` accepts every single rounding procedure", {
  singles <- c(
    "up",
    "down",
    "even",
    "ceiling",
    "floor",
    "trunc",
    "anti_trunc",
    "up_from",
    "down_from",
    "ties_up",
    "ties_down",
    "ties_away",
    "ties_zero"
  )
  for (rounding in singles) {
    bias <- rounding_bias(
      vec,
      digits = 1,
      rounding = rounding,
      threshold = 4,
      mean = FALSE
    )
    expect_length(bias, length(vec))
  }
})


test_that("a vector-valued `rounding` is still `reround()`'s error", {
  # The compound check only applies to a length-1 `rounding`; anything longer
  # is rejected downstream, where the message is about `rounding` having to
  # describe a single procedure.
  expect_error(
    rounding_bias(vec, digits = 1, rounding = c("up", "down")) |>
      suppressWarnings()
  )
})
