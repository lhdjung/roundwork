test_that("`is_whole_number()` returns correct values", {
  is_whole_number(1) |> expect_true()
  is_whole_number(985) |> expect_true()
  is_whole_number(37) |> expect_true()

  is_whole_number(0.2) |> expect_false()
  is_whole_number(25.05) |> expect_false()
  is_whole_number(75.489) |> expect_false()
})


test_that("`an_a()` returns correct values", {
  an_a("start") |> expect_equal("a")
  an_a("end") |> expect_equal("an")
})


test_that("`an_a_type()` returns correct values", {
  an_a_type("bla") |> expect_equal("a string")
  an_a_type(4) |> expect_equal("a double (numeric value)")
})


test_that("`wrong_spec_string()` returns a string", {
  wrong_spec_string(4) |> expect_type("character")
})


test_that("`decimal_places()` counts from the string representation", {
  # Trailing zeros are lost from a numeric, which is why `unround()` needs `x`
  # as a string unless `digits` is given:
  decimal_places("3.70") |> expect_equal(2L)
  decimal_places(3.70) |> expect_equal(1L)
  decimal_places("5") |> expect_equal(0L)
  decimal_places(c("1.2", "1.23", NA)) |> expect_equal(c(1L, 2L, NA_integer_))
  # Whitespace at the end of a string is not counted:
  decimal_places("6.0     ") |> expect_equal(1L)
})


test_that("`resolve_ties_rounding()` maps each `\"ties_*\"` string", {
  # This one table is what keeps `reround()` and `rounding_offsets()` from
  # disagreeing about what a name means.
  resolve_ties_rounding("ties_up", FALSE) |>
    expect_equal(list(rounding = "up", symmetric = FALSE))
  resolve_ties_rounding("ties_away", FALSE) |>
    expect_equal(list(rounding = "up", symmetric = TRUE))
  resolve_ties_rounding("ties_down", TRUE) |>
    expect_equal(list(rounding = "down", symmetric = FALSE))
  resolve_ties_rounding("ties_zero", FALSE) |>
    expect_equal(list(rounding = "down", symmetric = TRUE))
  # A non-`"ties_*"` method passes through with `symmetric` untouched:
  resolve_ties_rounding("up", TRUE) |>
    expect_equal(list(rounding = "up", symmetric = TRUE))
  resolve_ties_rounding("ceiling", FALSE) |>
    expect_equal(list(rounding = "ceiling", symmetric = FALSE))
})


test_that("`rounding_constituents()` splits the compound methods", {
  rounding_constituents("up_or_down") |> expect_equal(c("up", "down"))
  rounding_constituents("up_from_or_down_from") |>
    expect_equal(c("up_from", "down_from"))
  rounding_constituents("ceiling_or_floor") |>
    expect_equal(c("ceiling", "floor"))
  # A single method is its own only constituent:
  rounding_constituents("even") |> expect_equal("even")
})


test_that("`restore_sign()` gives `value` the sign of `x`", {
  restore_sign(3, 2) |> expect_equal(3)
  restore_sign(3, -2) |> expect_equal(-3)
  restore_sign(3, 0) |> expect_equal(3)
  restore_sign(c(1, 2, 3), c(-1, 0, 1)) |> expect_equal(c(-1, 2, 3))
  restore_sign(3, NA_real_) |> expect_equal(NA_real_)
})


test_that("`check_threshold_valid()` accepts thresholds inside the step", {
  # A threshold has to lie strictly inside the step, so that both directions
  # remain possible:
  expect_silent(check_threshold_valid(5))
  expect_silent(check_threshold_valid(1))
  expect_silent(check_threshold_valid(9.5))
  expect_error(check_threshold_valid(0))
  expect_error(check_threshold_valid(10))
  expect_error(check_threshold_valid(-2))
  expect_error(check_threshold_valid(NA))
  expect_error(check_threshold_valid("5"))
  expect_error(check_threshold_valid(c(3, 7)))
})


# `check_lengths_congruent()` ---------------------------------------------

# Vectors with lengths 5, 3, 2, and 3 (they are all > 1):
numbers <- 1:5
nephews <- c("Huey", "Dewey", "Louie")
norberts <- c("Lammert", "Röttgen")
nikes <- c("Air Max", "Zoom Freak", "Phantom")

test_that("`check_lengths_congruent()` throws an error when it should", {
  check_lengths_congruent(list(numbers, nephews, norberts, nikes)) |>
    expect_error()
  check_lengths_congruent(list(numbers, nephews, norberts)) |> expect_error()
  check_lengths_congruent(list(numbers, nephews, nikes)) |> expect_error()
  check_lengths_congruent(list(numbers, norberts, nikes)) |> expect_error()
  check_lengths_congruent(list(nephews, norberts, nikes)) |> expect_error()
})

test_that("`check_lengths_congruent()` throws a warning when it should", {
  check_lengths_congruent(list(nephews, nikes), warn = TRUE) |> expect_warning()
  check_lengths_congruent(list(numbers, numbers), warn = TRUE) |>
    expect_warning()
})

test_that("`check_lengths_congruent()` remains silent when it should", {
  check_lengths_congruent(list(nephews, nikes), warn = FALSE) |> expect_silent()
  check_lengths_congruent(list("a", "b", c("c", "d", "e"))) |> expect_silent()
  check_lengths_congruent(list("a", "b")) |> expect_silent()
  check_lengths_congruent(list(1, 2, 3, 4, 5)) |> expect_silent()
  check_lengths_congruent(list(1, c(1, 2), 3, 4, 5)) |> expect_silent()
})

test_that("`check_lengths_congruent()` accepts arguments of equal length", {
  # Two arguments of the same length are congruent -- that is the whole point of
  # the check -- so they warn about being paired but must not error. Up to
  # roundwork 0.1.0 they did error whenever a length-1 argument sat between them
  # in the list, because the deduplication of lengths was indexed by the lengths
  # of *all* arguments rather than of those longer than 1, and so silently did
  # nothing. `reround_to_fraction(c(0.4, 0.6), denominator = 2, digits = c(1,
  # 2))` hit exactly that.
  a2 <- 1:2
  b2 <- 3:4
  s1 <- 1

  expect_no_error(
    check_lengths_congruent(list(a2, b2, s1)) |> suppressWarnings()
  )
  expect_no_error(
    check_lengths_congruent(list(a2, s1, b2, s1, s1)) |> suppressWarnings()
  )
  expect_no_condition(check_lengths_congruent(list(a2, s1, s1)))
  expect_no_condition(check_lengths_congruent(list(s1, s1, s1)))

  # The pairing warning still fires for the congruent case:
  expect_warning(check_lengths_congruent(list(a2, b2)))
})

test_that("`check_lengths_congruent()` rejects genuinely unequal lengths", {
  a2 <- 1:2
  b2 <- 3:4
  c3 <- 1:3

  expect_error(check_lengths_congruent(list(a2, c3)))
  expect_error(check_lengths_congruent(list(a2, 1, c3, 1)))
  # ...and it names the pair that actually disagrees, not the congruent one:
  expect_error(check_lengths_congruent(list(a2, b2, c3)), regexp = "c3")
})
