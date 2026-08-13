x <- rnorm(25000, 500, 30)

test_reround <- function(x, digits) {
  all(
    all(dplyr::near(reround(x, digits, "up"), round_up(x, digits))),
    all(dplyr::near(reround(x, digits, "down"), round_down(x, digits))),
    all(dplyr::near(reround(x, digits, "even"), round(x, digits))),
    all(dplyr::near(reround(x, digits, "ceiling"), round_ceiling(x, digits))),
    all(dplyr::near(reround(x, digits, "floor"), round_floor(x, digits))),
    all(dplyr::near(reround(x, digits, "trunc"), round_trunc(x, digits))),
    all(dplyr::near(
      reround(x, digits, "anti_trunc"),
      round_anti_trunc(x, digits)
    ))
  )
}


test_that("`reround()` works like each of the specific rounding functions", {
  test_reround(x, 1:250) |> expect_true()
})


test_that("a compound method returns both results per input value", {
  # Interleaved, so that each input value's own pair of results stays together.
  # A caller that pools them across input values compares matches that did not
  # come from the same original value; see
  # <https://github.com/lhdjung/scrutiny/issues/85>.
  expect_equal(reround(c(1.25, 2.35), 1), c(1.3, 1.2, 2.4, 2.3))
  expect_length(reround(c(1.25, 2.35), 1), 4L)
  expect_length(reround(c(1.25, 2.35), 1, "ceiling_or_floor"), 4L)
  expect_length(
    reround(c(1.25, 2.35), 1, "up_from_or_down_from", threshold = 4),
    4L
  )
  # ...whereas a single method returns one value per input value:
  expect_length(reround(c(1.25, 2.35), 1, "up"), 2L)
})
