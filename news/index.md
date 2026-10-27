# Changelog

## roundwork 0.1.0

CRAN release: 2026-10-26

This release accompanies [scrutiny](https://lhdjung.github.io/scrutiny/)
1.0.0, which now imports its rounding from roundwork instead of keeping
copies of the functions. The rounding layer was rewritten inside
scrutiny in the run-up to that release and then moved here, so nearly
every function changed, and the rounding bounds and the rounding
procedures are now derived from a single shared source. See
[`vignette("scrutiny")`](https://lhdjung.github.io/roundwork/articles/scrutiny.md)
for how the two packages relate.

### New functions

- [`round_ties_up()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md),
  [`round_ties_down()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md),
  [`round_ties_away()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md),
  [`round_ties_zero()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md),
  and
  [`round_ties_even()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md)
  name a complete tie-breaking procedure each, so that `symmetric` does
  not have to be spelled out separately. The first four are the
  combinations of
  [`round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
  and
  [`round_down()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
  with `symmetric`, and
  [`round_ties_even()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md)
  is [`base::round()`](https://rdrr.io/r/base/Round.html); nothing is
  deprecated by them. The `rounding` strings `"ties_up"`, `"ties_down"`,
  `"ties_away"`, `"ties_zero"`, and `"ties_even"` reach the same
  procedures through
  [`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md)
  and
  [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md).
  Combining one of these strings with `symmetric = TRUE` is an error,
  because the string already determines how negative ties are broken.

  They exist because the translation is easy to get backwards: “up”
  means *on the number line* here, but *away from zero* in Excel, Java,
  and Python.
  [`round_ties_away()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md)
  is the same procedure as
  [`round_ties_to_away()`](https://lhdjung.github.io/roundwork/reference/ieee-754.md)
  under a name that does not presuppose the IEEE 754 vocabulary.

- [`bound_numerators()`](https://lhdjung.github.io/roundwork/reference/bound_numerators.md)
  returns the same bounds as
  [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md),
  but as exact integer numerators over a common denominator instead of
  decimals. It is the low-level entry point that consistency tests build
  on: deriving candidate values from floating-point bounds can drop a
  legitimate candidate or admit a phantom one, either of which flips a
  verdict. See
  [lhdjung/scrutiny#86](https://github.com/lhdjung/scrutiny/issues/86),
  which was mostly about a development version of scrutiny.

- [`sum_range()`](https://lhdjung.github.io/roundwork/reference/sum_range.md)
  returns the least and the greatest whole-number sum that, divided by
  `n_items` and rounded, gives a reported mean. This is the arithmetic
  behind GRIM, and behind any other share of a whole such as a
  percentage. It is built on
  [`bound_numerators()`](https://lhdjung.github.io/roundwork/reference/bound_numerators.md)
  and exact in the same way, so packages that check granularity no
  longer have to derive candidate sums from floating-point products.
  Like
  [`bound_numerators()`](https://lhdjung.github.io/roundwork/reference/bound_numerators.md),
  it is a low-level primitive that leaves most input checks to the
  caller.

- [`resolve_ties_rounding()`](https://lhdjung.github.io/roundwork/reference/resolve_ties_rounding.md)
  translates a `"ties_*"` string into the `rounding` and `symmetric` it
  stands for, so that a function that branches on `rounding` itself
  agrees with
  [`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md)
  and
  [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md).

### Rounding bounds

- [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
  now derives its bounds from the same machinery that a consistency test
  derives its candidate range from, so a test and the bounds can no
  longer disagree about what a rounded number means, about which
  rounding methods exist, or about what `threshold` and `symmetric`
  mean.

- [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
  gains a `symmetric` argument, matching the rounding functions it
  inverts.

- An infinite `x` has `NA` bounds, like a missing one. Its “bounds” used
  to be infinite on both sides, which no rounded number can have.

- [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
  now supports `"up_from"`, `"down_from"`, `"up_from_or_down_from"`,
  `"ceiling_or_floor"`, and the five `"ties_*"` methods. These used to
  throw an error, so a caller could reconstruct with a rounding method
  whose bounds
  [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
  then refused to give.

- `threshold` no longer affects `"up_or_down"`, `"up"`, and `"down"`.
  Those round from a fixed 5, so a `threshold` other than 5 used to
  widen the reconstructed range for a rounding that never happens.

- With `rounding = "even"`, both bounds are now reported as inclusive
  rather than as `NA`.
  [`base::round()`](https://rdrr.io/r/base/Round.html) breaks ties by
  the parity of the binary double, so whether a bound is really
  inclusive cannot be predicted; treating both as inclusive can only
  widen a range, never narrow it, which is the safe direction for error
  detection.

- A vector-valued `digits` now yields a well-formed tibble. The row
  count used to be taken from `length(x)` while the columns took
  whatever length [`paste0()`](https://rdrr.io/r/base/paste.html)
  recycling produced.

### Rounding procedures

- Every rounding function now shares one floating-point tolerance of
  about `1.5e-9`, and
  [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)’s
  bounds assume exactly that tolerance. Shifting a number by `digits`
  decimal places is inexact — `0.28 * 100` is `28.000000000000004` — and
  without the shared nudge, `round_ceiling(0.28, 2)` was `0.29`. The
  equality between the two families was implicit before and is now
  stated in one constant.

- [`anti_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
  and
  [`round_anti_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
  leave a value that already sits on the rounding grid where it is, and
  `0` stays `0`. Previously they moved such a value one step further
  out, so `anti_trunc(3)` was `4`. This makes them exact matches for
  Excel’s and Google Sheets’ `ROUNDUP()`, Java’s `RoundingMode.UP`, and
  Python’s `decimal.ROUND_UP`.

- [`round_down_from()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
  interprets `threshold` like
  [`round_up_from()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md):
  it rounds down if the part cut off is at most `threshold` tenths of a
  rounding step. It used to switch at `10 - threshold`, so
  `"up_from_or_down_from"` could span up to 1.8 steps, and a GRIM test
  with `threshold = 9` accepted every mean at `n = 57`. Results at the
  default `threshold = 5` are unchanged.

- `digits` may be negative, rounding to powers of ten.

### `reround()`

- `rounding`, `threshold`, and `symmetric` must now each have length 1.
  They describe a single rounding procedure, which is applied to all of
  `x`; passing vectors paired values with procedures by position, which
  no consistency test ever wanted.
  [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
  keeps the old behavior for its display use case.

- In every function, `rounding` must be a string and `symmetric` a
  single `TRUE` or `FALSE`. A numeric or logical `rounding` was silently
  read as a position in the list of rounding methods, so `rounding = 2`
  was `"ties_down"`.

- The compound methods `"up_or_down"`, `"up_from_or_down_from"`, and
  `"ceiling_or_floor"` return their two results per input value
  *interleaved*, so that each input value’s own pair stays together.
  Pooling the pairs across input values compares matches that did not
  come from the same original value; that was a false-pass bug in a
  GRIMMER implementation
  ([lhdjung/scrutiny#85](https://github.com/lhdjung/scrutiny/issues/85)).

- [`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md)
  is no longer built on
  [`Vectorize()`](https://rdrr.io/r/base/Vectorize.html). Every rounding
  function is natively vectorized, so the dispatch now happens once for
  the whole of `x`.

### `threshold` validation

- A `threshold` of 5 is now accepted. It used to throw an error, on the
  theory that it could only be the argument’s default showing through —
  so any caller that computed a threshold and passed it on failed
  spuriously at exactly the most common value. `"up_from"` with a
  threshold of 5 is simply `"up"`, which is a correct answer rather than
  an error.

- A `threshold` outside the open interval from 0 to 10 is now rejected.
  At 0 or 10, one of the two directions can never be taken, which
  silently turns the method into a ceiling or a floor.

### Fractional rounding

- [`reround_to_fraction()`](https://lhdjung.github.io/roundwork/reference/fractional-rounding.md)
  and
  [`reround_to_fraction_level()`](https://lhdjung.github.io/roundwork/reference/fractional-rounding.md)
  return two values per element of `x` under a compound rounding method,
  as
  [`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md)
  does. The two procedures used to be *paired* with the elements of `x`
  instead, so `reround_to_fraction(c(0.4, 0.6), denominator = 2)`
  returned two values rather than four.

- [`reround_to_fraction()`](https://lhdjung.github.io/roundwork/reference/fractional-rounding.md)
  resolves `digits = "auto"` before validating `digits` as a whole
  number. The string used to reach the numeric check and fail there.

- Subsequent rounding to `digits` keeps each branch of a compound method
  on its own procedure, instead of re-applying the compound method and
  doubling the values a second time.

- Both functions accept one `digits` value per element of `x`, which is
  what the argument is documented to take. This used to fail an internal
  length check.

### Rounding bias

- [`rounding_bias()`](https://lhdjung.github.io/roundwork/reference/rounding_bias.md)
  now throws an error when `rounding` is set to one of the compound
  methods `"up_or_down"`, `"up_from_or_down_from"`, or
  `"ceiling_or_floor"`, which its documentation has always ruled out.
  Nothing enforced that before: the compound method left two rounded
  values per element of `x`, subtracting `x` recycled it across the
  pairs, and the result was twice as long as `x` and misaligned
  throughout. The error names the two constituent procedures so that
  they can be compared one at a time.

### Other

- [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md),
  [`reround_to_fraction()`](https://lhdjung.github.io/roundwork/reference/fractional-rounding.md),
  and
  [`reround_to_fraction_level()`](https://lhdjung.github.io/roundwork/reference/fractional-rounding.md)
  no longer throw a spurious length error when two arguments of the same
  length are separated by one of length 1, as in
  `unround(x, digits = digits)` with a vector `x`, a vector `digits`,
  and the default `rounding`. The shared length check deduplicated
  lengths by an index that counted all arguments rather than only those
  longer than 1.

- The tests now sweep every rounding method against its own bounds
  across signs, decimal counts, thresholds, and `symmetric`, checking
  the inclusivity of each bound as well as its position. This is what
  pins the forward rounding functions and the inverse bounds to each
  other.

## roundwork 0.0.1

CRAN release: 2024-09-25

- Initial CRAN submission.
- This new package is a spin-off from
  [scrutiny](https://lhdjung.github.io/scrutiny/). Much of the code was
  copied from there; see Github issue
  [lhdjung/scrutiny#68](https://github.com/lhdjung/scrutiny/issues/68).
- Notable changes compared to scrutiny:
  - [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
    no longer guesses whether the bounds are inclusive with
    `rounding = "even"`. This is highly variable in the underlying
    [`base::round()`](https://rdrr.io/r/base/Round.html) function, and
    the firm statement in earlier versions of scrutiny that the bounds
    are never inclusive was unwarranted.
  - [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md),
    [`anti_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
    , and
    [`round_anti_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
    now correctly return zero in a rare edge case (rounding or
    unrounding zero while assuming `"anti_trunc"` rounding).
  - Five new functions around
    [`round_ties_to_even()`](https://lhdjung.github.io/roundwork/reference/ieee-754.md)
    implement the IEEE 754 rounding standard by wrapping existing
    functions.
