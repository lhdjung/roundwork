# roundwork 0.1.0

This release takes over the rounding infrastructure as it was revamped in [scrutiny](https://lhdjung.github.io/scrutiny/) 1.0.0. Nearly every function was rewritten, and scrutiny now depends on roundwork for all of it rather than keeping its own copies. See Github issue [lhdjung/scrutiny#68](https://github.com/lhdjung/scrutiny/issues/68).

## New functions

-   `round_ties_up()`, `round_ties_down()`, `round_ties_away()`, and `round_ties_zero()` name a complete tie-breaking procedure each, so that `symmetric` does not have to be spelled out separately. They are the four combinations of `round_up()` and `round_down()` with `symmetric`, and nothing is deprecated by them. The `rounding` strings `"ties_up"`, `"ties_down"`, `"ties_away"`, and `"ties_zero"` reach the same procedures through `reround()` and `unround()`; `symmetric` is ignored when one of them is given.

    They exist because the translation is easy to get backwards: "up" means *on the number line* here, but *away from zero* in Excel, Java, and Python. `round_ties_away()` is the same procedure as `round_ties_to_away()` under a name that does not presuppose the IEEE 754 vocabulary.

-   `bound_numerators()` returns the same bounds as `unround()`, but as exact integer numerators over a common denominator instead of decimals. It is the low-level entry point that consistency tests build on: deriving candidate values from floating-point bounds can drop a legitimate candidate or admit a phantom one, either of which flips a verdict. See [lhdjung/scrutiny#86](https://github.com/lhdjung/scrutiny/issues/86).

## Rounding bounds

-   `unround()` now derives its bounds from the same machinery that scrutiny's GRIM, GRIMMER, and DEBIT use, so the tests and the bounds can no longer disagree about what a rounded number means, about which rounding methods exist, or about what `threshold` and `symmetric` mean.

-   `unround()` gains a `symmetric` argument, matching the rounding functions it inverts.

-   `unround()` now supports `"up_from"`, `"down_from"`, `"up_from_or_down_from"`, `"ceiling_or_floor"`, and the four `"ties_*"` methods. These used to throw an error, so a caller could reconstruct with a rounding method whose bounds `unround()` then refused to give.

-   `threshold` no longer affects `"up_or_down"`, `"up"`, and `"down"`. Those round from a fixed 5, so a `threshold` other than 5 used to widen the reconstructed range for a rounding that never happens.

-   With `rounding = "even"`, both bounds are now reported as inclusive rather than as `NA`. `base::round()` breaks ties by the parity of the binary double, so whether a bound is really inclusive cannot be predicted; treating both as inclusive can only widen a range, never narrow it, which is the safe direction for error detection.

-   A vector-valued `digits` now yields a well-formed tibble. The row count used to be taken from `length(x)` while the columns took whatever length `paste0()` recycling produced.

## Rounding procedures

-   Every rounding function now shares one floating-point tolerance of about `1.5e-9`, and `unround()`'s bounds assume exactly that tolerance. Shifting a number by `digits` decimal places is inexact --- `0.28 * 100` is `28.000000000000004` --- and without the shared nudge, `round_ceiling(0.28, 2)` was `0.29`. The equality between the two families was implicit before and is now stated in one constant.

-   `anti_trunc()` and `round_anti_trunc()` leave a value that already sits on the rounding grid where it is, and `0` stays `0`. Previously they moved such a value one step further out, so `anti_trunc(3)` was `4`. This makes them exact matches for Excel's and Google Sheets' `ROUNDUP()`, Java's `RoundingMode.UP`, and Python's `decimal.ROUND_UP`.

-   `round_up_from()` and `round_down_from()` document how `round_down_from()` mirrors `threshold`: it rounds down when the cut-off part is at most `10 - threshold` tenths of a step. The two coincide at 5, which is why the difference had gone unnoticed.

-   `digits` may be negative, rounding to powers of ten.

## `reround()`

-   `rounding`, `threshold`, and `symmetric` must now each have length 1. They describe a single rounding procedure, which is applied to all of `x`; passing vectors paired values with procedures by position, which no consistency test ever wanted. `unround()` keeps the old behavior for its display use case.

-   The compound methods `"up_or_down"`, `"up_from_or_down_from"`, and `"ceiling_or_floor"` return their two results per input value *interleaved*, so that each input value's own pair stays together. Pooling the pairs across input values compares matches that did not come from the same original value; that was a false-pass bug in scrutiny's GRIMMER ([lhdjung/scrutiny#85](https://github.com/lhdjung/scrutiny/issues/85)).

-   `reround()` is no longer built on `Vectorize()`. Every rounding function is natively vectorized, so the dispatch now happens once for the whole of `x`.

## `threshold` validation

-   A `threshold` of 5 is now accepted. It used to throw an error, on the theory that it could only be the argument's default showing through --- so any caller that computed a threshold and passed it on failed spuriously at exactly the most common value. `"up_from"` with a threshold of 5 is simply `"up"`, which is a correct answer rather than an error.

-   A `threshold` outside the open interval from 0 to 10 is now rejected. At 0 or 10, one of the two directions can never be taken, which silently turns the method into a ceiling or a floor.

## Fractional rounding

-   `reround_to_fraction()` and `reround_to_fraction_level()` return two values per element of `x` under a compound rounding method, as `reround()` does. The two procedures used to be *paired* with the elements of `x` instead, so `reround_to_fraction(c(0.4, 0.6), denominator = 2)` returned two values rather than four.

-   `reround_to_fraction()` resolves `digits = "auto"` before validating `digits` as a whole number. The string used to reach the numeric check and fail there.

-   Subsequent rounding to `digits` keeps each branch of a compound method on its own procedure, instead of re-applying the compound method and doubling the values a second time.

-   Both functions accept one `digits` value per element of `x`, which is what the argument is documented to take. This used to fail an internal length check.

## Rounding bias

-   `rounding_bias()` now throws an error when `rounding` is set to one of the compound methods `"up_or_down"`, `"up_from_or_down_from"`, or `"ceiling_or_floor"`, which its documentation has always ruled out. Nothing enforced that before: the compound method left two rounded values per element of `x`, subtracting `x` recycled it across the pairs, and the result was twice as long as `x` and misaligned throughout. The error names the two constituent procedures so that they can be compared one at a time.

## Other

-   `check_lengths_congruent()`, which backs the length checks in several functions, no longer errors on arguments that are in fact the same length. Its deduplication of lengths was indexed by the lengths of all arguments rather than of those longer than 1, so it silently did nothing whenever a length-1 argument sat in between.

-   The tests now sweep every rounding method against its own bounds across signs, decimal counts, thresholds, and `symmetric`, checking the inclusivity of each bound as well as its position. This is what pins the forward rounding functions and the inverse bounds to each other.


# roundwork 0.0.1

-   Initial CRAN submission.
-   This new package is a spin-off from [scrutiny](https://lhdjung.github.io/scrutiny/). Much of the code was copied from there; see Github issue [lhdjung/scrutiny#68](https://github.com/lhdjung/scrutiny/issues/68).
-   Notable changes compared to scrutiny:
    -   `unround()` no longer guesses whether the bounds are inclusive with `rounding = "even"`. This is highly variable in the underlying `base::round()` function, and the firm statement in earlier versions of scrutiny that the bounds are never inclusive was unwarranted.
    -   `unround()`, `anti_trunc()` , and `round_anti_trunc()` now correctly return zero in a rare edge case (rounding or unrounding zero while assuming `"anti_trunc"` rounding).
    -   Five new functions around `round_ties_to_even()` implement the IEEE 754 rounding standard by wrapping existing functions.
