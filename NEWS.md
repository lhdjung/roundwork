# roundwork 0.0.1

-   Initial CRAN submission.
-   This new package is a spin-off from [scrutiny](https://lhdjung.github.io/scrutiny/). Much of the code was copied from there; see Github issue [lhdjung/scrutiny#68](https://github.com/lhdjung/scrutiny/issues/68).
-   Notable changes compared to scrutiny:
    -   `unround()` no longer guesses whether the bounds are inclusive with `rounding = "even"`. This is highly variable in the underlying `base::round()` function, and the firm statement in earlier versions of scrutiny that the bounds are never inclusive was unwarranted.
    -   `unround()`, `anti_trunc()` , and `round_anti_trunc()` now correctly return zero in a rare edge case (rounding or unrounding zero while assuming `"anti_trunc"` rounding).
    -   Five new functions around `round_ties_to_even()` implement the IEEE 754 rounding standard by wrapping existing functions.
