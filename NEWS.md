# roundwork (development version)

-   Initial CRAN submission.
-   This new package is a spin-off from [scrutiny](https://lhdjung.github.io/scrutiny/). Much of the code was copied from there; see [lhdjung/scrutiny#68](https://github.com/lhdjung/scrutiny/issues/68) on Github.
-   Notable changes from scrutiny:
    -   `unround()` no longer guesses whether the bounds are inclusive with `rounding = "even"`. This is highly variable, and the firm statement in the previous version that the bounds are never inclusive was unwarranted.
    -   `unround()` and `anti_trunc()` now correctly throw an error in a certain edge case (rounding or unrounding zero using or presuming "anti_trunc" rounding).
