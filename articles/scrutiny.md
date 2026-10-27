# Relation to scrutiny

The roundwork package was outsourced from
[scrutiny](https://lhdjung.github.io/scrutiny/), a much larger and less
focused package. (See [this Github
issue](https://github.com/ianhussey/ERROR/issues/6) for the rationale.)

roundwork only contains the rounding layer:

- The individual rounding procedures
  ([`round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md),
  [`round_ceiling()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md),
  [`round_ties_away()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md),
  and so on);
- [`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md),
  the interface that names each procedure by a string;
- [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
  and
  [`bound_numerators()`](https://lhdjung.github.io/roundwork/reference/bound_numerators.md),
  which run the same procedures backwards to recover the range of an
  unknown original number;
- [`sum_range()`](https://lhdjung.github.io/roundwork/reference/sum_range.md),
  which finds the whole-number sums behind a rounded mean;
- [`reround_to_fraction()`](https://lhdjung.github.io/roundwork/reference/fractional-rounding.md)
  and
  [`rounding_bias()`](https://lhdjung.github.io/roundwork/reference/rounding_bias.md);
- [`resolve_ties_rounding()`](https://lhdjung.github.io/roundwork/reference/resolve_ties_rounding.md),
  for functions that branch on `rounding` themselves.

The error detection techniques that build on this layer — GRIM, GRIMMER,
DEBIT — live in scrutiny. From version 1.0.0 on, scrutiny imports its
rounding from roundwork and no longer exports any rounding functions of
its own.

## Resolving conflicts

Before its 1.0.0 version, scrutiny had its own copies of the rounding
functions, some of them under the same names. Attaching both packages —
roundwork and an older version of scrutiny — can create ambiguity. These
copies are older, and some of them behave differently: for example,
[`round_down_from()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
reads `threshold` differently there.

I strongly recommend using scrutiny 1.0.0 or later. However, just in
case you use roundwork together with scrutiny 0.6.1 or earlier, use the
[conflicted](https://conflicted.r-lib.org/index.html) package to clarify
which package you mean when you call certain functions:

``` r

library(conflicted)
library(scrutiny)
library(roundwork)
conflict_prefer_matching("round", "roundwork", quiet = TRUE)
conflict_prefer("anti_trunc", "roundwork", quiet = TRUE)
```

You can then run `round_up` without parentheses and check the
`namespace` signature at the bottom. If it says `namespace:roundwork`,
everything is fine. If not, please [open an
issue](https://github.com/lhdjung/roundwork/issues).

Package developers can simply use `package::function()` syntax, as
usual, or call `usethis::use_import_from()`. With scrutiny 1.0.0 or
later, there is no conflict to resolve.
