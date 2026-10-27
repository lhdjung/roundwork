# Package index

## Main API

Reconstruct rounded numbers, or the bounds of the numbers behind them.

- [`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md)
  : General interface to reconstructing rounded numbers
- [`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)
  : Reconstruct rounding bounds

## Individual rounding procedures

Each of these can also be reached through
[`reround()`](https://lhdjung.github.io/roundwork/reference/reround.md)’s
`rounding` argument.

- [`round_up_from()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
  [`round_down_from()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
  [`round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
  [`round_down()`](https://lhdjung.github.io/roundwork/reference/rounding-common.md)
  : Common rounding procedures
- [`round_ties_up()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md)
  [`round_ties_down()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md)
  [`round_ties_away()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md)
  [`round_ties_zero()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md)
  [`round_ties_even()`](https://lhdjung.github.io/roundwork/reference/rounding-ties.md)
  : Rounding procedures named by their tie-breaking rule
- [`round_ceiling()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
  [`round_floor()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
  [`round_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
  [`anti_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
  [`round_anti_trunc()`](https://lhdjung.github.io/roundwork/reference/rounding-uncommon.md)
  : Rounding in one direction
- [`round_ties_to_even()`](https://lhdjung.github.io/roundwork/reference/ieee-754.md)
  [`round_ties_to_away()`](https://lhdjung.github.io/roundwork/reference/ieee-754.md)
  [`round_toward_positive()`](https://lhdjung.github.io/roundwork/reference/ieee-754.md)
  [`round_toward_negative()`](https://lhdjung.github.io/roundwork/reference/ieee-754.md)
  [`round_toward_zero()`](https://lhdjung.github.io/roundwork/reference/ieee-754.md)
  : IEEE 754 rounding standard

## Fractions and bias

- [`reround_to_fraction()`](https://lhdjung.github.io/roundwork/reference/fractional-rounding.md)
  [`reround_to_fraction_level()`](https://lhdjung.github.io/roundwork/reference/fractional-rounding.md)
  : Rounding to the nearest fraction
- [`rounding_bias()`](https://lhdjung.github.io/roundwork/reference/rounding_bias.md)
  : Compute rounding bias

## For package developers

The exact-integer form of
[`unround()`](https://lhdjung.github.io/roundwork/reference/unround.md)’s
bounds, the whole-number sums they admit, and the meaning of a
`"ties_*"` string, for building consistency tests on.

- [`bound_numerators()`](https://lhdjung.github.io/roundwork/reference/bound_numerators.md)
  : Rounding bounds as exact integers

- [`sum_range()`](https://lhdjung.github.io/roundwork/reference/sum_range.md)
  : Whole-number sums behind a rounded mean

- [`resolve_ties_rounding()`](https://lhdjung.github.io/roundwork/reference/resolve_ties_rounding.md)
  :

  Resolve a `"ties_*"` rounding string
