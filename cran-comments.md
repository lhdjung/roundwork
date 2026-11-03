## Submission comments

This update follows 0.1.0 after only about a week, sorry. It fixes a hang: when a roundwork function raised an error and was called from a user's function evaluated in a data mask, as by `rlang::eval_tidy()`, R looped forever. scrutiny 1.0.0, which is to be submitted next, imports roundwork and needs this fix. The release also fixes several smaller bugs and drops three imports (dplyr, magrittr, and stringr).

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

unsum, the only reverse dependency on CRAN, passes R CMD check with this version.
