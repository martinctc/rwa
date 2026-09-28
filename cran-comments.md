## CRAN Submission Comments for rwa v1.0.1

This is a corrective submission addressing the single test ERROR reported for
rwa 1.0.0 on the R release/macOS ARM and R old-release/macOS ARM checks.

### Test environments

* Local Windows 11 (R 4.6.1)
* GitHub Actions: Ubuntu (R release), macOS arm64 (R release and old-release)

### R CMD check results

0 errors | 0 warnings | 0 notes

### Reason for the previous ERROR

The test "estimable highly correlated predictors retain their valid fit" in
`tests/testthat/test-matrix-contract.R` compared the R-squared from
`rwa_multiregress()` and `rwa()` against `summary(lm(...))$r.squared` at
`tolerance = 1e-8`, using predictors correlated at `1 - 5e-9`. That fixture gave
the predictor correlation matrix a condition number near `4e8`, so the observed
deviation tracked `condition number * machine epsilon` and exceeded the tolerance
on macOS ARM (`0.799999990` against `0.800000000`). The same fixture was already
outside tolerance on Ubuntu, so the failure was bounded floating-point rounding
rather than a defect in the calculation.

### Changes in this version

Test-only. No package code, no exported API, and no documented behaviour are
changed. `calculate_rwa()` is untouched.

* Replaced the over-conditioned fixture in "estimable highly correlated
  predictors retain their valid fit" with one whose predictors correlate at
  about `0.99995` (condition number near `4e4`, measured deviation `1.5e-12`).
  The predictors remain genuinely collinear and estimable, so the test still
  covers the near-collinear path, but the rounding error now sits roughly four
  orders of magnitude below the existing `1e-8` tolerance.
* Applied the same treatment to the neighbouring test "additional predictors do
  not impose a new conditioning cutoff", which used a `1.1e-7` offset giving a
  condition number near `3e14` and only about 7x headroom under `1e-8`. It now
  uses a `1e-4` offset (condition number near `4e8`, measured deviation
  `1.1e-12`). The test's purpose, that adding predictors introduces no new
  conditioning cutoff, is unaffected.
* Strengthened both tests without relaxing any numeric tolerance: the predictor
  correlation is pinned within `(0.9999, 1)`, the raw relative weights are
  asserted to sum to the returned R-squared, and `rwa()` is asserted to agree
  with `rwa_multiregress()`. No returned value is clamped or repaired to satisfy
  an assertion.
* The R CMD check workflow now also runs on macOS arm64 with R release and
  old-release alongside the Ubuntu R release job, so the platform that reported
  the ERROR is covered by CI.

### Notes for the reviewers

* The `WARN` count in the testthat summary comes from tests that deliberately
  trigger small-sample and extreme-order-statistic bootstrap warnings. These
  are not CRAN WARNINGs and are intentionally left in place.
* The earlier `roxygen2::roxygenise()` item on the pull request is not
  outstanding: no files under `R/` were modified, so `NAMESPACE` and `man/` are
  unchanged and already current.
