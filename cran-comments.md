## CRAN Submission Comments for rwa v1.0.0

### Test environments

* Local Windows 11 (R 4.6.1)
* GitHub Actions (Ubuntu, macOS, Windows) with R release and devel

### R CMD check results
0 errors | 0 warnings | 0 notes

### Changes in this version

This is a major release with a breaking change:

* `Raw.Significant` is now derived from a comparison against a randomly
  generated variable's weight, rather than from the confidence interval
  around the raw weight itself (#26). Raw relative weights are non-negative,
  so an interval around a weight almost always excludes zero and previously
  flagged even unrelated predictors as significant. Predictors previously
  reported as significant may now correctly be reported as not significant.
* Added `rwa_logit()` and `rwa_multiregress()` to support logistic
  regression and multiple regression, with an accompanying vignette.
* Added `use` and `weight` parameters to `rwa()` to control missing-data
  handling and support observation-weighted analysis (#12), including
  `n_weighted` and `n_effective` diagnostics.
* Added validation for the full joint correlation matrix and predictor
  invertibility, with actionable errors for missing-data-induced indefinite
  matrices, constant variables, singularity, and insufficient data (#24).
* Various bug fixes and internal improvements — see NEWS.md for full details.
