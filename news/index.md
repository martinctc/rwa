# Changelog

## rwa (development version)

### New Features

- Added
  [`rwa_logit()`](https://martinctc.github.io/rwa/reference/rwa_logit.md)
  and
  [`rwa_multiregress()`](https://martinctc.github.io/rwa/reference/rwa_multiregress.md)
  to support logistic regression and multiple regression.
- Added new vignette to cover the new regression methods.
- Added `use` parameter to
  [`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md) function
  to control how missing data is handled when computing correlations.
  Options include “pairwise.complete.obs” (default, pairwise deletion),
  “complete.obs” (listwise deletion), and other standard options from
  [`cor()`](https://rdrr.io/r/stats/cor.html).
  ([\#12](https://github.com/martinctc/rwa/issues/12))
- Added `weight` parameter to
  [`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md) function
  to perform observation-weighted Relative Weights Analysis
  ([\#12](https://github.com/martinctc/rwa/issues/12)), using a weighted
  complete-case correlation matrix. Bootstrap inference uses iid
  individual-row resampling, carrying each row’s weight; clusters,
  strata, and replicate-weight survey designs are not supported.
- Weighted results from both
  [`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md) and
  [`rwa_multiregress()`](https://martinctc.github.io/rwa/reference/rwa_multiregress.md)
  now include `n_weighted` (sum of retained original weights) and
  `n_effective` (Kish’s unequal-weighting effective sample size).
  Existing unweighted return fields are unchanged.
- Added a weighted/missing-data vignette explaining filtering, weight
  scaling, diagnostic counts, and bootstrap limitations.
- Updated the introductory and regression-methods vignettes to cover the
  `use` and `weight` arguments, the weighted sample-size diagnostics,
  and the multiple-regression-only scope. Corrected the introductory
  vignette’s incorrect statement that missing data is handled by
  listwise deletion; the default has been pairwise deletion.

### Improvements

- Updated all bootstrap functions to support the new `use` and `weight`
  parameters
- Shared numeric, finite, strictly positive weight validation across
  point estimates and bootstrap calculations; missing-weight filtering
  remains mode-dependent.
- Validate the full joint correlation matrix and predictor invertibility
  with documented numerical tolerances and actionable errors for
  missing-data-induced indefinite matrices, constant variables,
  singularity, and insufficient data
  ([\#24](https://github.com/martinctc/rwa/issues/24)). Exact fits
  remain valid when predictors are not collinear.
- Preserve legacy missing-data preprocessing, including outcome removal
  before every correlation mode and weighted predictor-completeness
  filtering even for `all.obs`; corrected `na.or.complete`
  documentation.
- [`plot_rwa()`](https://martinctc.github.io/rwa/reference/plot_rwa.md)
  now reports the sum of weights and the effective sample size in the
  caption for weighted analyses, so weighted charts are distinguishable
  from unweighted ones.
- Weighted results return `n_weighted` and `n_effective` immediately
  after `n`, making them easier to find. Field names and unweighted
  output are unchanged.
- Datasets with fewer usable observations than predictors now name the
  sample-size problem in the singular-matrix error, instead of reporting
  only an eigenvalue (relevant to the rank-deficient data discussed in
  [\#10](https://github.com/martinctc/rwa/issues/10)). Models that were
  previously estimable, including pairwise-deletion models with few
  complete cases, are unaffected.
- Added a plain-English summary of the weighting and missing-data
  behavior to
  [`?rwa`](https://martinctc.github.io/rwa/reference/rwa.md), including
  guidance on when to use survey weights.
- Improved test coverage and minor bug fixes

### Bug Fixes

- Fixed weighted single-predictor calculations and random-comparison
  name collisions, including weight columns named `rand`.
- Bootstrap samples now preserve predictor identity, order, and
  statistic length. Invalid or degenerate samples error instead of
  dropping variables or recycling estimates; no samples are skipped or
  retried.
- Comprehensive bootstrap now computes random comparisons without a
  focal predictor and correctly labels and maps random/focal intervals
  to the requested predictors.

------------------------------------------------------------------------

## rwa 0.1.1

CRAN release: 2026-01-20

### Improvements

- **Input validation**: Added comprehensive validation for
  [`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md) parameters
  including `conf_level`, `n_bootstrap`, non-numeric variables,
  zero-variance variables, and singular correlation matrices with
  informative error messages
- **Code refactoring**: Consolidated duplicate RWA calculation code in
  bootstrap functions into a single internal helper
- **Documentation**: Added links to pkgdown site in README; fixed
  internal function documentation

### Bug Fixes

- Fixed flaky bootstrap test by using a fixed random seed for
  reproducibility ([\#20](https://github.com/martinctc/rwa/issues/20))

### Tests

- Added extensive tests for input validation and edge cases
  (collinearity, small samples, invalid parameters)

------------------------------------------------------------------------

## rwa 0.1.0

CRAN release: 2025-07-16

### New Features

- **Bootstrap confidence intervals**: Added `bootstrap = TRUE` parameter
  to [`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md) for
  statistical significance testing of relative weights
- **Result sorting**: Added `sort = TRUE` parameter to automatically
  sort results by importance (descending order). Set `sort = FALSE` to
  preserve original predictor order
- **Comprehensive vignette**: New detailed documentation covering
  methodology, examples, and best practices
- **Enhanced documentation**: Updated README and function documentation

### Technical Improvements

- **Package compliance**: Updated DESCRIPTION with proper `Authors@R`
  field for CRAN submission
- **CI/CD**: Enhanced GitHub Actions workflow with vignette building
  support
- **Dependencies**: Added `boot`, `purrr`, and `utils` packages for
  bootstrap functionality
- **Code quality improvements**: Fixed long lines in R code to meet CRAN
  standards
- **Documentation cleanup**: Improved code formatting and removed unused
  variables
- **Enhanced vignette formatting**: Cleaned up formatting in
  comprehensive vignette documentation

### Bug Fixes

- Fixed vignette compilation issues
- Resolved R CMD check warnings and notes
- Removed unused variables to eliminate R CMD check notes
- Improved consistency in code formatting

------------------------------------------------------------------------

### Version 0.0.3

Re-submission to CRAN

- Unwrap `\donttest{}` in examples where unnecessary

### Version 0.0.2

Re-submission to CRAN

- DOI references added to DESCRIPTION
- Added CodeFactor badge
- Typos in DESCRIPTION rectified

### Version 0.0.1

First submission to CRAN (required to re-submit)

- [`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md)
- [`plot_rwa()`](https://martinctc.github.io/rwa/reference/plot_rwa.md)
- [`remove_all_na_cols()`](https://martinctc.github.io/rwa/reference/remove_all_na_cols.md)
- `%>%` operator is exported
