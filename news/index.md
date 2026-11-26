# Changelog

## rwa (development version)

### Documentation

- Clarified the existing `Sign` convention in
  [`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md),
  [`rwa_multiregress()`](https://martinctc.github.io/rwa/reference/rwa_multiregress.md),
  [`rwa_logit()`](https://martinctc.github.io/rwa/reference/rwa_logit.md),
  plotting documentation, README, and vignettes
  ([\#27](https://github.com/martinctc/rwa/issues/27)). `Sign` comes
  from the coefficient for an orthogonalized predictor and can differ
  from an original predictor’s marginal association.
  `Sign.Rescaled.RelWeight` is a signed importance score, not a
  regression coefficient or causal effect. **No returned values or
  behavior changed.**

### Bug Fixes

Each change below cites the source it is based on. The investigation,
including the measurements quoted here, is recorded in
[\#31](https://github.com/martinctc/rwa/issues/31)
(<https://github.com/martinctc/rwa/issues/31#issuecomment-7194980633>).

- Fixed BCa confidence intervals silently falling back to basic
  bootstrap intervals whenever `n_bootstrap` was smaller than the number
  of rows with a non-missing outcome
  ([\#31](https://github.com/martinctc/rwa/issues/31)). With the default
  `n_bootstrap = 1000`, this affected every dataset with more than 1,000
  such rows, weighted or unweighted, including the random-variable
  comparison intervals behind `Raw.Significant`. BCa intervals are the
  method recommended by Tonidandel, LeBreton and Johnson (2009, p. 392;
  <https://doi.org/10.1037/a0017735>), who found that BCa gave more
  accurate coverage, Type I error and power than percentile intervals
  (p. 398). The BCa acceleration is computed from empirical influence
  values (Efron, 1987,
  <https://doi.org/10.1080/01621459.1987.10478410>). When `n_bootstrap`
  is smaller than the number of rows,
  [`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html)
  approximates those values with the usual leave-one-out jackknife
  ([`boot::empinf()`](https://rdrr.io/pkg/boot/man/empinf.html)), but it
  re-called the RWA statistic without its required arguments, and the
  resulting error was caught and replaced by a basic interval. The
  package now computes the same jackknife influence values itself, once
  per bootstrap, and passes them to
  [`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html). The
  estimator is unchanged: tests confirm the values match
  `boot::empinf(type = "jack")`. When the analysis uses a single set of
  complete rows (weighted analyses, listwise deletion, or data without
  missing values), the values are obtained from leave-one-out
  correlation matrices rather than by refitting. This is an exact
  numerical shortcut, verified in tests to agree with refitting, and
  keeps the added time small (about 10 seconds per bootstrap at 50,000
  rows and 10 predictors). With pairwise deletion and missing values,
  each jackknife sample is refitted, which can be slow for large
  datasets; a message is shown in that case. **Intervals and
  `Raw.Significant` flags for datasets with more rows than `n_bootstrap`
  may change**, because they are now BCa intervals as documented. Raw
  and rescaled weight intervals are unchanged when `n_bootstrap` is at
  least the number of rows with a non-missing outcome, unless BCa could
  not be computed (see below).
- The random-variable significance test now follows the procedure of
  Tonidandel, LeBreton and Johnson (2009, p. 392). Their step 1 adds a
  single random variable to the original data, which is then resampled
  with the rows; their simulation does the same (p. 393). The package
  previously drew a new random variable within every bootstrap sample,
  so the point estimate and each bootstrap replicate used different
  random draws. That departs from the published procedure, and it made
  the BCa bias correction depend on an unrelated draw: in the
  investigation in [\#31](https://github.com/martinctc/rwa/issues/31),
  repeating the draw moved the lower bound for a near-zero weight by
  more than one bootstrap standard deviation. **`Random.Diff.CI.*` and
  `Raw.Significant` may change for all datasets**, including those where
  `n_bootstrap` is at least the number of rows. In a check of 150
  simulated datasets (see
  [\#31](https://github.com/martinctc/rwa/issues/31)), significance
  rates were essentially unchanged: null predictors were flagged in
  under 1% of runs, which is consistent with the conservative Type I
  error reported by Tonidandel et al. (2009, p. 393). As in the
  published procedure, results depend on the single random draw; use
  [`set.seed()`](https://rdrr.io/r/base/Random.html) for
  reproducibility. The raw and rescaled weight intervals are unaffected.
- When a BCa interval cannot be computed, the fallback is now reported
  with a warning that names the affected variables and the reason. The
  first fallback is now a percentile interval, as previously documented,
  which is the method Johnson (2004,
  <https://doi.org/10.1177/1094428104266018>) recommended before
  Tonidandel et al. (2009, p. 392) showed BCa to perform better. A basic
  interval is kept only as a last resort from earlier versions; it has
  no basis in this literature and can extend below zero even though raw
  relative weights cannot be negative. Previously, any BCa error jumped
  straight to a basic interval without warning.

------------------------------------------------------------------------

## rwa 1.0.1

CRAN release: 2026-09-28

### Improvements

- Fixed the CRAN macOS ARM test error in the near-collinear fixture of
  “estimable highly correlated predictors retain their valid fit”
  ([\#29](https://github.com/martinctc/rwa/issues/29)). The predictors
  correlated at `1 - 5e-9`, giving a condition number near `4e8` and a
  platform-dependent rounding error above the test’s tolerance. They now
  correlate at about `0.99995`, which stays highly collinear and
  estimable with a much smaller rounding error. `calculate_rwa()` is
  unchanged, since the deviation was bounded floating-point error rather
  than a calculation defect.
- Applied the same treatment to the neighbouring test “additional
  predictors do not impose a new conditioning cutoff”, whose `1.1e-7`
  offset gave a condition number near `3e14` and left only about 7x
  headroom under the `1e-8` tolerance. It now uses a `1e-4` offset. The
  assertions in both tests were strengthened, not relaxed: the predictor
  correlation is pinned within `(0.9999, 1)`, the raw relative weights
  are asserted to sum to the returned R-squared, and
  [`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md) is
  asserted to agree with
  [`rwa_multiregress()`](https://martinctc.github.io/rwa/reference/rwa_multiregress.md).
  No returned value is clamped or repaired to satisfy an assertion.
- The R CMD check workflow now runs on macOS arm64 with R release and
  old-release alongside the existing Ubuntu R release job, covering the
  CRAN platform that reported the error.

There are no user-facing changes in this release.

------------------------------------------------------------------------

## rwa 1.0.0

CRAN release: 2026-09-16

### Breaking Changes

- `Raw.Significant` is now derived from a comparison against a randomly
  generated variable’s weight, rather than from the confidence interval
  around the raw weight itself
  ([\#26](https://github.com/martinctc/rwa/issues/26)). Raw relative
  weights are non-negative, so an interval around a weight almost always
  excludes zero and previously flagged even unrelated predictors as
  significant. Comparing each weight to that of a random variable is the
  approach suggested by Tonidandel, LeBreton and Johnson (2009,
  <https://doi.org/10.1037/a0017735>) for judging whether a weight
  exceeds what chance alone would produce (see also the discussion in
  [`vignette("evaluating-rwa-method-reference")`](https://martinctc.github.io/rwa/articles/evaluating-rwa-method-reference.md)).
  This package applies a directional cutoff on that comparison: a
  predictor is significant only when the lower bound of the difference
  interval is above zero, so a predictor that performed worse than the
  random variable is not reported as significant either. **Predictors
  previously reported as significant may now correctly be reported as
  not significant.**
  [`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md) also
  returns `Random.Diff.CI.Lower` and `Random.Diff.CI.Upper` for the
  comparison the flag is based on, and the descriptive
  `Raw.RelWeight.CI.*` columns are unchanged.
- Because significance requires this comparison, `bootstrap = TRUE` now
  runs an additional bootstrap when `comprehensive = FALSE`, which
  roughly doubles bootstrap time. `comprehensive = TRUE` already
  computed the comparison and is unaffected.

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
