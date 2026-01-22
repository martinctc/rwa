# rwa (development version)

- Added `rwa_logit()` and `rwa_multiregress()` to support logistic regression and multiple regression. 
- Added new vignette to cover the new regression methods. 
- Improved test coverage and minor bugfixes.

# rwa (Development)

## New Features

- **Missing data handling**: Added `use` parameter to `rwa()` and `rwa_multiregress()` functions to control how missing data is handled when computing correlations. Options include "pairwise.complete.obs" (default, pairwise deletion), "complete.obs" (listwise deletion), and other standard options from `cor()`. This addresses issue #8.
- **Weight variable support**: Added `weight` parameter to `rwa()` and `rwa_multiregress()` functions to perform weighted Relative Weights Analysis. When a weight variable is specified, the function computes a weighted correlation matrix using `cov.wt()`. This feature allows for proper handling of survey weights or importance weights in the analysis. This addresses issue #8.

## Improvements

- Updated bootstrap functions to support the new `use` and `weight` parameters
- Enhanced input validation to check weight variable properties (numeric, positive)

---

# rwa 0.1.1

## Improvements

- **Input validation**: Added comprehensive validation for `rwa()` parameters including `conf_level`, `n_bootstrap`, non-numeric variables, zero-variance variables, and singular correlation matrices with informative error messages
- **Code refactoring**: Consolidated duplicate RWA calculation code in bootstrap functions into a single internal helper
- **Documentation**: Added links to pkgdown site in README; fixed internal function documentation

## Bug Fixes

- Fixed flaky bootstrap test by using a fixed random seed for reproducibility (#20)

## Tests

- Added extensive tests for input validation and edge cases (collinearity, small samples, invalid parameters)

---

# rwa 0.1.0

## New Features

- **Bootstrap confidence intervals**: Added `bootstrap = TRUE` parameter to `rwa()` for statistical significance testing of relative weights
- **Result sorting**: Added `sort = TRUE` parameter to automatically sort results by importance (descending order). Set `sort = FALSE` to preserve original predictor order
- **Comprehensive vignette**: New detailed documentation covering methodology, examples, and best practices
- **Enhanced documentation**: Updated README and function documentation

## Technical Improvements  

- **Package compliance**: Updated DESCRIPTION with proper `Authors@R` field for CRAN submission
- **CI/CD**: Enhanced GitHub Actions workflow with vignette building support
- **Dependencies**: Added `boot`, `purrr`, and `utils` packages for bootstrap functionality
- **Code quality improvements**: Fixed long lines in R code to meet CRAN standards
- **Documentation cleanup**: Improved code formatting and removed unused variables
- **Enhanced vignette formatting**: Cleaned up formatting in comprehensive vignette documentation

## Bug Fixes

- Fixed vignette compilation issues
- Resolved R CMD check warnings and notes
- Removed unused variables to eliminate R CMD check notes
- Improved consistency in code formatting

---

## Version 0.0.3

Re-submission to CRAN

- Unwrap `\donttest{}` in examples where unnecessary

## Version 0.0.2

Re-submission to CRAN

- DOI references added to DESCRIPTION
- Added CodeFactor badge
- Typos in DESCRIPTION rectified

## Version 0.0.1

First submission to CRAN (required to re-submit)

- `rwa()`
- `plot_rwa()`
- `remove_all_na_cols()`
- `%>%` operator is exported

