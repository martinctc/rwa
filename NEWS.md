# rwa 0.1.0

## New Features

- **Bootstrap confidence intervals**: Added `bootstrap = TRUE` parameter to `rwa()` for statistical significance testing of relative weights
- **Comprehensive vignette**: New detailed documentation covering methodology, examples, and best practices
- **Enhanced documentation**: Updated README and function documentation

## Technical Improvements  

- **Package compliance**: Updated DESCRIPTION with proper `Authors@R` field for CRAN submission
- **CI/CD**: Enhanced GitHub Actions workflow with vignette building support
- **Dependencies**: Added `boot`, `purrr`, and `utils` packages for bootstrap functionality

## Bug Fixes

- Fixed vignette compilation issues
- Resolved R CMD check warnings and notes

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

