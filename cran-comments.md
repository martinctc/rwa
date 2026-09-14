# CRAN Submission Comments for rwa v0.2.0

### Test environments

* Current candidate: local Windows 11 x64 (build 26200), R 4.6.1
  (2026-06-24 ucrt), checked on 2026-09-14.
* Historical GitHub Actions checks (Ubuntu, R release) predate these changes.
  Current-candidate CI is reported separately on PR #22 and is not included in
  the local results below.

### R CMD check results

0 errors | 0 warnings | 0 notes

The current local candidate passed
`devtools::check(document = FALSE, error_on = "warning", manual = FALSE)`,
using `--no-manual --as-cran`. Package installation, examples (including
`--run-donttest`), tests, and vignette builds/rebuilds passed.
Remote CRAN incoming checks were disabled by the local check defaults;
the PDF manual, reverse dependencies, and other platforms were not checked.

### Changes in this version

* Added weighted relative weights analysis through the new `weight` argument.
* Added configurable missing-data handling through the new `use` argument.
* Updated bootstrap routines to support both new arguments.
* Added weighted-only retained weight totals and Kish effective sample-size
  diagnostics to both exported multiple-regression paths.
* Shared data preparation and matrix validation prevent invalid joint matrices,
  singular predictor models, and silently shortened bootstrap statistics.
* A separate fit-validity check catches impossible R-squared values even when
  joint-matrix errors are small. No additional conditioning cutoff is imposed
  on previously estimable, highly correlated predictors.
* Fixed single-predictor weighting, random-variable name collisions, and
  comprehensive comparison labels, including random comparisons without a focal
  predictor.
* Documented legacy missing-data filtering and iid individual-row bootstrap
  scope, without claiming general complex-survey variance support.
* Added validation, tests, and documentation for the new functionality.
* Internal refactoring and documentation improvements.

### Additional checks

* `spelling::spell_check_package()` completed with no remaining spelling
  findings after updating the package word list and correcting NEWS wording.
* The focused point/bootstrap tests passed 392 assertions in 73 test blocks.
  Twenty-one new contract tests passed 255 assertions without warnings. Existing
  tests emitted 30 diagnostic warnings (small samples, bootstrap endpoint
  estimates, and logistic fits); these were not R CMD check warnings.
* No reverse dependency checks were run for this release.
* Independent review of the agreed PR scope completed after resolving the
  numerical-boundary findings. Previously estimable models remain supported
  without an additional conditioning cutoff.
* [Raw.Significant inference (#26)](https://github.com/martinctc/rwa/issues/26)
  remains a separate release concern;
  [sign interpretation (#27)](https://github.com/martinctc/rwa/issues/27) is a
  deferred follow-up. Neither interpretation was changed in this PR.
  These results are not a CRAN submission or statistical-method validation.
