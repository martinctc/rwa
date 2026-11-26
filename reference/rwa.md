# Create a Relative Weights Analysis (RWA)

This function creates a Relative Weights Analysis (RWA) and returns a
list of outputs. RWA provides a heuristic method for estimating the
relative weight of predictor variables in multiple regression, which
involves creating a multiple regression with on a set of transformed
predictors which are orthogonal to each other but maximally related to
the original set of predictors. `rwa()` is optimised for dplyr pipes and
can return sign annotations for weights.

## Usage

``` r
rwa(
  df,
  outcome,
  predictors,
  applysigns = FALSE,
  method = "auto",
  sort = TRUE,
  bootstrap = FALSE,
  n_bootstrap = 1000,
  conf_level = 0.95,
  focal = NULL,
  comprehensive = FALSE,
  include_rescaled_ci = FALSE,
  use = "pairwise.complete.obs",
  weight = NULL
)
```

## Arguments

- df:

  Data frame or tibble to be passed through.

- outcome:

  Outcome variable, to be specified as a string or bare input. Must be a
  numeric variable.

- predictors:

  Predictor variable(s), to be specified as a vector of string(s) or
  bare input(s). All variables must be numeric.

- applysigns:

  Logical value specifying whether to return `Sign.Rescaled.RelWeight`,
  the rescaled relative weight negated when the
  orthogonalized-coefficient sign is `"-"`. Defaults to `FALSE`.

- method:

  String to specify the method of regression to apply. Valid values
  include:

  - `"auto"`: automatically detect whether to use multiple regression or
    logistic regression based on the outcome variable provided.

  - `"multiple"`: use multiple regression.

  - `"logistic"`: use logistic regression.

- sort:

  Logical value specifying whether to sort results by rescaled relative
  weights in descending order. Defaults to `TRUE`.

- bootstrap:

  Logical value specifying whether to calculate bootstrap confidence
  intervals. Defaults to `FALSE`. Currently only supported for multiple
  regression.

- n_bootstrap:

  Number of bootstrap samples to use when bootstrap = TRUE. Defaults to
  1000, the number used by Tonidandel, LeBreton and Johnson (2009, p.
  392); RWA-Web (Tonidandel & LeBreton, 2015) uses 10,000 by default.
  Intervals are bias-corrected and accelerated (BCa), as recommended by
  Tonidandel et al. (2009, p. 392). When `n_bootstrap` is smaller than
  the number of rows with a non-missing outcome, the BCa acceleration is
  estimated from jackknife influence values (Efron, 1987). This is quick
  unless pairwise deletion is used with missing values, in which case
  each jackknife sample is refitted. Check `ci_method` in the returned
  `ci_results`; any fallback from BCa is reported with a warning.

- conf_level:

  Confidence level for bootstrap intervals. Defaults to 0.95.

- focal:

  Focal variable for bootstrap comparisons (optional).

- comprehensive:

  Whether to run comprehensive bootstrap analysis including random
  variable comparisons and, when `focal` is supplied, comparisons
  against that predictor.

- include_rescaled_ci:

  Logical value specifying whether to include confidence intervals for
  rescaled weights. Defaults to `FALSE` due to compositional data
  constraints. Use with caution.

- use:

  Method for handling missing data when computing correlations. Options
  are: "everything" (remaining missing values propagate, causing a
  non-finite correlation matrix error if correlations cannot be
  estimated), "all.obs" (error for remaining missing predictors in
  unweighted analysis), "complete.obs" (listwise deletion),
  "na.or.complete" (listwise deletion; no complete cases produces an
  insufficient-data error rather than an unusable matrix of NAs),
  "pairwise.complete.obs" (pairwise deletion, default). See
  [`cor`](https://rdrr.io/r/stats/cor.html) for more details. Only
  applicable for multiple regression. Rows with missing outcomes are
  always removed first, including for "all.obs". When `weight` is
  specified, remaining missing weights cause an error for "all.obs" and
  are removed otherwise. Missing predictors are then removed by listwise
  deletion for every weighted mode, including "all.obs". Thus weighted
  correlations always use complete cases, regardless of `use`; weighted
  pairwise correlation is not implemented.

- weight:

  Optional name of a weight variable in the data frame. If provided, a
  weighted correlation matrix will be computed using the specified
  weights. Non-missing weights must be numeric, finite, and strictly
  positive (zero weights are not supported). Missing weights follow the
  `use` rules. Defaults to `NULL` (unweighted analysis). Only applicable
  for multiple regression.

## Value

`rwa()` returns a list of outputs, as follows:

- `predictors`: character vector of names of the predictor variables
  used.

- `rsquare`: the rsquare value of the regression model (multiple
  regression only).

- `result`: the final output of the importance metrics (sorted by
  Rescaled.RelWeight in descending order by default).

  - The `Rescaled.RelWeight` column sums up to 100.

  - The `Sign` column gives the sign of the coefficient for the
    corresponding orthogonalized predictor. It is not the sign of the
    original predictor's marginal association or original-scale
    regression coefficient.

  - When `applysigns = TRUE`, `Sign.Rescaled.RelWeight` is the rescaled
    relative weight negated when `Sign` is `"-"`; it is a signed
    importance score, not a regression coefficient or causal effect.

  - When bootstrap = TRUE, includes confidence interval columns for raw
    weights.

  - When bootstrap = TRUE, `Random.Diff.CI.Lower`/`Random.Diff.CI.Upper`
    give the interval for the difference between each predictor's weight
    and the weight of a randomly generated variable, and
    `Raw.Significant` is `TRUE` when `Random.Diff.CI.Lower` is above
    zero. Significance is assessed this way, and not from the interval
    around the weight itself, because raw relative weights are
    non-negative: an unrelated predictor still receives a small positive
    weight, so an interval around it would almost always exclude zero.
    Comparing each weight to that of a randomly generated variable is
    the approach suggested by Tonidandel, LeBreton and Johnson (2009;
    [doi:10.1037/a0017735](https://doi.org/10.1037/a0017735) ) as a way
    to judge whether a weight exceeds what chance alone would produce.
    The `Raw.Significant` cutoff applied here is directional: a
    predictor is significant only when it exceeds the random variable
    (`Random.Diff.CI.Lower > 0`), because an interval lying entirely
    below zero indicates the predictor performed worse than noise, which
    is not evidence of importance in either direction.

  - Rescaled weight CIs are available via include_rescaled_ci = TRUE but
    not recommended for inference.

- `n`: complete-case observation count for the selected analysis
  variables (and weight, if supplied). Unweighted pairwise correlations
  may use more observations than this conservative count.

- `n_weighted`: weighted results only; sum of original weights after all
  analysis filters. This is a population-size estimate only for
  appropriately calibrated weights and the retained population scope.

- `n_effective`: weighted results only; Kish's unequal-weighting
  effective sample size, `(sum(w)^2) / sum(w^2)`, calculated using
  scaled weights for numerical stability. This diagnostic ignores
  clustering, stratification, and weight/outcome relationships; it is
  not model degrees of freedom or the exact precision of RWA.

- `bootstrap`: bootstrap results (only present when bootstrap = TRUE),
  containing:

  - `ci_results`: confidence intervals for weights

  - `boot_object`: raw bootstrap object for advanced analysis

  - `n_bootstrap`: number of bootstrap samples used

- `lambda`: lambda matrix from the RWA calculation.

- `RXX`: Correlation matrix of all the predictor variables against each
  other. Not available for logistic regression.

- `RXY`: Correlation values of the predictor variables against the
  outcome variable. Not available for logistic regression.

## Details

`rwa()` produces raw relative weight values (epsilons) as well as
rescaled weights (scaled as a percentage of predictable variance) for
every predictor in the model. Signs are added to the weights when the
`applysigns` argument is set to `TRUE`. See
<https://www.scotttonidandel.com/rwa-web> for the original
implementation that inspired this package.

The `Sign` column uses the coefficient sign for the orthogonalized
predictor in the RWA calculation. Because orthogonalized predictors are
transformations of the originals, this sign can differ from an original
predictor's marginal association with the outcome. It is not the
coefficient sign from an ordinary regression on the original predictors.
`Sign.Rescaled.RelWeight` (when requested with `applysigns = TRUE`) is
the rescaled weight negated when `Sign` is `"-"`. It is a signed
importance score, not a regression coefficient or a causal effect. With
`weight`, the same convention applies to coefficients based on the
weighted correlation matrix. See
[`vignette("regression-methods")`](https://martinctc.github.io/rwa/articles/regression-methods.md)
for reproducible examples.

This function is a wrapper around
[`rwa_multiregress()`](https://martinctc.github.io/rwa/reference/rwa_multiregress.md)
and
[`rwa_logit()`](https://martinctc.github.io/rwa/reference/rwa_logit.md),
automatically selecting the appropriate method based on the outcome
variable or the `method` argument.

In brief, for the two missing-data and weighting arguments:

- Without `weight`, missing values are handled by `use`, which defaults
  to pairwise deletion when correlating predictors.

- With `weight`, the analysis always uses complete cases across the
  outcome, the predictors, and the weight. Weighted pairwise deletion is
  not implemented, so `use` does not change a weighted result.

- In both cases, rows with a missing outcome are dropped first.

Use weights when the analysis should represent a target population
rather than the achieved sample. Comparing weighted with unweighted
results is informative: a large difference indicates that sample
composition matters.

Multiple-regression estimates require a finite joint correlation matrix.
Its smallest eigenvalue must be at least minus
`sqrt(.Machine$double.eps) * max(1, max(abs(eigenvalues)))`. The
predictor block must have strictly positive computed eigenvalues and the
transformation must be solvable. No additional conditioning cutoff is
imposed on previously estimable models; highly correlated predictors can
still yield sensitive estimates. Calculated R-squared must not exceed
one by more than `sqrt(.Machine$double.eps)`; the fit is checked
separately because small matrix errors can be amplified by nearly
collinear predictors. An exactly fitted outcome (a singular joint
matrix) is allowed when the predictor block is positive definite.
Invalid matrices, constant variables, and insufficient observations
cause informative errors; variables are not dropped and matrices are not
silently repaired.

Weighted analysis is observation-weighted RWA. Bootstrap intervals use
independent, identically distributed (iid) individual-row resampling,
with each row's original weight carried along, not sampling proportional
to weights. Rows with missing outcomes are removed before resampling;
other missing-data filters are applied within each sample, preserving
the outcome-complete sampling frame. A degenerate sample stops the
bootstrap with an error: samples are not skipped, retried, or allowed to
lose predictors. Clusters, strata, and replicate-weight survey designs
are not supported; a weight column alone does not provide general
complex-survey variance. See
[`vignette("weighted-missing-data")`](https://martinctc.github.io/rwa/articles/weighted-missing-data.md)
for examples and limitations.

## References

Efron, B. (1987). Better bootstrap confidence intervals. *Journal of the
American Statistical Association*, 82(397), 171-185.
[doi:10.1080/01621459.1987.10478410](https://doi.org/10.1080/01621459.1987.10478410)

Johnson, J. W. (2000). A heuristic method for estimating the relative
weight of predictor variables in multiple regression. *Multivariate
Behavioral Research*, 35(1), 1-19.
[doi:10.1207/S15327906MBR3501_1](https://doi.org/10.1207/S15327906MBR3501_1)

Tonidandel, S., LeBreton, J. M., & Johnson, J. W. (2009). Determining
the statistical significance of relative weights. *Psychological
Methods*, 14(4), 387-399.
[doi:10.1037/a0017735](https://doi.org/10.1037/a0017735)

Tonidandel, S., & LeBreton, J. M. (2015). RWA Web: A free,
comprehensive, web-based, and user-friendly tool for relative weight
analyses. *Journal of Business and Psychology*, 30(2), 207-216.
[doi:10.1007/s10869-014-9351-z](https://doi.org/10.1007/s10869-014-9351-z)

## See also

[`plot_rwa()`](https://martinctc.github.io/rwa/reference/plot_rwa.md)
for plotting results,
[`rwa_multiregress()`](https://martinctc.github.io/rwa/reference/rwa_multiregress.md)
and
[`rwa_logit()`](https://martinctc.github.io/rwa/reference/rwa_logit.md)
for the underlying implementations.

## Examples

``` r
library(ggplot2)
# Basic RWA (results sorted by default)
rwa(diamonds, "price", c("depth", "carat"))
#> Parsing `price` as a non-binary variable.
#> Applying multiple regression to calculate relative weights...
#> $predictors
#> [1] "depth" "carat"
#> 
#> $rsquare
#> [1] 0.8506755
#> 
#> $result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1     carat   0.849946308        99.91428588    +
#> 2     depth   0.000729149         0.08571412    -
#> 
#> $n
#> [1] 53940
#> 
#> $lambda
#>            [,1]       [,2]
#> [1,] 0.99990040 0.01411356
#> [2,] 0.01411356 0.99990040
#> 
#> $RXX
#>            depth      carat
#> depth 1.00000000 0.02822431
#> carat 0.02822431 1.00000000
#> 
#> $RXY
#>      depth      carat 
#> -0.0106474  0.9215913 
#> 

# RWA without sorting (preserves original predictor order)
rwa(diamonds, "price", c("depth", "carat"), sort = FALSE)
#> Parsing `price` as a non-binary variable.
#> Applying multiple regression to calculate relative weights...
#> $predictors
#> [1] "depth" "carat"
#> 
#> $rsquare
#> [1] 0.8506755
#> 
#> $result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1     depth   0.000729149         0.08571412    -
#> 2     carat   0.849946308        99.91428588    +
#> 
#> $n
#> [1] 53940
#> 
#> $lambda
#>            [,1]       [,2]
#> [1,] 0.99990040 0.01411356
#> [2,] 0.01411356 0.99990040
#> 
#> $RXX
#>            depth      carat
#> depth 1.00000000 0.02822431
#> carat 0.02822431 1.00000000
#> 
#> $RXY
#>      depth      carat 
#> -0.0106474  0.9215913 
#> 

# Plot results using plot_rwa()
diamonds |>
  rwa("price", c("depth", "carat", "x", "y")) |>
  plot_rwa()
#> Parsing `price` as a non-binary variable.
#> Applying multiple regression to calculate relative weights...


# \donttest{
# For faster examples, use a subset of data for bootstrap
diamonds_small <- diamonds[sample(nrow(diamonds), 1000), ]

# RWA with different missing data handling
# Use complete.obs for listwise deletion
rwa(diamonds_small, "price", c("depth", "carat"), use = "complete.obs")
#> Parsing `price` as a non-binary variable.
#> Applying multiple regression to calculate relative weights...
#> $predictors
#> [1] "depth" "carat"
#> 
#> $rsquare
#> [1] 0.8506213
#> 
#> $result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1     carat  0.8496668919         99.8878042    +
#> 2     depth  0.0009543612          0.1121958    -
#> 
#> $n
#> [1] 1000
#> 
#> $lambda
#>           [,1]      [,2]
#> [1,] 0.9999021 0.0139908
#> [2,] 0.0139908 0.9999021
#> 
#> $RXX
#>            depth      carat
#> depth 1.00000000 0.02797885
#> carat 0.02797885 1.00000000
#> 
#> $RXY
#>       depth       carat 
#> -0.01517396  0.92138091 
#> 

# RWA with weights
diamonds_small$sample_weight <- runif(nrow(diamonds_small), 0.5, 2)
rwa(diamonds_small, "price", c("depth", "carat"), weight = "sample_weight")
#> Parsing `price` as a non-binary variable.
#> Applying multiple regression to calculate relative weights...
#> $predictors
#> [1] "depth" "carat"
#> 
#> $rsquare
#> [1] 0.8538196
#> 
#> $result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1     carat  0.8530214942        99.90652809    +
#> 2     depth  0.0007980815         0.09347191    -
#> 
#> $n
#> [1] 1000
#> 
#> $n_weighted
#> [1] 1258.593
#> 
#> $n_effective
#> [1] 895.036
#> 
#> $lambda
#>            [,1]       [,2]
#> [1,] 0.99987652 0.01571459
#> [2,] 0.01571459 0.99987652
#> 
#> $RXX
#>            depth      carat
#> depth 1.00000000 0.03142531
#> carat 0.03142531 1.00000000
#> 
#> $RXY
#>        depth        carat 
#> -0.009720205  0.923210645 
#> 

# RWA with bootstrap confidence intervals (raw weights only)
rwa(diamonds_small, "price", c("depth", "carat"),
    bootstrap = TRUE, n_bootstrap = 100)
#> Parsing `price` as a non-binary variable.
#> Applying multiple regression to calculate relative weights...
#> Running bootstrap analysis with 100 samples...
#> Warning: extreme order statistics used as endpoints
#> Warning: extreme order statistics used as endpoints
#> $predictors
#> [1] "depth" "carat"
#> 
#> $rsquare
#> [1] 0.8506213
#> 
#> $result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign Raw.RelWeight.CI.Lower
#> 1     carat  0.8496668919         99.8878042    +           0.8089068986
#> 2     depth  0.0009543612          0.1121958    -           0.0001541835
#>   Raw.RelWeight.CI.Upper Random.Diff.CI.Lower Random.Diff.CI.Upper
#> 1            0.871656887          0.807330298          0.866078279
#> 2            0.002043556         -0.004911839          0.002905542
#>   Raw.Significant
#> 1            TRUE
#> 2           FALSE
#> 
#> $n
#> [1] 1000
#> 
#> $lambda
#>           [,1]      [,2]
#> [1,] 0.9999021 0.0139908
#> [2,] 0.0139908 0.9999021
#> 
#> $RXX
#>            depth      carat
#> depth 1.00000000 0.02797885
#> carat 0.02797885 1.00000000
#> 
#> $RXY
#>       depth       carat 
#> -0.01517396  0.92138091 
#> 
#> $bootstrap
#> $bootstrap$boot_object
#> 
#> ORDINARY NONPARAMETRIC BOOTSTRAP
#> 
#> 
#> Call:
#> boot::boot(data = bootstrap_data, statistic = rwa_boot_statistic, 
#>     R = n_bootstrap, outcome = outcome, predictors = predictors, 
#>     use = use, weight_var = weight)
#> 
#> 
#> Bootstrap Statistics :
#>         original        bias     std. error
#> t1* 0.0009543612  0.0003926974 0.0007904312
#> t2* 0.8496668919 -0.0005076164 0.0144265478
#> 
#> $bootstrap$boot_object_random
#> 
#> ORDINARY NONPARAMETRIC BOOTSTRAP
#> 
#> 
#> Call:
#> boot::boot(data = random_data, statistic = rwa_boot_rand_statistic, 
#>     R = n_bootstrap, outcome = outcome, predictors = predictors, 
#>     use = use, weight_var = weight, random_var = random_name)
#> 
#> 
#> Bootstrap Statistics :
#>         original        bias    std. error
#> t1* 0.0004410546 -0.0001340805 0.001692648
#> t2* 0.8486712093  0.0014463693 0.013004533
#> 
#> $bootstrap$ci_results
#> $bootstrap$ci_results$raw_weights
#> # A tibble: 2 × 6
#>   variable weight_index ci_lower ci_upper ci_method ci_type
#>   <chr>           <int>    <dbl>    <dbl> <chr>     <chr>  
#> 1 depth               1 0.000154  0.00204 bca       raw    
#> 2 carat               2 0.809     0.872   bca       raw    
#> 
#> $bootstrap$ci_results$random_comparison
#> # A tibble: 2 × 6
#>   variable weight_index ci_lower ci_upper ci_method ci_type  
#>   <chr>           <int>    <dbl>    <dbl> <chr>     <chr>    
#> 1 depth               1 -0.00491  0.00291 bca       rand_diff
#> 2 carat               2  0.807    0.866   bca       rand_diff
#> 
#> 
#> $bootstrap$n_bootstrap
#> [1] 100
#> 
#> $bootstrap$conf_level
#> [1] 0.95
#> 
#> $bootstrap$comprehensive
#> [1] FALSE
#> 
#> $bootstrap$focal
#> NULL
#> 
#> 

# Include rescaled weight CIs (use with caution for inference)
rwa(diamonds_small, "price", c("depth", "carat"),
    bootstrap = TRUE, include_rescaled_ci = TRUE, n_bootstrap = 100)
#> Parsing `price` as a non-binary variable.
#> Applying multiple regression to calculate relative weights...
#> Running bootstrap analysis with 100 samples...
#> Warning: extreme order statistics used as endpoints
#> Warning: extreme order statistics used as endpoints
#> Warning: extreme order statistics used as endpoints
#> Warning: extreme order statistics used as endpoints
#> Warning: Rescaled weight confidence intervals should be interpreted with caution due to compositional data constraints. Use for descriptive purposes only, not formal statistical inference.
#> $predictors
#> [1] "depth" "carat"
#> 
#> $rsquare
#> [1] 0.8506213
#> 
#> $result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign Raw.RelWeight.CI.Lower
#> 1     carat  0.8496668919         99.8878042    +           0.8199449572
#> 2     depth  0.0009543612          0.1121958    -           0.0001303366
#>   Raw.RelWeight.CI.Upper Random.Diff.CI.Lower Random.Diff.CI.Upper
#> 1            0.870668876          0.812852936          0.868888450
#> 2            0.002466696         -0.004261283          0.002481041
#>   Raw.Significant Rescaled.RelWeight.CI.Lower Rescaled.RelWeight.CI.Upper
#> 1            TRUE                 99.71964602                   99.975117
#> 2           FALSE                  0.02488297                    0.280354
#> 
#> $n
#> [1] 1000
#> 
#> $lambda
#>           [,1]      [,2]
#> [1,] 0.9999021 0.0139908
#> [2,] 0.0139908 0.9999021
#> 
#> $RXX
#>            depth      carat
#> depth 1.00000000 0.02797885
#> carat 0.02797885 1.00000000
#> 
#> $RXY
#>       depth       carat 
#> -0.01517396  0.92138091 
#> 
#> $bootstrap
#> $bootstrap$boot_object
#> 
#> ORDINARY NONPARAMETRIC BOOTSTRAP
#> 
#> 
#> Call:
#> boot::boot(data = bootstrap_data, statistic = rwa_boot_statistic, 
#>     R = n_bootstrap, outcome = outcome, predictors = predictors, 
#>     use = use, weight_var = weight)
#> 
#> 
#> Bootstrap Statistics :
#>         original       bias    std. error
#> t1* 0.0009543612 0.0004161429 0.001015778
#> t2* 0.8496668919 0.0003876215 0.013084402
#> 
#> $bootstrap$boot_object_rescaled
#> 
#> ORDINARY NONPARAMETRIC BOOTSTRAP
#> 
#> 
#> Call:
#> boot::boot(data = bootstrap_data, statistic = rwa_boot_statistic_rescaled, 
#>     R = n_bootstrap, outcome = outcome, predictors = predictors, 
#>     use = use, weight_var = weight)
#> 
#> 
#> Bootstrap Statistics :
#>       original      bias    std. error
#> t1*  0.1121958  0.06479356   0.1091478
#> t2* 99.8878042 -0.06479356   0.1091478
#> 
#> $bootstrap$boot_object_random
#> 
#> ORDINARY NONPARAMETRIC BOOTSTRAP
#> 
#> 
#> Call:
#> boot::boot(data = random_data, statistic = rwa_boot_rand_statistic, 
#>     R = n_bootstrap, outcome = outcome, predictors = predictors, 
#>     use = use, weight_var = weight, random_var = random_name)
#> 
#> 
#> Bootstrap Statistics :
#>         original        bias    std. error
#> t1* 0.0002910965 -6.043971e-05 0.001410464
#> t2* 0.8483740012 -1.990434e-03 0.014143075
#> 
#> $bootstrap$ci_results
#> $bootstrap$ci_results$raw_weights
#> # A tibble: 2 × 6
#>   variable weight_index ci_lower ci_upper ci_method ci_type
#>   <chr>           <int>    <dbl>    <dbl> <chr>     <chr>  
#> 1 depth               1 0.000130  0.00247 bca       raw    
#> 2 carat               2 0.820     0.871   bca       raw    
#> 
#> $bootstrap$ci_results$rescaled_weights
#> # A tibble: 2 × 6
#>   variable weight_index ci_lower ci_upper ci_method ci_type 
#>   <chr>           <int>    <dbl>    <dbl> <chr>     <chr>   
#> 1 depth               1   0.0249    0.280 bca       rescaled
#> 2 carat               2  99.7     100.0   bca       rescaled
#> 
#> $bootstrap$ci_results$random_comparison
#> # A tibble: 2 × 6
#>   variable weight_index ci_lower ci_upper ci_method ci_type  
#>   <chr>           <int>    <dbl>    <dbl> <chr>     <chr>    
#> 1 depth               1 -0.00426  0.00248 bca       rand_diff
#> 2 carat               2  0.813    0.869   bca       rand_diff
#> 
#> 
#> $bootstrap$n_bootstrap
#> [1] 100
#> 
#> $bootstrap$conf_level
#> [1] 0.95
#> 
#> $bootstrap$comprehensive
#> [1] FALSE
#> 
#> $bootstrap$focal
#> NULL
#> 
#> 

# Comprehensive bootstrap analysis with focal variable
result <- rwa(diamonds_small, "price", c("depth", "carat", "table"),
              bootstrap = TRUE, comprehensive = TRUE, focal = "carat",
              n_bootstrap = 100)
#> Parsing `price` as a non-binary variable.
#> Applying multiple regression to calculate relative weights...
#> Running bootstrap analysis with 100 samples...
#> Warning: extreme order statistics used as endpoints
# View confidence intervals
result$bootstrap$ci_results
#> $raw_weights
#> # A tibble: 3 × 6
#>   variable weight_index ci_lower ci_upper ci_method ci_type
#>   <chr>           <int>    <dbl>    <dbl> <chr>     <chr>  
#> 1 depth               1 0.000307  0.00271 bca       raw    
#> 2 carat               2 0.809     0.873   bca       raw    
#> 3 table               3 0.00271   0.0158  bca       raw    
#> 
#> $random_comparison
#> # A tibble: 3 × 6
#>   variable weight_index  ci_lower ci_upper ci_method ci_type  
#>   <chr>           <int>     <dbl>    <dbl> <chr>     <chr>    
#> 1 depth               1 -0.00584   0.00357 bca       rand_diff
#> 2 carat               2  0.805     0.866   bca       rand_diff
#> 3 table               3  0.000723  0.0186  bca       rand_diff
#> 
#> $focal_comparison
#> # A tibble: 2 × 6
#>   variable weight_index ci_lower ci_upper ci_method ci_type   
#>   <chr>           <int>    <dbl>    <dbl> <chr>     <chr>     
#> 1 depth               1   -0.871   -0.804 bca       focal_diff
#> 2 table               2   -0.861   -0.792 bca       focal_diff
#> 
# }

# Based on logistic regression (auto-detected from binary outcome)
diamonds$IsIdeal <- as.numeric(diamonds$cut == "Ideal")
rwa(diamonds, "IsIdeal", c("depth", "carat"))
#> Parsing `IsIdeal` as a binary variable.
#> Applying logistic regression to calculate relative weights...
#> $predictors
#> [1] "depth" "carat"
#> 
#> $rsquare
#> [1] 0.02835802
#> 
#> $result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1     carat  0.0279360368          98.511958    -
#> 2     depth  0.0004219793           1.488042    -
#> 
#> $n
#> [1] 53940
#> 
#> $lambda
#>           depth      carat
#> [1,] 0.99990040 0.01411356
#> [2,] 0.01411356 0.99990040
#> 
```
