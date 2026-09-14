# Create a Relative Weights Analysis (RWA)

This function creates a Relative Weights Analysis (RWA) and returns a
list of outputs. RWA provides a heuristic method for estimating the
relative weight of predictor variables in multiple regression, which
involves creating a multiple regression with on a set of transformed
predictors which are orthogonal to each other but maximally related to
the original set of predictors. `rwa_multiregress()` is optimised for
dplyr pipes and shows positive / negative signs for weights.

## Usage

``` r
rwa_multiregress(
  df,
  outcome,
  predictors,
  applysigns = FALSE,
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

  Logical value specifying whether to show an estimate that applies the
  sign. Defaults to `FALSE`.

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

`rwa_multiregress()` returns a list of outputs, as follows:

- `predictors`: character vector of names of the predictor variables
  used.

- `rsquare`: the rsquare value of the regression model.

- `result`: the final output of the importance metrics.

  - The `Rescaled.RelWeight` column sums up to 100.

  - The `Sign` column indicates whether a predictor is positively or
    negatively correlated with the outcome.

- `n`: complete-case observation count for the selected variables and
  weight, if supplied. Unweighted pairwise correlations may use more
  observations.

- `n_weighted`: weighted results only; sum of original weights after
  outcome, missing-weight, and predictor-completeness filters.
  Population-size meaning requires appropriately calibrated weights and
  retained population scope.

- `n_effective`: weighted results only; Kish's unequal-weighting
  effective sample size, `(sum(w)^2) / sum(w^2)`, evaluated using scaled
  weights. This is not model degrees of freedom or exact RWA precision
  and ignores clustering, stratification, and weight/outcome
  relationships.

- `lambda`: the transformation matrix that maps the original correlated
  predictors to orthogonal variables while preserving their relationship
  to the outcome. Used internally to compute relative weights.

- `RXX`: Correlation matrix of all the predictor variables against each
  other.

- `RXY`: Correlation values of the predictor variables against the
  outcome variable.

## Details

`rwa_multiregress()` produces raw relative weight values (epsilons) as
well as rescaled weights (scaled as a percentage of predictable
variance) for every predictor in the model. Signs are added to the
weights when the `applysigns` argument is set to `TRUE`. See
<https://www.scotttonidandel.com/rwa-web> for the original
implementation that inspired this package.

This is observation-weighted RWA when `weight` is provided, not a
complex-survey variance estimator. See
[`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md) for
joint-matrix and predictor positive-definiteness tolerances, bootstrap
sampling assumptions, and diagnostics. See
[`vignette("weighted-missing-data")`](https://martinctc.github.io/rwa/articles/weighted-missing-data.md)
for worked examples.

## Examples

``` r
# Basic multiple regression RWA
result <- rwa_multiregress(
  df = mtcars,
  outcome = "mpg",
  predictors = c("cyl", "disp", "hp", "wt")
)

# View the relative importance results
result$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1       cyl     0.2096904           24.70914    -
#> 2      disp     0.1883043           22.18908    -
#> 3        hp     0.1799590           21.20570    -
#> 4        wt     0.2706812           31.89607    -

# With sign information
result_signed <- rwa_multiregress(
  df = mtcars,
  outcome = "mpg",
  predictors = c("cyl", "disp", "hp", "wt"),
  applysigns = TRUE
)
result_signed$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign Sign.Rescaled.RelWeight
#> 1       cyl     0.2096904           24.70914    -               -24.70914
#> 2      disp     0.1883043           22.18908    -               -22.18908
#> 3        hp     0.1799590           21.20570    -               -21.20570
#> 4        wt     0.2706812           31.89607    -               -31.89607

# Using listwise deletion for missing data
rwa_multiregress(
  df = mtcars,
  outcome = "mpg",
  predictors = c("cyl", "disp"),
  use = "complete.obs"
)
#> $predictors
#> [1] "cyl"  "disp"
#> 
#> $rsquare
#> [1] 0.7595658
#> 
#> $result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1       cyl     0.3837012           50.51586    -
#> 2      disp     0.3758646           49.48414    -
#> 
#> $n
#> [1] 32
#> 
#> $lambda
#>           [,1]      [,2]
#> [1,] 0.8460695 0.5330725
#> [2,] 0.5330725 0.8460695
#> 
#> $RXX
#>            cyl      disp
#> cyl  1.0000000 0.9020329
#> disp 0.9020329 1.0000000
#> 
#> $RXY
#>        cyl       disp 
#> -0.8521620 -0.8475514 
#> 

# With observation weights
mtcars_weighted <- mtcars
mtcars_weighted$w <- runif(nrow(mtcars), 0.5, 2)
rwa_multiregress(
  df = mtcars_weighted,
  outcome = "mpg",
  predictors = c("cyl", "disp"),
  weight = "w"
)
#> $predictors
#> [1] "cyl"  "disp"
#> 
#> $rsquare
#> [1] 0.7722937
#> 
#> $result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1       cyl     0.3974757           51.46692    -
#> 2      disp     0.3748179           48.53308    -
#> 
#> $n
#> [1] 32
#> 
#> $n_weighted
#> [1] 39.93325
#> 
#> $n_effective
#> [1] 28.45581
#> 
#> $lambda
#>           [,1]      [,2]
#> [1,] 0.8455087 0.5339616
#> [2,] 0.5339616 0.8455087
#> 
#> $RXX
#>            cyl      disp
#> cyl  1.0000000 0.9029384
#> disp 0.9029384 1.0000000
#> 
#> $RXY
#>        cyl       disp 
#> -0.8633245 -0.8501008 
#> 
```
