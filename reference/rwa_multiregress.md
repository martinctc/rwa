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
rwa_multiregress(df, outcome, predictors, applysigns = FALSE)
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

## Value

`rwa_multiregress()` returns a list of outputs, as follows:

- `predictors`: character vector of names of the predictor variables
  used.

- `rsquare`: the rsquare value of the regression model.

- `result`: the final output of the importance metrics.

  - The `Rescaled.RelWeight` column sums up to 100.

  - The `Sign` column indicates whether a predictor is positively or
    negatively correlated with the outcome.

- `n`: indicates the number of observations used in the analysis.

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
https://relativeimportance.davidson.edu/multipleregression.html for the
original implementation that inspired this package.

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
```
