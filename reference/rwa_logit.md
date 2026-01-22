# Create a Relative Weights Analysis with logistic regression

This function performs Relative Weights Analysis (RWA) for binary
outcome variables using logistic regression. RWA provides a method for
estimating the relative importance of predictor variables by
transforming them into orthogonal variables while preserving their
relationship to the outcome. This implementation follows Johnson (2000)
for logistic regression.

## Usage

``` r
rwa_logit(df, outcome, predictors, applysigns = FALSE)
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

`rwa_logit()` returns a list of outputs, as follows:

- `predictors`: character vector of names of the predictor variables
  used.

- `rsquare`: the pseudo R-squared value (sum of epsilon weights) for the
  logistic regression model.

- `result`: the final output of the importance metrics.

  - The `Rescaled.RelWeight` column sums up to 100.

  - The `Sign` column indicates whether a predictor is positively or
    negatively associated with the outcome.

- `n`: indicates the number of observations used in the analysis.

- `lambda`: the Lambda transformation matrix from the analysis.

## Examples

``` r
# Create a binary outcome variable
mtcars_binary <- mtcars
mtcars_binary$high_mpg <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 0)

# Basic logistic RWA
result <- rwa_logit(
  df = mtcars_binary,
  outcome = "high_mpg",
  predictors = c("cyl", "disp", "hp", "wt")
)
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

# View the relative importance results
result$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1       cyl      7.296847           22.22380    -
#> 2      disp      7.343709           22.36653    -
#> 3        hp      4.937049           15.03663    -
#> 4        wt     13.255876           40.37304    -

# With sign information
result_signed <- rwa_logit(
  df = mtcars_binary,
  outcome = "high_mpg",
  predictors = c("cyl", "disp", "hp", "wt"),
  applysigns = TRUE
)
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
result_signed$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign      Sign
#> 1       cyl      7.296847           22.22380    - -22.22380
#> 2      disp      7.343709           22.36653    - -22.36653
#> 3        hp      4.937049           15.03663    - -15.03663
#> 4        wt     13.255876           40.37304    - -40.37304
```
