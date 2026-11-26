# Plot the rescaled importance values from the output of `rwa()`

Pass the output of
[`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md) and plot a
bar chart of the signed rescaled importance values. The sign follows the
coefficient for the orthogonalized predictor in the RWA calculation; it
can differ from the original predictor's marginal association with the
outcome. The signed score is not a regression coefficient or causal
effect.

## Usage

``` r
plot_rwa(rwa)
```

## Arguments

- rwa:

  Direct list output from
  [`rwa()`](https://martinctc.github.io/rwa/reference/rwa.md).

## Examples

``` r

library(ggplot2)
# Use a smaller sample for faster execution
diamonds_small <- diamonds[sample(nrow(diamonds), 1000), ]
diamonds_small %>%
  rwa(outcome = "price",
      predictors = c("depth","carat", "x", "y", "z"),
      applysigns = TRUE) %>%
  plot_rwa()
#> Parsing `price` as a non-binary variable.
#> Applying multiple regression to calculate relative weights...


```
