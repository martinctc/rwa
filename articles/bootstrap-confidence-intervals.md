# Bootstrap Confidence Intervals for Relative Weights Analysis

``` r

library(rwa)
library(dplyr)
library(ggplot2)
```

## Introduction

Bootstrap confidence intervals represent a major advancement in Relative
Weights Analysis, addressing a long-standing methodological limitation.
This vignette provides comprehensive guidance on using bootstrap methods
with the `rwa` package for statistical significance testing of predictor
importance.

## Why Bootstrap for RWA?

### The Statistical Challenge

As noted by Tonidandel et al. (2009):

> “The difficulty in determining the statistical significance of
> relative weights stems from the fact that the exact (or small sample)
> sampling distribution of relative weights is unknown.”

Traditional RWA provides point estimates of relative importance but
lacks a framework for statistical inference. Bootstrap methods solve
this by empirically estimating the sampling distribution of relative
weights.

### Bootstrap Solution

Bootstrap resampling: 1. **Creates multiple samples** from your original
data 2. **Calculates RWA** for each bootstrap sample  
3. **Estimates confidence intervals** from the distribution of bootstrap
results 4. **Enables significance testing** by comparing each predictor
against a randomly generated variable

### How significance is assessed

A relative weight **cannot be tested against zero**. Raw relative
weights are non-negative, so a predictor with no real relationship to
the outcome still receives a small positive weight, and an interval
around that weight will almost always exclude zero.

Following Tonidandel, LeBreton and Johnson (2009) — who suggest
comparing a weight against that of a randomly generated variable to
judge whether it exceeds what chance alone would produce (see also the
discussion in
[`vignette("evaluating-rwa-method-reference")`](https://martinctc.github.io/rwa/articles/evaluating-rwa-method-reference.md))
— significance is instead assessed by adding a **randomly generated
variable** to the model and bootstrapping the *difference* between each
predictor’s weight and the random variable’s weight.

The directional cutoff applied by this package is: a predictor is
flagged significant only when the **lower bound** of that difference
interval is above zero, meaning it explains meaningfully more variance
than noise would. An interval lying entirely *below* zero means the
predictor performed worse than the random variable, which is equally not
evidence of importance, so it is not flagged significant either.

This means the package reports two distinct intervals:

| Columns | Purpose |
|----|----|
| `Raw.RelWeight.CI.Lower` / `.Upper` | Descriptive interval around the weight itself. **Not** a significance test. |
| `Random.Diff.CI.Lower` / `.Upper` | Difference from a random variable’s weight. `Raw.Significant` is `TRUE` when `Random.Diff.CI.Lower > 0`. |

## Basic Bootstrap Analysis

### Sampling scope

The implementation uses independent, identically distributed (iid)
individual-row resampling. For observation-weighted RWA, each sampled
row carries its original weight; clusters, strata, and replicate-weight
survey designs are not supported. Outcome-missing rows are removed
before resampling, while other missing-data filters are applied within
each sample. Invalid samples stop with an error rather than dropping
predictors or retrying. See
[`vignette("weighted-missing-data")`](https://martinctc.github.io/rwa/articles/weighted-missing-data.md)
for the filtering contract and examples.

### Simple Bootstrap Example

``` r

# Bootstrap analysis with 1000 samples
result_bootstrap <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp", "gear"),
      bootstrap = TRUE,
      n_bootstrap = 1000,
      conf_level = 0.95)

# View results with confidence intervals
result_bootstrap$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign Raw.RelWeight.CI.Lower
#> 1        hp     0.2321744           29.79691    -             0.18864625
#> 2       cyl     0.2284797           29.32274    -             0.17336836
#> 3      disp     0.2221469           28.50999    -             0.15772412
#> 4      gear     0.0963886           12.37037    +             0.04155014
#>   Raw.RelWeight.CI.Upper Random.Diff.CI.Lower Random.Diff.CI.Upper
#> 1              0.2811493           0.18517981            0.3238297
#> 2              0.2788206           0.17457756            0.3011385
#> 3              0.2804741           0.15417064            0.2962516
#> 4              0.1843592           0.03300665            0.2160141
#>   Raw.Significant
#> 1            TRUE
#> 2            TRUE
#> 3            TRUE
#> 4            TRUE
```

### Understanding Bootstrap Output

The bootstrap analysis enhances the standard RWA output with:

- **Raw.RelWeight.CI.Lower/Upper**: confidence intervals around each raw
  weight, for description rather than testing
- **Random.Diff.CI.Lower/Upper**: confidence intervals for the
  difference from a randomly generated variable’s weight
- **Raw.Significant**: significance flag, `TRUE` when the
  random-comparison interval excludes zero

``` r

# Bootstrap-specific information
cat("Bootstrap samples used:", result_bootstrap$bootstrap$n_bootstrap, "\n")
#> Bootstrap samples used: 1000

# Detailed CI information
print(result_bootstrap$bootstrap$ci_results$raw_weights)
#> # A tibble: 4 × 6
#>   variable weight_index ci_lower ci_upper ci_method ci_type
#>   <chr>           <int>    <dbl>    <dbl> <chr>     <chr>  
#> 1 cyl                 1   0.173     0.279 bca       raw    
#> 2 disp                2   0.158     0.280 bca       raw    
#> 3 hp                  3   0.189     0.281 bca       raw    
#> 4 gear                4   0.0416    0.184 bca       raw

# Identify significant predictors
significant_vars <- result_bootstrap$result %>%
  filter(Raw.Significant == TRUE) %>%
  pull(Variables)

cat("Significant predictors:", paste(significant_vars, collapse = ", "))
#> Significant predictors: hp, cyl, disp, gear
```

## Advanced Bootstrap Features

### Comprehensive Bootstrap Analysis

For detailed analysis including focal variable comparisons:

``` r

# Comprehensive bootstrap with focal variable comparison
result_comprehensive <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp", "gear", "wt"),
      bootstrap = TRUE,
      comprehensive = TRUE,
      focal = "wt",  # Compare other variables to weight
      n_bootstrap = 500)  # Fewer samples for speed

# Access all bootstrap results
names(result_comprehensive$bootstrap$ci_results)
#> [1] "raw_weights"       "random_comparison" "focal_comparison"
```

### Bootstrap Parameters

Key parameters for bootstrap analysis:

- **`n_bootstrap`**: Number of bootstrap samples (default: 1000)
- **`conf_level`**: Confidence level (default: 0.95)
- **`focal`**: Focal variable for comparative analysis
- **`comprehensive`**: Enable additional bootstrap tests

``` r

# Example with different parameters
custom_bootstrap <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp"),
      bootstrap = TRUE,
      n_bootstrap = 2000,  # More samples for precision
      conf_level = 0.99)   # 99% confidence intervals

custom_bootstrap$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign Raw.RelWeight.CI.Lower
#> 1       cyl     0.3837012           50.51586    -              0.2913949
#> 2      disp     0.3758646           49.48414    -              0.2486923
#>   Raw.RelWeight.CI.Upper Random.Diff.CI.Lower Random.Diff.CI.Upper
#> 1              0.4587817            0.2486182            0.4762462
#> 2              0.4631033            0.2231991            0.4723433
#>   Raw.Significant
#> 1            TRUE
#> 2            TRUE
```

## Rescaled Weight Confidence Intervals

### Important Considerations

**Rescaled weight confidence intervals should be interpreted with
caution** due to compositional data constraints. They are not
recommended for formal statistical inference.

``` r

# Rescaled CIs (use with caution)
result_rescaled_ci <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp"),
      bootstrap = TRUE,
      include_rescaled_ci = TRUE,
      n_bootstrap = 500)

# Note the warning message about interpretation
result_rescaled_ci$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign Raw.RelWeight.CI.Lower
#> 1      disp     0.2793550           36.37966    -              0.2098045
#> 2       cyl     0.2723144           35.46279    -              0.2164417
#> 3        hp     0.2162184           28.15755    -              0.1466585
#>   Raw.RelWeight.CI.Upper Random.Diff.CI.Lower Random.Diff.CI.Upper
#> 1              0.3490397            0.1909653            0.3599660
#> 2              0.3350063            0.1923987            0.3293708
#> 3              0.2655783            0.1287796            0.2812365
#>   Raw.Significant Rescaled.RelWeight.CI.Lower Rescaled.RelWeight.CI.Upper
#> 1            TRUE                    29.92571                    43.22107
#> 2            TRUE                    30.39978                    42.52850
#> 3            TRUE                    20.51533                    36.28601
```

### Why Rescaled CIs Are Problematic

Rescaled weights are **compositional data** (they sum to 100%), which
creates dependencies between variables. This violates assumptions needed
for independent confidence intervals.

**Recommendation**: Focus on **raw weight confidence intervals** for
statistical inference.

## Real-World Applications

### Diamond Price Analysis

``` r

# Analyze diamond price drivers
diamonds_subset <- diamonds %>%
  select(price, carat, depth, table, x, y, z) %>%
  sample_n(1000)  # Sample for faster computation

diamond_rwa <- diamonds_subset %>%
  rwa(outcome = "price",
      predictors = c("carat", "depth", "table", "x", "y", "z"),
      bootstrap = TRUE,
      applysigns = TRUE,
      n_bootstrap = 500)

print(diamond_rwa$result)
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign Sign.Rescaled.RelWeight
#> 1     carat   0.257396293         29.0479318    +              29.0479318
#> 2         y   0.209603461         23.6543696    +              23.6543696
#> 3         x   0.208884201         23.5731989    +              23.5731989
#> 4         z   0.202370232         22.8380782    +              22.8380782
#> 5     table   0.005485581          0.6190640    +               0.6190640
#> 6     depth   0.002369078          0.2673575    -              -0.2673575
#>   Raw.RelWeight.CI.Lower Raw.RelWeight.CI.Upper Random.Diff.CI.Lower
#> 1           0.2473611316            0.268517346         0.2445869343
#> 2           0.2047811688            0.214125991         0.2025645259
#> 3           0.2036382695            0.213592851         0.2017898804
#> 4           0.1938388565            0.215250023         0.1921825417
#> 5           0.0001472316            0.008141882        -0.0007519212
#> 6          -0.0003476379            0.003278339        -0.0015851728
#>   Random.Diff.CI.Upper Raw.Significant
#> 1          0.265471011            TRUE
#> 2          0.212047411            TRUE
#> 3          0.211611735            TRUE
#> 4          0.212098934            TRUE
#> 5          0.007118207           FALSE
#> 6          0.002223108           FALSE
```

### Interpreting Results

``` r

# Focus on significant predictors (results are already sorted by importance)
significant_drivers <- diamond_rwa$result %>%
  filter(Raw.Significant == TRUE) %>%
  select(Variables, Rescaled.RelWeight, Sign.Rescaled.RelWeight)

cat("Significant diamond price drivers (sorted by importance):\n")
#> Significant diamond price drivers (sorted by importance):
print(significant_drivers)
#>   Variables Rescaled.RelWeight Sign.Rescaled.RelWeight
#> 1     carat           29.04793                29.04793
#> 2         y           23.65437                23.65437
#> 3         x           23.57320                23.57320
#> 4         z           22.83808                22.83808

cat("\nModel R-squared:", round(diamond_rwa$rsquare, 3))
#> 
#> Model R-squared: 0.886
```

## Best Practices

### 1. Sample Size Guidelines

``` r

# Check your sample size
n_obs <- mtcars %>% 
  select(mpg, cyl, disp, hp, gear) %>% 
  na.omit() %>% 
  nrow()

cat("Sample size:", n_obs)
#> Sample size: 32
cat("\nRecommended bootstrap samples:", min(2000, n_obs * 10))
#> 
#> Recommended bootstrap samples: 320

# Rule of thumb: At least 1000 bootstrap samples, more for smaller datasets
```

### 2. Confidence Interval Interpretation

The intervals around the raw weights describe **precision**, not
significance. Use them to judge how tightly each weight is estimated;
use `Raw.Significant` (from the random-variable comparison) to judge
importance.

``` r

# Examine CI characteristics
ci_data <- result_bootstrap$bootstrap$ci_results$raw_weights
print(head(ci_data))
#> # A tibble: 4 × 6
#>   variable weight_index ci_lower ci_upper ci_method ci_type
#>   <chr>           <int>    <dbl>    <dbl> <chr>     <chr>  
#> 1 cyl                 1   0.173     0.279 bca       raw    
#> 2 disp                2   0.158     0.280 bca       raw    
#> 3 hp                  3   0.189     0.281 bca       raw    
#> 4 gear                4   0.0416    0.184 bca       raw

# Assess precision
ci_analysis <- ci_data %>%
  mutate(
    ci_width = ci_upper - ci_lower,
    precision = case_when(
      ci_width < 0.05 ~ "High precision",
      ci_width < 0.15 ~ "Medium precision", 
      TRUE ~ "Low precision"
    )
  )

print(ci_analysis)
#> # A tibble: 4 × 8
#>   variable weight_index ci_lower ci_upper ci_method ci_type ci_width precision  
#>   <chr>           <int>    <dbl>    <dbl> <chr>     <chr>      <dbl> <chr>      
#> 1 cyl                 1   0.173     0.279 bca       raw       0.105  Medium pre…
#> 2 disp                2   0.158     0.280 bca       raw       0.123  Medium pre…
#> 3 hp                  3   0.189     0.281 bca       raw       0.0925 Medium pre…
#> 4 gear                4   0.0416    0.184 bca       raw       0.143  Medium pre…
```

### 3. Bootstrap Method Selection

The package automatically selects the best available bootstrap CI
method:

1.  **BCA (Bias-Corrected and Accelerated)** - Preferred when possible
2.  **Percentile** - Fallback if BCA fails
3.  **Basic bootstrap** - Final fallback option

``` r

# Check which methods were used
ci_methods <- result_bootstrap$bootstrap$ci_results$raw_weights %>%
  count(ci_method)

print(ci_methods)
#> # A tibble: 1 × 2
#>   ci_method     n
#>   <chr>     <int>
#> 1 bca           4
```

## Performance Considerations

### Bootstrap Speed Tips

``` r

# For large datasets or many predictors, consider:

# 1. Reduce bootstrap samples for initial exploration
quick_result <- mtcars %>%
  rwa(outcome = "mpg", 
      predictors = c("cyl", "disp"), 
      bootstrap = TRUE, 
      n_bootstrap = 500)  # Faster

# 2. Use comprehensive analysis only when needed
# comprehensive = TRUE adds computational overhead

# 3. Consider parallel processing for very large analyses
# (not currently implemented but could be future enhancement)
```

### Memory Usage

``` r

# Bootstrap objects can be large - access specific components
str(result_bootstrap$bootstrap, max.level = 1)
#> List of 7
#>  $ boot_object       :List of 11
#>   ..- attr(*, "class")= chr "boot"
#>   ..- attr(*, "boot_type")= chr "boot"
#>  $ boot_object_random:List of 11
#>   ..- attr(*, "class")= chr "boot"
#>   ..- attr(*, "boot_type")= chr "boot"
#>  $ ci_results        :List of 2
#>  $ n_bootstrap       : num 1000
#>  $ conf_level        : num 0.95
#>  $ comprehensive     : logi FALSE
#>  $ focal             : NULL

# For memory efficiency, extract only needed results
ci_summary <- result_bootstrap$bootstrap$ci_results$raw_weights %>%
  select(variable, ci_lower, ci_upper, ci_method)

print(ci_summary)
#> # A tibble: 4 × 4
#>   variable ci_lower ci_upper ci_method
#>   <chr>       <dbl>    <dbl> <chr>    
#> 1 cyl        0.173     0.279 bca      
#> 2 disp       0.158     0.280 bca      
#> 3 hp         0.189     0.281 bca      
#> 4 gear       0.0416    0.184 bca
```

## Troubleshooting

### Common Bootstrap Issues

``` r

# 1. Check for perfect multicollinearity
cor_check <- mtcars %>%
  select(cyl, disp, hp, gear) %>%
  cor()

# Look for correlations = 1.0 (excluding diagonal)
perfect_cor <- which(abs(cor_check) == 1 & cor_check != diag(diag(cor_check)), arr.ind = TRUE)

if(length(perfect_cor) > 0) {
  cat("Perfect multicollinearity detected - remove redundant variables")
} else {
  cat("No perfect multicollinearity detected")
}
#> No perfect multicollinearity detected

# 2. Ensure adequate sample size
min_sample_size <- 5 * length(c("cyl", "disp", "hp", "gear"))  # 5 obs per predictor
actual_sample_size <- nrow(na.omit(mtcars[c("mpg", "cyl", "disp", "hp", "gear")]))

cat("\nMinimum recommended sample size:", min_sample_size)
#> 
#> Minimum recommended sample size: 20
cat("\nActual sample size:", actual_sample_size)
#> 
#> Actual sample size: 32
```

## Reporting Bootstrap Results

### Standard Reporting Format

When reporting bootstrap RWA results, include:

1.  **Sample size** and missing data handling
2.  **Bootstrap parameters** (number of samples, confidence level)
3.  **CI method** used (BCA, percentile, basic)
4.  **Significant predictors** with confidence intervals
5.  **Model fit** (R-squared)

### Example Report

``` r

# Generate a summary report
report_data <- result_bootstrap$result %>%
  filter(Raw.Significant == TRUE) %>%
  arrange(desc(Rescaled.RelWeight)) %>%
  select(Variables, Rescaled.RelWeight, Raw.RelWeight.CI.Lower, Raw.RelWeight.CI.Upper)

cat("Relative Weights Analysis Results\n")
#> Relative Weights Analysis Results
cat("=================================\n")
#> =================================
cat("Sample size:", result_bootstrap$n, "\n")
#> Sample size: 32
cat("Bootstrap samples:", result_bootstrap$bootstrap$n_bootstrap, "\n")
#> Bootstrap samples: 1000
cat("Model R-squared:", round(result_bootstrap$rsquare, 3), "\n\n")
#> Model R-squared: 0.779
cat("Significant Predictors:\n")
#> Significant Predictors:
print(report_data)
#>   Variables Rescaled.RelWeight Raw.RelWeight.CI.Lower Raw.RelWeight.CI.Upper
#> 1        hp           29.79691             0.18864625              0.2811493
#> 2       cyl           29.32274             0.17336836              0.2788206
#> 3      disp           28.50999             0.15772412              0.2804741
#> 4      gear           12.37037             0.04155014              0.1843592
```

## References

**Bootstrap Methods in RWA:**

- Tonidandel, S., LeBreton, J. M., & Johnson, J. W. (2009). Determining
  the statistical significance of relative weights. *Psychological
  Methods*, 14(4), 387-399.

**General Bootstrap Theory:**

- Efron, B., & Tibshirani, R. J. (1993). *An introduction to the
  bootstrap*. Chapman & Hall/CRC.

**Compositional Data Analysis:**

- Aitchison, J. (1986). *The statistical analysis of compositional
  data*. Chapman & Hall.

## Conclusion

Bootstrap confidence intervals provide a robust solution for statistical
inference in Relative Weights Analysis. By following the guidelines in
this vignette, researchers can:

- Determine statistical significance of predictor importance
- Report confidence intervals with appropriate interpretations  
- Avoid common pitfalls in bootstrap analysis
- Apply best practices for reliable results

The bootstrap functionality in the `rwa` package represents a
significant advancement in making RWA a complete tool for both
exploratory analysis and confirmatory research.
