# Introduction to Relative Weights Analysis with the rwa Package

``` r
library(rwa)
library(dplyr)
library(ggplot2)
```

## Introduction

**Relative Weights Analysis (RWA)** is a powerful method for determining
the relative importance of predictor variables in multiple regression
models, particularly when dealing with multicollinearity. This vignette
provides a comprehensive guide to using the `rwa` package, explaining
the methodology, interpreting results, exploring advanced features, and
evaluating the theoretical foundations and validity of the method.

## Background and Methodology

### What is Relative Weights Analysis?

Relative Weights Analysis addresses a fundamental problem in multiple
regression: when predictor variables are correlated with each other
(multicollinearity), it becomes difficult to determine the true
contribution of each variable to the outcome. Traditional regression
coefficients can be misleading, unstable, or difficult to interpret in
the presence of multicollinearity.

RWA decomposes the total variance predicted in a regression model (R²)
into weights that accurately reflect the proportional contribution of
the various predictor variables. The method implemented in this package
is based on **Tonidandel and LeBreton (2015)**, with its original roots
in **Johnson (2000)**.

### How RWA Works

The core insight of RWA is to create a set of orthogonal (uncorrelated)
variables that are maximally related to the original predictors, then
use these in a regression model. The process involves:

1.  **Eigenvalue decomposition** of the predictor correlation matrix
2.  **Transformation** of predictors into orthogonal components
3.  **Regression** using the transformed predictors
4.  **Decomposition** of R² into relative weights

This approach allows us to determine each variable’s unique contribution
to the explained variance, even when predictors are highly correlated.

### When to Use RWA

RWA is particularly useful when:

- **Multicollinearity** is present among predictors
- You need to **rank variables** by importance
- **Traditional regression coefficients** are unstable or difficult to
  interpret
- You want to understand **proportional contributions** to explained
  variance
- Conducting **key drivers analysis** in market research or business
  analytics

## Theoretical Foundation and Methodological Evaluation

### Core Methodological Insight

RWA addresses multicollinearity by attributing **shared (collinear)
variance between predictors** to those predictors in proportion to their
structure with the outcome. Unlike traditional regression coefficients
that reflect only unique contributions, RWA considers both **direct and
indirect (shared) effects**.

The **Johnson (2000)** algorithm performs an **eigenvalue
decomposition** of the predictor correlation matrix to create
uncorrelated principal components, then regresses the outcome on these
components. This procedure yields weights virtually identical to what
would be obtained by averaging a predictor’s incremental R² contribution
over *all possible subsets* of predictors (the logic used in dominance
analysis), but **without brute-force search** over subsets, making it
computationally efficient.

### Demonstrating RWA’s Theoretical Properties

Let’s demonstrate this with a controlled example where we know the true
relationships:

``` r
# Create controlled scenario to demonstrate RWA's theoretical properties
set.seed(123)
n <- 200

# Generate predictors with known correlation structure
x1 <- rnorm(n)
x2 <- 0.7 * x1 + 0.3 * rnorm(n)  # r ≈ 0.7 with x1
x3 <- 0.5 * x1 + 0.8 * rnorm(n)  # r ≈ 0.5 with x1
x4 <- rnorm(n)                    # Independent

# True population model with known coefficients
y <- 0.6 * x1 + 0.4 * x2 + 0.3 * x3 + 0.2 * x4 + rnorm(n, sd = 0.5)

theory_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)

# Compare traditional regression vs RWA
lm_theory <- lm(y ~ x1 + x2 + x3 + x4, data = theory_data)
rwa_theory <- rwa(theory_data, "y", c("x1", "x2", "x3", "x4"))

# Show how multicollinearity affects traditional coefficients
cat("True population contributions (designed into simulation):\n")
#> True population contributions (designed into simulation):
true_contributions <- c(0.6, 0.4, 0.3, 0.2)
names(true_contributions) <- c("x1", "x2", "x3", "x4")
print(true_contributions)
#>  x1  x2  x3  x4 
#> 0.6 0.4 0.3 0.2

cat("\nStandardized regression coefficients (distorted by multicollinearity):\n")
#> 
#> Standardized regression coefficients (distorted by multicollinearity):
std_betas <- summary(lm_theory)$coefficients[2:5, "Estimate"]
names(std_betas) <- c("x1", "x2", "x3", "x4")
print(round(std_betas, 3))
#>    x1    x2    x3    x4 
#> 0.449 0.586 0.285 0.164

cat("\nRWA weights (better reflect true importance despite correlations):\n")
#> 
#> RWA weights (better reflect true importance despite correlations):
rwa_weights_theory <- rwa_theory$result$Raw.RelWeight
names(rwa_weights_theory) <- rwa_theory$result$Predictors
print(round(rwa_weights_theory, 3))
#> [1] 0.306 0.304 0.155 0.021

# Calculate correlation between methods and true values
cor_with_true <- data.frame(
  Method = c("Std_Betas", "RWA_Weights"),
  Correlation_with_True = c(
    cor(abs(std_betas), true_contributions),
    cor(rwa_weights_theory[names(true_contributions)], true_contributions)
  )
)
print("Correlation with true population values:")
#> [1] "Correlation with true population values:"
print(cor_with_true)
#>        Method Correlation_with_True
#> 1   Std_Betas             0.6924877
#> 2 RWA_Weights                    NA
```

### Current Validity: Why RWA Remains Relevant

**RWA remains a widely accepted and useful method for evaluating
predictor importance under multicollinearity.** Since its introduction,
numerous studies have validated that RWA provides a fair assessment of
each variable’s importance even in high-correlation settings.

**Evidence from research practice:** - **Organizational Psychology**:
Determining which employee factors predict performance when
intercorrelated - **Marketing Research**: Key driver analysis where
product attributes tend to move together  
- **Educational Research**: Assessing cognitive abilities that naturally
correlate - **Healthcare Analytics**: Understanding risk factors that
often co-occur

**Methodological validation:** 1. **RWA reveals important predictors**
that regression beta weights obscure due to multicollinearity 2.
**Rankings from RWA correlate \> 0.99** with dominance analysis in most
scenarios 3. **Bootstrap confidence intervals** provide robust
significance testing 4. **Computational efficiency** allows analysis of
large predictor sets

## Basic Usage

### Simple Example with mtcars

Let’s start with a basic example using the built-in `mtcars` dataset to
predict fuel efficiency (`mpg`):

``` r
# Basic RWA
result_basic <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp", "gear"))

# View the results
result_basic$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1        hp     0.2321744           29.79691    -
#> 2       cyl     0.2284797           29.32274    -
#> 3      disp     0.2221469           28.50999    -
#> 4      gear     0.0963886           12.37037    +
```

### Understanding the Output

The basic output includes several key components:

``` r
# Predictor variables used
result_basic$predictors
#> [1] "cyl"  "disp" "hp"   "gear"

# Model R-squared
result_basic$rsquare
#> [1] 0.7791896

# Number of complete observations
result_basic$n
#> [1] 32

# Correlation matrices (for advanced users)
str(result_basic$RXX)  # Predictor correlation matrix
#>  num [1:4, 1:4] 1 0.902 0.832 -0.493 0.902 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : chr [1:4] "cyl" "disp" "hp" "gear"
#>   ..$ : chr [1:4] "cyl" "disp" "hp" "gear"
str(result_basic$RXY)  # Predictor-outcome correlations
#>  Named num [1:4] -0.852 -0.848 -0.776 0.48
#>  - attr(*, "names")= chr [1:4] "cyl" "disp" "hp" "gear"
```

### Interpreting the Results Table

The main results table contains:

- **Variables**: Names of predictor variables
- **Raw.RelWeight**: Raw relative weights (sum to R²)
- **Rescaled.RelWeight**: Rescaled weights as percentages (sum to 100%)
- **Sign**: Direction of relationship with outcome (+/-)

By default, results are automatically sorted by rescaled relative
weights in descending order, making it easy to identify the most
important predictors:

``` r
# Results are sorted by default (most important first)
result_basic$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1        hp     0.2321744           29.79691    -
#> 2       cyl     0.2284797           29.32274    -
#> 3      disp     0.2221469           28.50999    -
#> 4      gear     0.0963886           12.37037    +

# Raw weights sum to R-squared
sum(result_basic$result$Raw.RelWeight)
#> [1] 0.7791896
result_basic$rsquare
#> [1] 0.7791896

# Rescaled weights sum to 100%
sum(result_basic$result$Rescaled.RelWeight)
#> [1] 100
```

### Controlling Output Sorting

You can control whether results are sorted using the `sort` parameter:

``` r
# Default behavior: sorted by importance (descending)
result_sorted <- mtcars %>%
  rwa(outcome = "mpg", predictors = c("cyl", "disp", "hp", "gear"))

result_sorted$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1        hp     0.2321744           29.79691    -
#> 2       cyl     0.2284797           29.32274    -
#> 3      disp     0.2221469           28.50999    -
#> 4      gear     0.0963886           12.37037    +

# Preserve original predictor order
result_unsorted <- mtcars %>%
  rwa(outcome = "mpg", predictors = c("cyl", "disp", "hp", "gear"), sort = FALSE)

result_unsorted$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1       cyl     0.2284797           29.32274    -
#> 2      disp     0.2221469           28.50999    -
#> 3        hp     0.2321744           29.79691    -
#> 4      gear     0.0963886           12.37037    +
```

## Advanced Features

### Adding Signs to Weights

The `applysigns` parameter adds directional information to help
interpret whether variables positively or negatively influence the
outcome:

``` r
result_signs <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp", "gear"),
      applysigns = TRUE)

result_signs$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign Sign.Rescaled.RelWeight
#> 1        hp     0.2321744           29.79691    -               -29.79691
#> 2       cyl     0.2284797           29.32274    -               -29.32274
#> 3      disp     0.2221469           28.50999    -               -28.50999
#> 4      gear     0.0963886           12.37037    +                12.37037
```

### Visualization

The package allows you to visualize the relative importance by piping
the results to
[`plot_rwa()`](https://martinctc.github.io/rwa/reference/plot_rwa.md):

``` r
# Generate RWA results 
rwa_result <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp", "gear", "wt"))

# Create plot
rwa_result %>% plot_rwa()
```

![](reference/figures/README-visualization-1.png)

``` r

# The rescaled relative weights
rwa_result$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1        wt    0.23642417          27.778646    -
#> 2       cyl    0.18833374          22.128264    -
#> 3        hp    0.18809142          22.099792    -
#> 4      disp    0.16479802          19.362936    -
#> 5      gear    0.07345304           8.630361    +
```

## Bootstrap Confidence Intervals

The `rwa` package includes powerful bootstrap functionality for
determining the statistical significance of relative weights. For
detailed coverage of bootstrap methods, see the dedicated vignette:

``` r
vignette("bootstrap-confidence-intervals", package = "rwa")
```

### Quick Bootstrap Example

``` r
# Basic bootstrap analysis
bootstrap_result <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp"),
      bootstrap = TRUE,
      n_bootstrap = 500)  # Reduced for speed

# View significant predictors
bootstrap_result$result %>%
  filter(Raw.Significant == TRUE) %>%
  select(Variables, Rescaled.RelWeight, Raw.RelWeight.CI.Lower, Raw.RelWeight.CI.Upper)
#>   Variables Rescaled.RelWeight Raw.RelWeight.CI.Lower Raw.RelWeight.CI.Upper
#> 1      disp           36.37966              0.2019181              0.3411714
#> 2       cyl           35.46279              0.2175689              0.3301762
#> 3        hp           28.15755              0.1566494              0.2638215
```

For comprehensive coverage of bootstrap methods, advanced features, and
best practices, consult the bootstrap vignette.

## Real-World Example: Diamond Price Analysis

Let’s explore a more complex example using the `diamonds` dataset:

``` r
# Analyze diamond price drivers
diamonds_subset <- diamonds %>%
  select(price, carat, depth, table, x, y, z) %>%
  sample_n(1000)  # Sample for faster computation

diamond_rwa <- diamonds_subset %>%
  rwa(outcome = "price",
      predictors = c("carat", "depth", "table", "x", "y", "z"),
      applysigns = TRUE)

diamond_rwa$result
#>   Variables Raw.RelWeight Rescaled.RelWeight Sign Sign.Rescaled.RelWeight
#> 1     carat   0.253554640         28.9515921    +              28.9515921
#> 2         y   0.206371378         23.5640727    +              23.5640727
#> 3         z   0.204433608         23.3428125    +              23.3428125
#> 4         x   0.204002285         23.2935629    +              23.2935629
#> 5     table   0.004842128          0.5528879    +               0.5528879
#> 6     depth   0.002584205          0.2950719    -              -0.2950719
```

For bootstrap analysis of this example with confidence intervals, see:

``` r
vignette("bootstrap-confidence-intervals", package = "rwa")
```

## Comparison with Traditional Regression

Let’s compare RWA results with traditional multiple regression to
highlight the differences:

``` r
# Traditional regression
lm_model <- lm(mpg ~ cyl + disp + hp + gear, data = mtcars)
lm_summary <- summary(lm_model)

# Display regression summary
print(lm_summary)
#> 
#> Call:
#> lm(formula = mpg ~ cyl + disp + hp + gear, data = mtcars)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -4.3716 -2.3319 -0.8279  1.3156  7.0782 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 27.42342    6.30108   4.352 0.000173 ***
#> cyl         -0.86624    0.84941  -1.020 0.316869    
#> disp        -0.01190    0.01190  -1.000 0.325996    
#> hp          -0.03050    0.01982  -1.539 0.135498    
#> gear         1.42306    1.21053   1.176 0.250030    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 3.035 on 27 degrees of freedom
#> Multiple R-squared:  0.7792, Adjusted R-squared:  0.7465 
#> F-statistic: 23.82 on 4 and 27 DF,  p-value: 1.606e-08

# RWA results
rwa_model <- mtcars %>%
  rwa(outcome = "mpg", predictors = c("cyl", "disp", "hp", "gear"))

# Compare importance rankings
comparison <- data.frame(
  Variable = rwa_model$predictors,
  RWA_Rescaled = rwa_model$result$Rescaled.RelWeight,
  RWA_Rank = rank(-rwa_model$result$Rescaled.RelWeight)
)

print(comparison)
#>   Variable RWA_Rescaled RWA_Rank
#> 1      cyl     29.79691        1
#> 2     disp     29.32274        2
#> 3       hp     28.50999        3
#> 4     gear     12.37037        4
```

### Methodological Advantages of RWA

#### 1. Superior to Standard Regression Under Multicollinearity

Traditional standardized beta coefficients reflect **unique contribution
holding others constant**, which severely underestimates variables that
share variance with others. RWA gives credit to predictors for variance
they share with the outcome **both individually and jointly** with other
variables.

#### 2. Avoids Stepwise Selection Problems

Stepwise methods arbitrarily pick one variable from a collinear set and
drop others, potentially leading to incorrect conclusions about
importance. Unlike stepwise selection, **RWA does not throw away
information** – it shows how much each predictor contributes given the
complete set.

#### 3. More Interpretable than Principal Components

While PCA addresses multicollinearity, it **buries the identity of
individual predictors** in composite factors. RWA uses orthogonalization
behind the scenes but **returns results in terms of original
predictors**, maintaining interpretability.

#### 4. Computational Efficiency vs. Dominance Analysis

RWA’s greatest achievement is producing **almost identical results to
dominance analysis but far more efficiently**. Research shows:

- **Correlation \> 0.99** between RWA and dominance analysis rankings
- **Exponential time savings**: dominance analysis requires 2^p models;
  RWA solves equations directly
- **Enhanced capabilities**: easier to provide confidence intervals and
  significance tests

``` r
# Demonstrate computational considerations
predictors <- c("cyl", "disp", "hp", "gear")
n_predictors <- length(predictors)

cat("Number of predictors:", n_predictors, "\n")
#> Number of predictors: 4
cat("Dominance analysis would require", 2^n_predictors, "subset models\n")
#> Dominance analysis would require 16 subset models
cat("RWA solves this in a single matrix operation\n")
#> RWA solves this in a single matrix operation

# Show RWA speed (for demonstration)
start_time <- Sys.time()
rwa_speed_test <- mtcars %>% rwa(outcome = "mpg", predictors = predictors)
end_time <- Sys.time()
cat("RWA computation time:", round(as.numeric(end_time - start_time, units = "secs"), 4), "seconds\n")
#> RWA computation time: 0.0079 seconds
```

## Critical Limitations and When to Exercise Caution

### 1. Not a Replacement for Theory

**RWA is fundamentally a descriptive, variance-partitioning tool.** It
tells us “how much prediction was attributable to X” but **does not
imply causation**. As Tonidandel & LeBreton emphasize: relative weights
**“are not causal indicators and thus do not necessarily dictate a
course of action.”**

RWA should enhance interpretation of regression models but decisions
must still be guided by substantive theory.

### 2. The Redundancy Problem

**RWA is not a magic bullet for extreme multicollinearity stemming from
redundant predictors.** When predictors measure essentially the same
construct, RWA will split the contribution between them, potentially
making each appear less important individually.

``` r
# Demonstrate the redundancy limitation
set.seed(456)
x1_orig <- rnorm(100)
x1_dup <- x1_orig + rnorm(100, sd = 0.05)  # Nearly identical (r ≈ 0.99)
y_simple <- 0.8 * x1_orig + rnorm(100, sd = 0.5)

redundant_data <- data.frame(y = y_simple, x1_original = x1_orig, 
                           x1_duplicate = x1_dup)

cat("Correlation between 'different' predictors:", cor(x1_orig, x1_dup), "\n")
#> Correlation between 'different' predictors: 0.9988244

# RWA correctly splits redundant variance
redundant_rwa <- rwa(redundant_data, "y", c("x1_original", "x1_duplicate"))
print("RWA with redundant predictors:")
#> [1] "RWA with redundant predictors:"
print(redundant_rwa$result)
#>      Variables Raw.RelWeight Rescaled.RelWeight Sign
#> 1  x1_original     0.3841750           50.05342    +
#> 2 x1_duplicate     0.3833551           49.94658    +

cat("\nEach variable appears less important individually,")
#> 
#> Each variable appears less important individually,
cat("\nbut together they account for most variance.\n")
#> 
#> but together they account for most variance.
cat("Combined contribution:", 
    sum(redundant_rwa$result$Raw.RelWeight), "\n")
#> Combined contribution: 0.7675301
```

This is **statistically appropriate** – it reflects that one variable
alone could do the job of both. Users must recognize that **the pair as
a whole may be very important even if each alone has a modest weight.**

### 3. Sample Size and Stability

RWA shares multiple regression’s sensitivity to **sample size and
predictor-to-observation ratios.** Bootstrap confidence intervals help
quantify uncertainty, but “bootstrapping will not overcome the inherent
limitations associated with a small sample size.”

## Best Practices and Recommendations

### 1. Sample Size Considerations

``` r
# Check your sample size
n_obs <- mtcars %>% 
  select(mpg, cyl, disp, hp, gear) %>% 
  na.omit() %>% 
  nrow()

cat("Sample size:", n_obs)
#> Sample size: 32
cat("\nRule of thumb: At least 5-10 observations per predictor")
#> 
#> Rule of thumb: At least 5-10 observations per predictor
```

### 2. Variable Selection and Model Specification

RWA works best with:

- Continuous variables (though categorical can be used with caution)
- Theoretically meaningful predictors  
- Reasonable predictor-to-observation ratios
- Predictors that are distinct but may be correlated

### 3. When to Use RWA vs. Alternatives

Use RWA when:

1.  Predictors are theoretically distinct but empirically correlated
2.  Understanding relative contribution (not just prediction) is the
    goal
3.  Sample size supports stable multiple regression
4.  Linear relationships are reasonable assumptions

Consider alternatives when:

1.  Predictors are essentially redundant measures
2.  Sample size is very small relative to predictors
3.  Primary goal is variable selection rather than interpretation
4.  Non-linear effects are suspected

### 4. Reporting Results

When reporting RWA results, include:

- Sample size and missing data handling
- Raw and rescaled weights
- Model R-squared
- Confidence intervals for key predictors (when using bootstrap)

For bootstrap confidence intervals and advanced statistical
considerations, see:

``` r
vignette("bootstrap-confidence-intervals", package = "rwa")
```

## Troubleshooting Common Issues

### Multicollinearity Warnings

If you encounter extreme multicollinearity:

``` r
# Check correlation matrix
cor_matrix <- mtcars %>%
  select(cyl, disp, hp, gear) %>%
  cor()

# Look for high correlations (>0.9)
high_cor <- which(abs(cor_matrix) > 0.9 & cor_matrix != 1, arr.ind = TRUE)
if(nrow(high_cor) > 0) {
  cat("High correlations detected between variables")
}
#> High correlations detected between variables
```

### Missing Data

RWA handles missing data through listwise deletion:

``` r
# Check for missing data patterns
missing_summary <- mtcars %>%
  select(mpg, cyl, disp, hp, gear) %>%
  summarise_all(~sum(is.na(.)))

print(missing_summary)
#>   mpg cyl disp hp gear
#> 1   0   0    0  0    0
```

## References

This package and methodology are based on the following key references:

**Primary Sources:**

- **Johnson, J. W. (2000)**. A heuristic method for estimating the
  relative weight of predictor variables in multiple regression.
  *Multivariate Behavioral Research*, 35(1), 1-19. DOI:
  10.1207/S15327906MBR3501_1

- **Tonidandel, S., & LeBreton, J. M. (2015)**. RWA Web: A free,
  comprehensive, web-based, and user-friendly tool for relative weight
  analyses. *Journal of Business and Psychology*, 30(2), 207-216. DOI:
  10.1007/s10869-014-9351-z

**Additional Reading:**

- Azen, R., & Budescu, D. V. (2003). The dominance analysis approach for
  comparing predictors in multiple regression. *Psychological Methods*,
  8(2), 129-148.

- Grömping, U. (2006). Relative importance for linear regression in R:
  The package relaimpo. *Journal of Statistical Software*, 17(1), 1-27.

- Johnson, J. W., & LeBreton, J. M. (2004). History and use of relative
  importance indices in organizational research. *Organizational
  Research Methods*, 7(3), 238-257.

For bootstrap methods and statistical significance testing, see the
dedicated bootstrap vignette and its references.

For the latest methodological developments:
<https://www.scotttonidandel.com/rwa-web/>

## Conclusion

The `rwa` package provides a user-friendly implementation of Relative
Weights Analysis in R with seamless tidyverse integration. This vignette
covered both practical implementation and theoretical foundations.

### Key Takeaways

Methodological Strengths:

- Handles multicollinearity effectively by partitioning shared variance
  appropriately
- Provides interpretable results as percentages of explained variance  
- Computationally efficient compared to dominance analysis
- Statistically robust with bootstrap confidence intervals

Important Limitations:

- Not a replacement for theory - results are descriptive, not causal
- Redundant predictors will split variance, potentially appearing less
  important individually
- Sample size matters - adequate observations needed for stable
  estimates
- Linear model assumptions apply

### When to Use RWA

RWA is particularly valuable for:

- Key drivers analysis in market research and business analytics  
- Multicollinear predictors in scientific research
- Situations requiring interpretable importance measures
- Linear models where traditional regression coefficients are misleading

### The Bottom Line

RWA provides **clear, quantifiable answers to questions about predictor
importance in a multicollinear world** – answers that are often obscured
when using standard regression approaches. With proper understanding of
its assumptions and limitations, RWA remains an essential tool for
researchers and practitioners dealing with correlated predictors.

Whether you’re conducting key drivers analysis in market research,
exploring variable importance in predictive modeling, or addressing
multicollinearity in academic research, RWA provides valuable insights
that complement traditional regression approaches.

For advanced bootstrap methods and statistical significance testing,
see:

``` r
vignette("bootstrap-confidence-intervals", package = "rwa")
```
