# rwa

[![R build status](https://github.com/martinctc/rwa/workflows/R-CMD-check/badge.svg)](https://github.com/martinctc/rwa/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/martinctc/rwa/badge)](https://www.codefactor.io/repository/github/martinctc/rwa)
[![CRAN status](https://www.r-pkg.org/badges/version/rwa)](https://CRAN.R-project.org/package=rwa/)
[![CRAN last month downloads](https://cranlogs.r-pkg.org/badges/last-month/rwa?color=green/)](https://cran.r-project.org/package=rwa/)

Perform a Relative Weights Analysis in R


### Background
**Relative Weights Analysis (RWA)** is a method of calculating relative importance of predictor variables in contributing to an outcome variable. The method implemented by this function is based on Tonidandel and LeBreton (2015), but the origin of this specific approach can be traced back to Johnson (2000), *A Heuristic Method for Estimating the Relative Weight of Predictor Variables in Multiple Regression*. Broadly speaking, RWA belongs to a family of techiques under the broad umbrella 'Relative Importance Analysis', where other members include the 'Shapley method' and 'dominance analysis'. This is often referred to as 'Key Drivers Analysis' within market research.

This package is built around the main function `rwa()`, which takes in a data frame as an argument and allows you to specify the names of the input variables and the outcome variable as arguments.

The `rwa()` function in this package is compatible with dplyr / tidyverse style of piping operations to enable cleaner and more readable code.

---

### Installation

You can install the stable CRAN version of **rwa** with:

```R
install.packages("rwa")
```

Alternatively, you can install the latest development version from GitHub with:

```R
install.packages("devtools")
devtools::install_github("martinctc/rwa")
```

---

### Method / Technical details
RWA decomposes the total variance predicted in a regression model (R2) into weights that accurately reﬂect the proportional contribution of the various predictor variables. 

RWA is a useful technique to calculate the relative importance of predictors (independent variables) when independent variables are correlated to each other. It is an alternative to multiple regression technique and it addresses the _multicollinearity_ problem, and also helps to calculate the importance rank of variables. It helps to answer "which variable is the most important and rank variables based on their contribution to R-Square".

See https://link.springer.com/content/pdf/10.1007%2Fs10869-014-9351-z.pdf. 

### Multicollinearity

When independent variables are correlated, it is difficult to determine the correct prediction power of each variable. Hence, it is difficult to rank them as we are unable to estimate coefficients correctly. Statistically, multicollinearity can increase the standard error of the coefficient estimates and make the estimates very sensitive to minor changes in the model. It means the coefficients are biased and difficult to interpret.

### Signs

Key Drivers Analysis methods do not conventionally include a score sign, which can make it difficult to interpret whether a variable is _positively_ or _negatively_ driving the outcome. The `applysigns` argument in `rwa::rwa()`, when set to `TRUE`, allows the application of positive or negative signs to the driver scores to match the signs of the corresponding linear regression coefficients from the model. This feature mimics the solution used in the [Q research software](https://wiki.q-researchsoftware.com/wiki/Driver_(Importance)_Analysis). The resulting column is labelled `Sign.Rescaled.RelWeight` to distinguish from the unsigned column.

### Estimating the statistical significance of relative weights

As Tonidandel et al. (2009) noted, there is no default procedure for determining the statistical significance of individual relative weights: 

> The difficulty in determining the statistical significance of relative weights stems from the fact that the exact (or small sample) sampling distribution of relative weights is unknown.

The paper itself suggests a Monte Carlo method for estimating the statistical significance, but this is currently not available or provided in the package, but the plan is to implement this in the near future.

### Basic example

You can pass the raw data directly into `rwa()`, without having to first compute a correlation matrix. The below example is with `mtcars`.

Code:
```R
library(rwa)
library(tidyverse)

mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp", "gear"),
      applysigns = TRUE)
```
Results:
```R
$predictors
[1] "cyl"  "disp" "hp"   "gear"

$rsquare
[1] 0.7791896

$result
  Variables Raw.RelWeight Rescaled.RelWeight Sign Sign.Rescaled.RelWeight
1       cyl     0.2284797           29.32274    -               -29.32274
2      disp     0.2221469           28.50999    -               -28.50999
3        hp     0.2321744           29.79691    -               -29.79691
4      gear     0.0963886           12.37037    +                12.37037

$n
[1] 32      
```

---

### Documentation

For comprehensive examples and methodology explanations, see the package vignettes:

```R
# Main introduction to RWA
vignette("introduction-to-rwa", package = "rwa")

# Bootstrap confidence intervals for statistical significance
vignette("bootstrap-confidence-intervals", package = "rwa")
```

**Online documentation**: Visit [https://martinctc.github.io/rwa/](https://martinctc.github.io/rwa/) for the full pkgdown site with:

- [Getting Started Guide](https://martinctc.github.io/rwa/articles/introduction-to-rwa.html)
- [Bootstrap Confidence Intervals](https://martinctc.github.io/rwa/articles/bootstrap-confidence-intervals.html)
- [Function Reference](https://martinctc.github.io/rwa/reference/index.html)

The vignettes cover:

- **Introduction vignette**: Detailed methodology, basic examples, and when to use RWA
- **Bootstrap vignette**: Statistical significance testing, confidence intervals, and advanced features
- Real-world applications and best practices
- Troubleshooting and interpretation guidance

---

### Latest Status

The main `rwa()` function is ready-to-use and now includes bootstrap confidence intervals for determining the statistical significance of relative weights. The package includes comprehensive documentation through vignettes and is designed to integrate seamlessly with tidyverse workflows.

---

### Contact me

Please feel free to submit suggestions and report bugs: <https://github.com/martinctc/rwa/issues>

Also check out my [website](https://martinctc.github.io) for my other work and packages.

### References / Bibliography

Azen, R., & Budescu, D. V. (2003). The dominance analysis approach for comparing predictors in multiple regression. *Psychological methods*, 8(2), 129.

Budescu, D. V. (1993). Dominance analysis: a new approach to the problem of relative importance of predictors in multiple regression. *Psychological bulletin*, 114(3), 542.

Grömping, U. (2006). Relative importance for linear regression in R: the package relaimpo. *Journal of statistical software*, 17(1), 1-27.

Grömping, U. (2009). Variable importance assessment in regression: linear regression versus random forest. *The American Statistician*, 63(4), 308-319.

Johnson, J. W., & LeBreton, J. M. (2004). History and use of relative importance indices in organizational research. *Organizational research methods*, 7(3), 238-257.

Lindeman RH, Merenda PF, Gold RZ (1980). *Introduction to Bivariate and Multivariate
Analysis*. Scott, Foresman, Glenview, IL.

Tonidandel, S., & LeBreton, J. M. (2011). Relative importance analysis: A useful supplement to regression analysis. *Journal of Business and Psychology*, 26(1), 1-9.

Tonidandel, S., LeBreton, J. M., & Johnson, J. W. (2009). Determining the statistical significance of relative weights. *Psychological methods*, *14*(4), 387.

Wang, X., Duverger, P., Bansal, H. S. (2013). Bayesian Inference of Predictors Relative Importance in Linear Regression Model using Dominance Hierarchies. *International Journal of Pure and Applied Mathematics*, Vol. 88, No. 3, 321-339.

Also see [Kovalyshyn](https://medium.com/analytics-vidhya/johnsons-relative-weights-analysis-implementation-with-javascript-d85393c0bbb4) for a similar implementation but in Javascript.

