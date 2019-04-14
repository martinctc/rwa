# rwa
Package for running Relative Weights Analysis in R


### Background
Relative Weights Analysis (RWA) method of calculating relative importance of predictor variables in contributing to an outcome variable. The method implemented by this function is based on Toniandel and LeBreton (2015). Broadly speaking, RWA belongs to a family of techiques under the broad umbrella 'Relative Importance Analysis', where other members include the 'Shapley method' and 'dominance analysis'.

The `rwa()` function in this package is compatible with dplyr / tidyverse style of piping operations to enable cleaner and more readable code.

---

### Installation

surveytoolbox is not release on CRAN (yet). 
You can install the latest development version from GitHub with:

```
install.packages("devtools")
devtools::install_github("martinctc/rwa")
```

---

### Method / Technical details
RWA decomposes the total variance predicted in a regression model (R2) into weights that accurately reﬂect the proportional contribution of the various predictor variables. 

RWA is a useful technique to calculate the relative importance of predictors (independent variables) when independent variables are correlated to each other. It is an alternative to multiple regression technique and it addresses multicollinearity proble,m and also helps to calculate the importance rank of variables. It helps to answer "which variable is the most important and rank variables based on their contribution to R-Square".

See https://link.springer.com/content/pdf/10.1007%2Fs10869-014-9351-z.pdf. 

### Multicollinearity
When independent variables are correlated, it is difficult to determine the correct prediction power of each variable. Hence, it is difficult to rank them as we are unable to estimate coefficients correctly. Statistically, multicollinearity can increase the standard error of the coefficient estimates and make the estimates very sensitive to minor changes in the model. It means the coefficients are biased and difficult to interpret.

---

### Latest Status

The main `rwa()` function is ready-to-use, but the intent is to develop additional functions for this package which supplement the use of this function, such as tidying outputs and visualisations.


### Contact me
---
Please feel free to submit suggestions and report bugs: <https://github.com/martinctc/rwa/issues>

Also check out my [website](https://martinctc.github.io) for my other work and packages.
