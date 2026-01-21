#' @title Create a Relative Weights Analysis (RWA)
#'
#' @description This function creates a Relative Weights Analysis (RWA) and returns a list of outputs.
#' RWA provides a heuristic method for estimating the relative weight of predictor variables in multiple regression, which involves
#' creating a multiple regression with on a set of transformed predictors which are orthogonal to each other but
#' maximally related to the original set of predictors.
#' `rwa()` is optimised for dplyr pipes and shows positive / negative signs for weights.
#'
#' @details
#' `rwa()` produces raw relative weight values (epsilons) as well as rescaled weights (scaled as a percentage of predictable variance)
#' for every predictor in the model.
#' Signs are added to the weights when the `applysigns` argument is set to `TRUE`.
#' See https://relativeimportance.davidson.edu/multipleregression.html for the original implementation that inspired this package.
#'
#' @param df Data frame or tibble to be passed through.
#' @param outcome Outcome variable, to be specified as a string or bare input. Must be a numeric variable.
#' @param predictors Predictor variable(s), to be specified as a vector of string(s) or bare input(s). All variables must be numeric.
#' @param applysigns Logical value specifying whether to show an estimate that applies the sign. Defaults to `FALSE`.
#'
#' @return `rwa_multiregress()` returns a list of outputs, as follows:
#' - `predictors`: character vector of names of the predictor variables used.
#' - `rsquare`: the rsquare value of the regression model.
#' - `result`: the final output of the importance metrics.
#'   - The `Rescaled.RelWeight` column sums up to 100.
#'   - The `Sign` column indicates whether a predictor is positively or negatively correlated with the outcome.
#' - `n`: indicates the number of observations used in the analysis.
#' - `lambda`:
#' - `RXX`: Correlation matrix of all the predictor variables against each other.
#' - `RXY`: Correlation values of the predictor variables against the outcome variable.
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr drop_na
#' @importFrom stats cor
#' @import dplyr
#' @examples
#' library(ggplot2)
#' rwa(diamonds,"price",c("depth","carat"))
#'
#' @export
rwa_multiregress <- function(df,
                             outcome,
                             predictors,
                             applysigns = FALSE){

  # Gets data frame in right order and form
  thedata <-
    df %>%
    dplyr::select(outcome,predictors) %>%
    tidyr::drop_na(outcome)

  numVar <- NCOL(thedata) # Output - number of variables

  cor_matrix <-
    cor(thedata, use = "pairwise.complete.obs") %>%
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>%
    remove_all_na_cols() %>%
    tidyr::drop_na()

  matrix_data <-
    cor_matrix %>%
    as.matrix()

  RXX <- matrix_data[2:ncol(matrix_data), 2:ncol(matrix_data)] # Only take the correlations with the predictor variables
  RXY <- matrix_data[2:ncol(matrix_data), 1] # Take the correlations of each of the predictors with the outcome variable

  # Get all the 'genuine' predictor variables
  Variables <-
    cor_matrix %>%
    names() %>%
    .[.!=outcome]

  RXX.eigen <- eigen(RXX) # Compute eigenvalues and eigenvectors of matrix
  D <- diag(RXX.eigen$val) # Run diag() on the values of eigen - construct diagonal matrix
  delta <- sqrt(D) # Take square root of the created diagonal matrix

  lambda <- RXX.eigen$vec %*% delta %*% t(RXX.eigen$vec) # Matrix multiplication
  lambdasq <- lambda ^ 2 # Square the result

  # To get partial effect of each independent variable on the dependent variable
  # We multiply the inverse matrix (RXY) on the correlation matrix between dependent and independent variables
  beta <- solve(lambda) %*% RXY # Solve numeric matrix containing coefficients of equation (Ax=B)
  rsquare <- sum(beta ^ 2) # Output - R Square, sum of squared values

  RawWgt <- lambdasq %*% beta ^ 2 # Raw Relative Weight
  import <- (RawWgt / rsquare) * 100 # Rescaled Relative Weight

  beta %>% # Get signs from coefficients
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>%
    dplyr::mutate_all(~(dplyr::case_when(.>0~"+",
                                         .<0~"-",
                                         .==0~"0",
                           TRUE~NA_character_))) %>%
    dplyr::rename(Sign="V1")-> sign

  result <- data.frame(Variables,
                       Raw.RelWeight = RawWgt,
                       Rescaled.RelWeight = import,
                       Sign = sign) # Output - results

  nrow(drop_na(thedata)) -> complete_cases

  if(applysigns == TRUE){
    result %>%
      dplyr::mutate(Sign.Rescaled.RelWeight = ifelse(Sign == "-",
                                              Rescaled.RelWeight * -1,
                                              Rescaled.RelWeight)) -> result
  }

  list("predictors" = Variables,
       "rsquare" = rsquare,
       "result" = result,
       "n" = complete_cases,
       "lambda" = lambda,
       "RXX" = RXX,
       "RXY" = RXY)
}
