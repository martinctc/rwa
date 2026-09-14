#' @title Create a Relative Weights Analysis (RWA)
#'
#' @description This function creates a Relative Weights Analysis (RWA) and returns a list of outputs.
#' RWA provides a heuristic method for estimating the relative weight of predictor variables in multiple regression, which involves
#' creating a multiple regression with on a set of transformed predictors which are orthogonal to each other but
#' maximally related to the original set of predictors.
#' `rwa_multiregress()` is optimised for dplyr pipes and shows positive / negative signs for weights.
#'
#' @details
#' `rwa_multiregress()` produces raw relative weight values (epsilons) as well as rescaled weights (scaled as a percentage of predictable variance)
#' for every predictor in the model.
#' Signs are added to the weights when the `applysigns` argument is set to `TRUE`.
#' See <https://www.scotttonidandel.com/rwa-web> for the original implementation that inspired this package.
#'
#' This is observation-weighted RWA when `weight` is provided, not a
#' complex-survey variance estimator. See [rwa()] for joint-matrix and predictor
#' positive-definiteness tolerances, bootstrap sampling assumptions, and
#' diagnostics. See `vignette("weighted-missing-data")` for worked examples.
#'
#' @param df Data frame or tibble to be passed through.
#' @param outcome Outcome variable, to be specified as a string or bare input. Must be a numeric variable.
#' @param predictors Predictor variable(s), to be specified as a vector of string(s) or bare input(s). All variables must be numeric.
#' @param applysigns Logical value specifying whether to show an estimate that applies the sign. Defaults to `FALSE`.
#' @inheritParams rwa
#'
#' @return `rwa_multiregress()` returns a list of outputs, as follows:
#' - `predictors`: character vector of names of the predictor variables used.
#' - `rsquare`: the rsquare value of the regression model.
#' - `result`: the final output of the importance metrics.
#'   - The `Rescaled.RelWeight` column sums up to 100.
#'   - The `Sign` column indicates whether a predictor is positively or negatively correlated with the outcome.
#' - `n`: complete-case observation count for the selected variables and weight,
#'   if supplied. Unweighted pairwise correlations may use more observations.
#' - `n_weighted`: weighted results only; sum of original weights after outcome,
#'   missing-weight, and predictor-completeness filters. Population-size meaning
#'   requires appropriately calibrated weights and retained population scope.
#' - `n_effective`: weighted results only; Kish's unequal-weighting effective
#'   sample size, `(sum(w)^2) / sum(w^2)`, evaluated using scaled weights.
#'   This is not model degrees of freedom or exact RWA precision and ignores
#'   clustering, stratification, and weight/outcome relationships.
#' - `lambda`: the transformation matrix that maps the original correlated predictors to orthogonal variables while preserving their relationship to the outcome. Used internally to compute relative weights.
#' - `RXX`: Correlation matrix of all the predictor variables against each other.
#' - `RXY`: Correlation values of the predictor variables against the outcome variable.
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr drop_na
#' @importFrom stats cor cov.wt complete.cases
#' @import dplyr
#' @examples
#' # Basic multiple regression RWA
#' result <- rwa_multiregress(
#'   df = mtcars,
#'   outcome = "mpg",
#'   predictors = c("cyl", "disp", "hp", "wt")
#' )
#'
#' # View the relative importance results
#' result$result
#'
#' # With sign information
#' result_signed <- rwa_multiregress(
#'   df = mtcars,
#'   outcome = "mpg",
#'   predictors = c("cyl", "disp", "hp", "wt"),
#'   applysigns = TRUE
#' )
#' result_signed$result
#'
#' # Using listwise deletion for missing data
#' rwa_multiregress(
#'   df = mtcars,
#'   outcome = "mpg",
#'   predictors = c("cyl", "disp"),
#'   use = "complete.obs"
#' )
#'
#' # With observation weights
#' mtcars_weighted <- mtcars
#' mtcars_weighted$w <- runif(nrow(mtcars), 0.5, 2)
#' rwa_multiregress(
#'   df = mtcars_weighted,
#'   outcome = "mpg",
#'   predictors = c("cyl", "disp"),
#'   weight = "w"
#' )
#'
#' @export
rwa_multiregress <- function(df,
                             outcome,
                             predictors,
                             applysigns = FALSE,
                             use = "pairwise.complete.obs",
                             weight = NULL){

  prepared <- prepare_rwa_data(df, outcome, predictors, use, weight)
  calculation <- calculate_rwa(prepared, outcome, predictors, use)
  Variables <- predictors
  beta <- calculation$beta
  RawWgt <- calculation$raw_weights
  import <- calculation$rescaled_weights

  sign <- beta %>% # Get signs from coefficients
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>%
    dplyr::mutate_all(~(dplyr::case_when(.>0~"+",
                                         .<0~"-",
                                         .==0~"0",
                           TRUE~NA_character_))) %>%
    dplyr::rename(Sign="V1")

  result <- data.frame(Variables,
                       Raw.RelWeight = RawWgt,
                       Rescaled.RelWeight = import,
                       Sign = sign) # Output - results

  if(applysigns == TRUE){
    result <-
      result %>%
      dplyr::mutate(Sign.Rescaled.RelWeight = ifelse(Sign == "-",
                                              Rescaled.RelWeight * -1,
                                              Rescaled.RelWeight))
  }

  output <- list("predictors" = Variables,
       "rsquare" = calculation$rsquare,
       "result" = result,
       "n" = prepared$n,
       "lambda" = calculation$lambda,
       "RXX" = calculation$RXX,
       "RXY" = calculation$RXY)
  if (!is.null(weight)) {
    normalized_weights <- prepared$weights / max(prepared$weights)
    output$n_weighted <- sum(prepared$weights)
    output$n_effective <- sum(normalized_weights)^2 / sum(normalized_weights^2)
  }
  output
}
