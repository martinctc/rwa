#' @title Create a Relative Weights Analysis (RWA)
#'
#' @description This function creates a Relative Weights Analysis (RWA) and
#'   returns a list of outputs. RWA provides a heuristic method for estimating
#'   the relative weight of predictor variables in multiple regression, which
#'   involves creating a multiple regression with on a set of transformed
#'   predictors which are orthogonal to each other but maximally related to the
#'   original set of predictors. `rwa()` is optimised for dplyr pipes and shows
#'   positive / negative signs for weights.
#'
#' @details
#' `rwa()` produces raw relative weight values (epsilons) as well as rescaled
#' weights (scaled as a percentage of predictable variance) for every predictor
#' in the model. Signs are added to the weights when the `applysigns` argument
#' is set to `TRUE`. See
#' https://relativeimportance.davidson.edu/multipleregression.html for the
#' original implementation that inspired this package.
#'
#' @param df Data frame or tibble to be passed through.
#' @param outcome Outcome variable, to be specified as a string or bare input.
#'   Must be a numeric variable.
#' @param predictors Predictor variable(s), to be specified as a vector of
#'   string(s) or bare input(s). All variables must be numeric.
#' @param applysigns Logical value specifying whether to show an estimate that
#'   applies the sign. Defaults to `FALSE`.
#' @param method String to specify the method of regression to apply. Valid
#' values include:
#'   - `"auto"`: automatically detect whether to use multiple regression or
#'   logistic regression based on the outcome variable provided.
#'   - `"multiple"`: use multiple regression.
#'   - `"logistic"`: use logistic regression.
#' @param plot Logical value specifying whether to plot the rescaled importance
#'   metrics.
#'
#' @return `rwa()` returns a list of outputs, as follows:
#' - `predictors`: character vector of names of the predictor variables used.
#' - `rsquare`: the rsquare value of the regression model.
#' - `result`: the final output of the importance metrics.
#'   - The `Rescaled.RelWeight` column sums up to 100.
#'   - The `Sign` column indicates whether a predictor is positively or
#'   negatively correlated with the outcome.
#' - `n`: indicates the number of observations used in the analysis.
#' - `lambda`:
#' - `RXX`: Correlation matrix of all the predictor variables against each
#' other. Not available for logistic regression.
#' - `RXY`: Correlation values of the predictor variables against the outcome
#' variable. Not available for logistic regression.
#'
#' @examples
#' library(ggplot2)
#'
#' # Based on multiple regression
#' rwa(diamonds,"price",c("depth","carat"))
#'
#' # Based on logistic regression
#' diamonds$IsIdeal <- as.numeric(diamonds$cut == "Ideal")
#' rwa(diamonds,"IsIdeal",c("depth","carat"))
#'
#' @export
rwa <- function(df,
                outcome,
                predictors,
                applysigns = FALSE,
                method = "auto",
                plot = TRUE){

  # Check if outcome variable is a binary variable
  outcome_var <- unique(df[[outcome]])
  outcome_var_unique <- dplyr::n_distinct(outcome_var[!is.na(outcome_var)])

  # Logistic regression
  run_logit <- function(){
    rwa_logit(
      df = df,
      outcome = outcome,
      predictors = predictors,
      applysigns = applysigns,
      plot = plot
    )
  }

  # Multiple regression
  run_mult <- function(){
    rwa_multiregress(
      df = df,
      outcome = outcome,
      predictors = predictors,
      applysigns = applysigns,
      plot = plot
    )
  }


  if(method == "auto"){

    if(outcome_var_unique == 2){

      message(
        paste0("Parsing `", outcome, "`", " as a binary variable."),
        "\nApplying logistic regression to calculate relative weights..."
      )

      run_logit()

    } else {

      message(
        paste0("Parsing `", outcome, "`", " as a non-binary variable."),
        "\nApplying multiple regression to calculate relative weights..."
      )

      run_mult()

    }

  } else if(method == "multiple"){

    run_mult()

  } else if(method == "logistic"){

    run_logit()

  } else {

    stop("invalid input for `method`.")

  }
}
