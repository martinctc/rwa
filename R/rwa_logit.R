#' @title Create a Relative Weights Analysis with logistic regression
#'
#' @description This function performs Relative Weights Analysis (RWA) for binary
#'   outcome variables using logistic regression. RWA provides a method for
#'   estimating the relative importance of predictor variables by transforming
#'   them into orthogonal variables while preserving their relationship to the
#'   outcome. This implementation follows Johnson (2000) for logistic regression.
#'
#' @inheritParams rwa
#'
#' @return `rwa_logit()` returns a list of outputs, as follows:
#' - `predictors`: character vector of names of the predictor variables used.
#' - `rsquare`: the pseudo R-squared value (sum of epsilon weights) for the logistic regression model.
#' - `result`: the final output of the importance metrics.
#'   - The `Rescaled.RelWeight` column sums up to 100.
#'   - The `Sign` column indicates whether a predictor is positively or negatively associated with the outcome.
#' - `n`: indicates the number of observations used in the analysis.
#' - `lambda`: the Lambda transformation matrix from the analysis.
#'
#' @examples
#' # Create a binary outcome variable
#' mtcars_binary <- mtcars
#' mtcars_binary$high_mpg <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 0)
#'
#' # Basic logistic RWA
#' result <- rwa_logit(
#'   df = mtcars_binary,
#'   outcome = "high_mpg",
#'   predictors = c("cyl", "disp", "hp", "wt")
#' )
#'
#' # View the relative importance results
#' result$result
#'
#' # With sign information
#' result_signed <- rwa_logit(
#'   df = mtcars_binary,
#'   outcome = "high_mpg",
#'   predictors = c("cyl", "disp", "hp", "wt"),
#'   applysigns = TRUE
#' )
#' result_signed$result
#'
#' @importFrom magrittr %>%
#' @importFrom stats glm binomial coef predict sd lm
#' @export
rwa_logit <- function(df,
                      outcome,
                      predictors,
                      applysigns = FALSE){

  # Gets data frame in right order and form
  thedata <-
    df %>%
    dplyr::select(all_of(c(outcome, predictors))) %>%
    tidyr::drop_na(all_of(outcome))

  # Get variable names for output
  Variables <-
    thedata %>%
    dplyr::select(all_of(predictors)) %>%
    names()

  # Select outcome variable
  Y <-
    thedata %>%
    pull(all_of(outcome))

  # Scaled predictors
  X <-
    thedata %>%
    dplyr::select(all_of(predictors)) %>%
    scale()

  X.svd <- svd(X) # Single-value decomposition
  Q <- X.svd$v
  P <- X.svd$u
  Z <- P %*% t(Q)

  Z.stand <- scale(Z)

  # Obtaining Lambda from equation 7 from Johnson (2000) pg 8
  Lambda <-
    solve(
      t(Z.stand) %*% Z.stand
      ) %*%
    t(Z.stand) %*%
    X

  logrfit <-
    glm(Y ~ Z.stand,
        family = binomial)

  unstCoefs <- coef(logrfit)

  b <- unstCoefs[2:length(unstCoefs)]

  LpredY <-
    predict(
      logrfit,
      newdata = thedata,
      type="response")

  # Clamp predictions to avoid Inf/-Inf in logit transformation
  # This can occur with perfect separation in logistic regression
  LpredY <- pmax(pmin(LpredY, 1 - 1e-10), 1e-10)

  # Creating logit-Y-hat
  lYhat <- log(LpredY/(1-LpredY))

  # Getting st dev of logit-Y-hat
  stdlYhat <- stats::sd(lYhat)

  # Check for zero standard deviation (can occur with perfect separation)
  if (stdlYhat == 0 || is.na(stdlYhat)) {
    warning("Perfect or near-perfect separation detected. Results may be unreliable.")
    stdlYhat <- 1e-10  # Avoid division by zero
  }

  # Getting R-sq
  getting.Rsq <- lm(LpredY ~ Y)

  # Computing standardized logistic regression coefficients
  Rsq <- summary(getting.Rsq)$r.squared
  beta <- b*((sqrt(Rsq))/stdlYhat)

  epsilon <- Lambda^2 %*% beta^2

  R.sq <- sum(epsilon)
  PropWeights <- (epsilon/R.sq) * 100  # Convert to percentage (0-100) for consistency with rwa_multiregress

  # Get signs from coefficients
  sign <-
    data.frame(V1 = beta) %>%
    dplyr::mutate_all(~(dplyr::case_when(.>0~"+",
                                         .<0~"-",
                                         .==0~"0",
                                         TRUE~NA_character_))) %>%
    dplyr::rename(Sign = "V1")

  ## Result
  result <-
    data.frame(Variables,
               Raw.RelWeight = epsilon,
               Rescaled.RelWeight = PropWeights) %>%
    dplyr::mutate(Sign = sign)

  complete_cases <- nrow(tidyr::drop_na(thedata))

  if(applysigns == TRUE){
    result <-
      result %>%
      dplyr::mutate(Sign.Rescaled.RelWeight = ifelse(Sign == "-",
                                              Rescaled.RelWeight * -1,
                                              Rescaled.RelWeight))
  }

  list("predictors" = predictors,
       "rsquare" = R.sq,
       "result" = result,
       "n" = complete_cases,
       "lambda" = Lambda)
}
