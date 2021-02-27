#' @title Create a Relative Weights Analysis with logistic regression
#'
#' @inherit rwa description
#'
#' @inheritParams rwa
#'
#' @export
rwa_logit <- function(df,
                      outcome,
                      predictors,
                      applysigns = FALSE,
                      plot = TRUE){

  # Gets data frame in right order and form
  thedata <-
    df %>%
    dplyr::select(outcome,predictors) %>%
    tidyr::drop_na(outcome)

  numVar <- NCOL(thedata) # Output - number of variables

  # Predictors
  Variables <-
    thedata %>%
    select(predictors)

  # Select outcome variable
  Y <-
    thedata %>%
    pull(outcome)

  # Scaled predictors
  X <-
    thedata %>%
    select(predictors) %>%
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

  # Creating logit-Y-hat
  lYhat <- log(LpredY/(1-LpredY))

  # Getting st dev of logit-Y-hat
  stdlYhat <- sd(lYhat)

  # Getting R-sq
  getting.Rsq <- lm(LpredY ~ Y)

  # Computing standardized logistic regression coefficients
  Rsq <- summary(getting.Rsq)$r.squared
  beta <- b*((sqrt(Rsq))/stdlYhat)

  epsilon <- Lambda^2 %*% beta^2

  R.sq <- sum(epsilon)
  PropWeights <- (epsilon/R.sq)

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
    data.frame(predictors,
               Raw.RelWeight = epsilon,
               Rescaled.RelWeight = PropWeights) %>%
    mutate(Sign = sign)

  complete_cases <- nrow(drop_na(thedata))

  if(applysigns == TRUE){
    result <-
      result %>%
      dplyr::mutate(Sign.Rescaled.RelWeight = ifelse(Sign == "-",
                                              Rescaled.RelWeight * -1,
                                              Rescaled.RelWeight))
  }

  list("predictors" = predictors,
       # "rsquare" = rsquare,
       "result" = result,
       "n" = complete_cases,
       "lambda" = Lambda)
}
