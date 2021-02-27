#' @title Create a Relative Weights Analysis with Multivariate Regression
#'
#' @inherit rwa description
#'
#' @inheritParams rwa
#'
#' @export
rwa_multvar <- function(df,
                      outcomes,
                      predictors,
                      applysigns = FALSE,
                      plot = TRUE){

  # Gets data frame in right order and form
  thedata <-
    df %>%
    dplyr::select(outcomes,predictors) %>%
    tidyr::drop_na(outcomes)

  numVar <- NCOL(thedata) # Output - number of variables

  # Predictors
  Variables <-
    thedata %>%
    select(predictors)

  # Correlation matrix
  mydata <-
    cor(thedata, use = "pairwise.complete.obs")

  J <- 3
  Q <- length(outcomes) # Number of outcomes
  R <- mydata
  RYY <- R[1:Q,1:Q] # Capture outcomes
  RXX <- R[(Q+1):numVar, (Q+1):numVar] # Capture predictors
  RXY <- R[1:Q,(Q+1):numVar] # Cross correlation

  RXX.eigen <- eigen(RXX)
  DX <- diag(RXX.eigen$val)
  deltax <- sqrt(DX)

  lambdax <- RXX.eigen$vec%*%deltax%*%t(RXX.eigen$vec)
  RYY.eigen <- eigen(RYY)
  DY <- diag(RYY.eigen$val)
  deltay <- sqrt(DY)
  lambday <- RYY.eigen$vec%*%deltay%*%t(RYY.eigen$vec)
  betay <- t(RXY)%*%solve(lambday)
  betax <- solve(lambdax)%*%betay
  lambdax2 <- lambdax^2
  betax2 <- betax^2
  mumatrix <- lambdax2%*%betax2
  mu1 <- rowSums(mumatrix)/Q
  P2XY <- sum(mu1)
  MU2 <- rowSums(mumatrix)
  SUMCC <- sum(MU2)
  RSMU1 <- (mu1/P2XY) * 100
  RSMU2 <- (MU2/SUMCC) * 100

  result<-
    data.frame(Variables,
               Raw.RelWeight = mu1,
               Rescaled.RelWeight = RSMU1)


  # # Get signs from coefficients
  # sign <-
  #   data.frame(V1 = beta) %>%
  #   dplyr::mutate_all(~(dplyr::case_when(.>0~"+",
  #                                        .<0~"-",
  #                                        .==0~"0",
  #                                        TRUE~NA_character_))) %>%
  #   dplyr::rename(Sign = "V1")
  #
  # ## Result
  # result <-
  #   data.frame(predictors,
  #              Raw.RelWeight = epsilon,
  #              Rescaled.RelWeight = PropWeights) %>%
  #   mutate(Sign = sign)
  #
  # complete_cases <- nrow(drop_na(thedata))
  #
  # if(applysigns == TRUE){
  #   result <-
  #     result %>%
  #     dplyr::mutate(Sign.Rescaled.RelWeight = ifelse(Sign == "-",
  #                                             Rescaled.RelWeight * -1,
  #                                             Rescaled.RelWeight))
  # }
  #
  list("predictors" = Variables,
       # "rsquare" = rsquare,
       "result" = result,
       # "n" = complete_cases,
       # "lambda" = Lambda,
       "mu1" = mu1,
       "P2XY" = P2XY,
       "MU2" = MU2,
       "SUMCC" = SUMCC,
       "RSMU1" = RSMU1,
       "RSMU2" = RSMU2)
}
