#' Relative Weights Analysis
#' Optimised for dplyr pipes and shows positive / negative signs for weights.
#'
#' @param df Data frame or tibble to be passed through
#' @param outcome Outcome variable, to be specified as a string or bare input. Must be a numeric variable.
#' @param predictors Predictor variable(s), to be specified as a vector of string(s) or bare input(s). All variables must be numeric.
#' @param applysigns A logical vector specifying whether to show an estimate that applies the sign. Defaults to `FALSE`.
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr drop_na
#' @import dplyr
#' @examples
#' library(ggplot2)
#' rwa(diamonds,"price",c("depth","carat"))
#'
#' @export
rwa <- function(df, outcome, predictors, applysigns = FALSE){
  df %>%
    dplyr::select(outcome,predictors) %>%
    tidyr::drop_na(outcome)-> thedata # Gets data frame in right order and form

  numVar <- NCOL(thedata) # Output - number of variables

  na_remover <- function(df){
    Filter(function(x)!all(is.na(x)),df)
  }

  cor(thedata, use = "pairwise.complete.obs") %>%
    dplyr::as_tibble() %>%
    na_remover() %>%
    tidyr::drop_na() %>%
    as.matrix() -> matrix_data

  RXX <- matrix_data[2:ncol(matrix_data), 2:ncol(matrix_data)] # Only take the correlations with the predictor variables
  RXY <- matrix_data[2:ncol(matrix_data), 1] # Take the correlations of each of the predictors with the outcome variable

  cor(thedata, use = "pairwise.complete.obs") %>%
    dplyr::as_tibble() %>%
    na_remover() %>%
    tidyr::drop_na() %>%
    names() %>%
    .[.!=outcome] -> Variables # Get all the 'genuine' predictor variables

  RXX.eigen <- eigen(RXX) # Compute eigenvalues and eigenvectors of matrix
  D <- diag(RXX.eigen$val) # Run diag() on the values of eigen - construct diagonal matrix
  delta <- sqrt(D) # Take square root of the created diagonal matrix

  lambda <- RXX.eigen$vec %*% delta %*% t(RXX.eigen$vec) # Matrix multiplication
  lambdasq <- lambda ^ 2 # Square the result
  beta <- solve(lambda) %*% RXY # Solve numeric matrix containing coefficients of equation (Ax=B)
  rsquare <- sum(beta ^ 2) # Output - R Square

  RawWgt <- lambdasq %*% beta ^ 2 # Raw Relative Weight
  import <- (RawWgt / rsquare) * 100 # Rescaled Relative Weight

  beta %>% # Get signs from coefficients
    dplyr::as_tibble() %>%
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
      mutate(Sign.Rescaled.RelWeight = ifelse(Sign == "-",
                                              Rescaled.RelWeight * -1,
                                              Rescaled.RelWeight)) -> result
  }

  list("predictors"=Variables,
       "rsquare"=rsquare,
       "result"=result,
       "n"=complete_cases)
}
