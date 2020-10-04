#' @title Create a Relative Weight Analysis (RWA)
#'
#' @description This function creates a Relative Weight Analysis by creating a regression
#' model based on a set of transformed predictors which are orthogonal to each other but
#' maximally proximate to the original set of predictors. `rwa()` is optimised for dplyr
#' pipes and shows positive / negative signs for weights.
#'
#' @param df Data frame or tibble to be passed through.
#' @param outcome Outcome variable, to be specified as a string or bare input. Must be a numeric variable.
#' @param predictors Predictor variable(s), to be specified as a vector of string(s) or bare input(s). All variables must be numeric.
#' @param applysigns A logical vector specifying whether to show an estimate that applies the sign. Defaults to `FALSE`.
#'
#' @return `rwa()` returns a list of outputs, as follows:
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
rwa <- function(df, outcome, predictors, applysigns = FALSE){
  df %>%
    dplyr::select(outcome,predictors) %>%
    tidyr::drop_na(outcome)-> thedata # Gets data frame in right order and form

  numVar <- NCOL(thedata) # Output - number of variables

  na_remover <- function(df){
    Filter(function(x)!all(is.na(x)),df)
  }

  cor(thedata, use = "pairwise.complete.obs") %>%
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>%
    # dplyr::as_tibble(.name_repair = c("check_unique", "unique", "universal", "minimal")) %>%
    na_remover() %>%
    tidyr::drop_na() %>%
    as.matrix() -> matrix_data

  RXX <- matrix_data[2:ncol(matrix_data), 2:ncol(matrix_data)] # Only take the correlations with the predictor variables
  RXY <- matrix_data[2:ncol(matrix_data), 1] # Take the correlations of each of the predictors with the outcome variable

  cor(thedata, use = "pairwise.complete.obs") %>%
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>%
    # dplyr::as_tibble(.name_repair = c("check_unique", "unique", "universal", "minimal")) %>%
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
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>%
    # dplyr::as_tibble(c("check_unique", "unique", "universal", "minimal")) %>%
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

  list("predictors"=Variables,
       "rsquare"=rsquare,
       "result"=result,
       "n"=complete_cases)
}
