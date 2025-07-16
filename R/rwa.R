#' @title Create a Relative Weights Analysis (RWA)
#'
#' @description This function creates a Relative Weights Analysis (RWA) and 
#'   returns a list of outputs. RWA provides a heuristic method for estimating 
#'   the relative weight of predictor variables in multiple regression, which 
#'   involves creating a multiple regression with on a set of transformed 
#'   predictors which are orthogonal to each other but maximally related to the 
#'   original set of predictors.
#' `rwa()` is optimised for dplyr pipes and shows positive / negative signs for weights.
#'
#' @details
#' `rwa()` produces raw relative weight values (epsilons) as well as rescaled 
#' weights (scaled as a percentage of predictable variance) for every predictor 
#' in the model. Signs are added to the weights when the `applysigns` argument 
#' is set to `TRUE`.
#' See https://www.scotttonidandel.com/rwa-web for the 
#' original implementation that inspired this package.
#'
#' @param df Data frame or tibble to be passed through.
#' @param outcome Outcome variable, to be specified as a string or bare input. 
#'   Must be a numeric variable.
#' @param predictors Predictor variable(s), to be specified as a vector of 
#'   string(s) or bare input(s). All variables must be numeric.
#' @param applysigns Logical value specifying whether to show an estimate that applies the sign. Defaults to `FALSE`.
#' @param sort Logical value specifying whether to sort results by rescaled relative weights in descending order. Defaults to `TRUE`.
#' @param bootstrap Logical value specifying whether to calculate bootstrap confidence intervals. Defaults to `FALSE`.
#' @param n_bootstrap Number of bootstrap samples to use when bootstrap = TRUE. Defaults to 1000.
#' @param conf_level Confidence level for bootstrap intervals. Defaults to 0.95.
#' @param focal Focal variable for bootstrap comparisons (optional).
#' @param comprehensive Whether to run comprehensive bootstrap analysis including random variable and focal comparisons.
#' @param include_rescaled_ci Logical value specifying whether to include confidence intervals for rescaled weights. Defaults to `FALSE` due to compositional data constraints. Use with caution.
#'
#' @return `rwa()` returns a list of outputs, as follows:
#' - `predictors`: character vector of names of the predictor variables used.
#' - `rsquare`: the rsquare value of the regression model.
#' - `result`: the final output of the importance metrics (sorted by Rescaled.RelWeight in descending order by default).
#'   - The `Rescaled.RelWeight` column sums up to 100.
#'   - The `Sign` column indicates whether a predictor is positively or negatively correlated with the outcome.
#'   - When bootstrap = TRUE, includes confidence interval columns for raw weights.
#'   - Rescaled weight CIs are available via include_rescaled_ci = TRUE but not recommended for inference.
#' - `n`: indicates the number of observations used in the analysis.
#' - `bootstrap`: bootstrap results (only present when bootstrap = TRUE), containing:
#'   - `ci_results`: confidence intervals for weights
#'   - `boot_object`: raw bootstrap object for advanced analysis
#'   - `n_bootstrap`: number of bootstrap samples used
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
#' # Basic RWA (results sorted by default)
#' rwa(diamonds,"price",c("depth","carat"))
#' 
#' # RWA without sorting (preserves original predictor order)
#' rwa(diamonds,"price",c("depth","carat"), sort = FALSE)
#' 
#' # RWA with bootstrap confidence intervals (raw weights only)
#' rwa(diamonds,"price",c("depth","carat"), bootstrap = TRUE, n_bootstrap = 500)
#' 
#' # Include rescaled weight CIs (use with caution for inference)
#' rwa(diamonds,"price",c("depth","carat"), bootstrap = TRUE, include_rescaled_ci = TRUE)
#' 
#' # Comprehensive bootstrap analysis with focal variable
#' result <- rwa(diamonds,"price",c("depth","carat","table"), 
#'               bootstrap = TRUE, comprehensive = TRUE, focal = "carat")
#' # View confidence intervals
#' result$bootstrap$ci_results
#'
#' @export
rwa <- function(df,
                outcome,
                predictors,
                applysigns = FALSE,
                sort = TRUE,
                bootstrap = FALSE,
                n_bootstrap = 1000,
                conf_level = 0.95,
                focal = NULL,
                comprehensive = FALSE,
                include_rescaled_ci = FALSE){

  # Gets data frame in right order and form
  thedata <-
    df %>%
    dplyr::select(dplyr::all_of(c(outcome, predictors))) %>%
    tidyr::drop_na(dplyr::all_of(outcome))

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

  # Sort results by rescaled relative weights if requested
  if(sort == TRUE){
    result <- result %>%
      dplyr::arrange(desc(Rescaled.RelWeight))
  }

  # Run bootstrap analysis if requested
  if(bootstrap == TRUE) {
    message("Running bootstrap analysis with ", n_bootstrap, " samples...")
    
    bootstrap_results <- run_rwa_bootstrap(
      data = df,
      outcome = outcome,
      predictors = predictors,
      n_bootstrap = n_bootstrap,
      conf_level = conf_level,
      focal = focal,
      comprehensive = comprehensive,
      include_rescaled = include_rescaled_ci  # Only include if explicitly requested
    )
    
    # Add confidence intervals to result dataframe
    if(!is.null(bootstrap_results$ci_results$raw_weights)) {
      ci_data <- bootstrap_results$ci_results$raw_weights
      
      # Add CI columns for raw weights
      result$Raw.RelWeight.CI.Lower <- ci_data$ci_lower[match(result$Variables, ci_data$variable)]
      result$Raw.RelWeight.CI.Upper <- ci_data$ci_upper[match(result$Variables, ci_data$variable)]
      
      # Add significance indicator for raw weights (if CI doesn't include 0)
      result$Raw.Significant <- !(result$Raw.RelWeight.CI.Lower <= 0 & result$Raw.RelWeight.CI.Upper >= 0)
    }
    
    # Add rescaled weight CIs only if explicitly requested and warn user
    if(include_rescaled_ci && !is.null(bootstrap_results$ci_results$rescaled_weights)) {
      warning("Rescaled weight confidence intervals should be interpreted with caution due to compositional data constraints. Use for descriptive purposes only, not formal statistical inference.")
      
      rescaled_ci <- bootstrap_results$ci_results$rescaled_weights
      
      result$Rescaled.RelWeight.CI.Lower <- rescaled_ci$ci_lower[match(result$Variables, rescaled_ci$variable)]
      result$Rescaled.RelWeight.CI.Upper <- rescaled_ci$ci_upper[match(result$Variables, rescaled_ci$variable)]
      
      # Note: Not adding significance indicator for rescaled weights due to interpretation issues
    }
    
    return_list <- list("predictors" = Variables,
                       "rsquare" = rsquare,
                       "result" = result,
                       "n" = complete_cases,
                       "bootstrap" = bootstrap_results,
                       "lambda" = lambda,
                       "RXX" = RXX,
                       "RXY" = RXY)
  } else {
    return_list <- list("predictors" = Variables,
                       "rsquare" = rsquare,
                       "result" = result,
                       "n" = complete_cases,
                       "lambda" = lambda,
                       "RXX" = RXX,
                       "RXY" = RXY)
  }

  return_list
}