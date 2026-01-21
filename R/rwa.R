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
#' @param use Method for handling missing data when computing correlations. Options are: 
#'   "everything" (missing values in correlations propagate), 
#'   "all.obs" (error if missing values present),
#'   "complete.obs" (listwise deletion),
#'   "na.or.complete" (error if some but not all missing),
#'   "pairwise.complete.obs" (pairwise deletion, default).
#'   See \code{\link[stats]{cor}} for more details.
#' @param weight Optional name of a weight variable in the data frame. If provided,
#'   a weighted correlation matrix will be computed using the specified weights.
#'   The weight variable must be numeric and positive. Defaults to \code{NULL} 
#'   (unweighted analysis).
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
#' @importFrom stats cor var
#' @import dplyr
#' @examples
#' library(ggplot2)
#' # Basic RWA (results sorted by default)
#' rwa(diamonds,"price",c("depth","carat"))
#' 
#' # RWA without sorting (preserves original predictor order)
#' rwa(diamonds,"price",c("depth","carat"), sort = FALSE)
#' 
#' # RWA with different missing data handling
#' # Use complete.obs for listwise deletion
#' rwa(diamonds,"price",c("depth","carat"), use = "complete.obs")
#' 
#' # Use pairwise.complete.obs for pairwise deletion (default)
#' rwa(diamonds,"price",c("depth","carat"), use = "pairwise.complete.obs")
#' 
#' \donttest{
#' # For faster examples, use a subset of data for bootstrap
#' diamonds_small <- diamonds[sample(nrow(diamonds), 1000), ]
#' 
#' # RWA with weights
#' diamonds_small$sample_weight <- runif(nrow(diamonds_small), 0.5, 2)
#' rwa(diamonds_small,"price",c("depth","carat"), weight = "sample_weight")
#' 
#' # RWA with bootstrap confidence intervals (raw weights only)
#' rwa(diamonds_small,"price",c("depth","carat"), bootstrap = TRUE, n_bootstrap = 100)
#' 
#' # Include rescaled weight CIs (use with caution for inference)
#' rwa(diamonds_small,"price",c("depth","carat"), bootstrap = TRUE, 
#'     include_rescaled_ci = TRUE, n_bootstrap = 100)
#' 
#' # Comprehensive bootstrap analysis with focal variable
#' result <- rwa(diamonds_small,"price",c("depth","carat","table"), 
#'               bootstrap = TRUE, comprehensive = TRUE, focal = "carat", 
#'               n_bootstrap = 100)
#' # View confidence intervals
#' result$bootstrap$ci_results
#' }
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
                include_rescaled_ci = FALSE,
                use = "pairwise.complete.obs",
                weight = NULL){


  # ---- Input validation ----
  

  # Validate conf_level
  if (!is.numeric(conf_level) || length(conf_level) != 1 || 
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single numeric value between 0 and 1 (exclusive).")
  }
  
  # Validate n_bootstrap
  if (!is.numeric(n_bootstrap) || length(n_bootstrap) != 1 || 
      n_bootstrap < 1 || n_bootstrap != floor(n_bootstrap)) {
    stop("`n_bootstrap` must be a positive integer.")
  }
  
  # Validate use parameter
  valid_use_options <- c("everything", "all.obs", "complete.obs", 
                         "na.or.complete", "pairwise.complete.obs")
  if (!use %in% valid_use_options) {
    stop(sprintf("`use` must be one of: %s", 
                 paste(valid_use_options, collapse = ", ")))
  }
  
  # Validate weight parameter if provided
  if (!is.null(weight)) {
    if (!is.character(weight) || length(weight) != 1) {
      stop("`weight` must be a single character string specifying the weight variable name.")
    }
    if (!weight %in% names(df)) {
      stop(sprintf("Weight variable '%s' not found in data.", weight))
    }
    if (!is.numeric(df[[weight]])) {
      stop(sprintf("Weight variable '%s' must be numeric.", weight))
    }
    if (any(df[[weight]] < 0, na.rm = TRUE)) {
      stop(sprintf("Weight variable '%s' must have non-negative values.", weight))
    }
  }
  
  # Check that outcome and predictors exist in data
  if (!outcome %in% names(df)) {
    stop(sprintf("Outcome variable '%s' not found in data.", outcome))
  }
  
  missing_predictors <- predictors[!predictors %in% names(df)]
  if (length(missing_predictors) > 0) {
    stop(sprintf("Predictor variable(s) not found in data: %s", 
                 paste(missing_predictors, collapse = ", ")))
  }
  
  # Validate that outcome is numeric
  if (!is.numeric(df[[outcome]])) {
    stop(sprintf("Outcome variable '%s' must be numeric.", outcome))
  }
  
  # Validate that all predictors are numeric
  non_numeric_predictors <- predictors[!sapply(df[predictors], is.numeric)]
  if (length(non_numeric_predictors) > 0) {
    stop(sprintf("All predictor variables must be numeric. Non-numeric: %s", 
                 paste(non_numeric_predictors, collapse = ", ")))
  }

  # Gets data frame in right order and form
  if (!is.null(weight)) {
    thedata <-
      df %>%
      dplyr::select(dplyr::all_of(c(outcome, predictors, weight))) %>%
      tidyr::drop_na(dplyr::all_of(outcome))
  } else {
    thedata <-
      df %>%
      dplyr::select(dplyr::all_of(c(outcome, predictors))) %>%
      tidyr::drop_na(dplyr::all_of(outcome))
  }

  # Check for zero-variance outcome
  outcome_var <- stats::var(thedata[[outcome]], na.rm = TRUE)
  if (is.na(outcome_var) || outcome_var == 0) {
    stop(sprintf("Outcome variable '%s' has zero variance.", outcome))
  }
  
  # Check for zero-variance predictors
  predictor_vars <- sapply(thedata[predictors], function(x) stats::var(x, na.rm = TRUE))
  zero_var_predictors <- names(predictor_vars)[is.na(predictor_vars) | predictor_vars == 0]
  if (length(zero_var_predictors) > 0) {
    stop(sprintf("Predictor variable(s) with zero variance: %s", 
                 paste(zero_var_predictors, collapse = ", ")))
  }

  # Compute correlation matrix (weighted or unweighted)
  if (!is.null(weight)) {
    # Extract weights and variables for analysis
    weight_values <- thedata[[weight]]
    analysis_data <- thedata %>% dplyr::select(dplyr::all_of(c(outcome, predictors)))
    
    # Check for zero or NA weights
    if (any(is.na(weight_values))) {
      if (use == "complete.obs") {
        # Remove rows with NA weights
        non_na_idx <- !is.na(weight_values)
        weight_values <- weight_values[non_na_idx]
        analysis_data <- analysis_data[non_na_idx, ]
      } else {
        stop("Weight variable contains NA values. Use use='complete.obs' for listwise deletion or remove NA weights from data.")
      }
    }
    
    if (sum(weight_values) == 0) {
      stop("Sum of weights is zero. Cannot compute weighted correlation.")
    }
    
    # Compute weighted covariance matrix using cov.wt
    # Note: cov.wt requires complete cases
    complete_cases_idx <- stats::complete.cases(analysis_data)
    if (sum(complete_cases_idx) == 0) {
      stop("No complete cases available for weighted correlation computation.")
    }
    
    cov_result <- stats::cov.wt(
      x = analysis_data[complete_cases_idx, , drop = FALSE],
      wt = weight_values[complete_cases_idx],
      cor = TRUE,
      method = "unbiased"
    )
    
    cor_matrix <- cov_result$cor %>%
      as.data.frame(stringsAsFactors = FALSE, row.names = NULL)
    
  } else {
    # Unweighted correlation
    cor_matrix <-
      cor(thedata[, c(outcome, predictors)], use = use) %>%
      as.data.frame(stringsAsFactors = FALSE, row.names = NULL)
  }
  
  cor_matrix <- cor_matrix %>%
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

  # Check for singular/near-singular correlation matrix (perfect collinearity)
  RXX_det <- det(as.matrix(RXX))
  if (abs(RXX_det) < .Machine$double.eps * 100) {
    stop("Predictor correlation matrix is singular or near-singular. This usually indicates perfect or near-perfect collinearity among predictors. Consider removing highly correlated predictors.")
  }

  RXX.eigen <- eigen(RXX) # Compute eigenvalues and eigenvectors of matrix
  
  # Check for negative eigenvalues (indicates numerical issues)
  if (any(RXX.eigen$val < 0)) {
    warning("Correlation matrix has negative eigenvalues, which may indicate numerical instability. Results should be interpreted with caution.")
  }
  
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
      include_rescaled = include_rescaled_ci,  # Only include if explicitly requested
      use = use,  # Pass missing data handling method
      weight = weight  # Pass weight variable
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