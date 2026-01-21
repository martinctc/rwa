#' Bootstrap Relative Weights Analysis
#'
#' Internal bootstrap functions for calculating confidence intervals
#' around relative weights from RWA.
#'
#' @importFrom boot boot boot.ci
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble select all_of mutate relocate last_col n
#' @importFrom tidyr drop_na
#' @importFrom magrittr %>%
#' @importFrom stats rnorm cor
#' @importFrom utils head tail
#'
#' @keywords internal
#' @noRd
NULL

#' Internal RWA calculation helper
#'
#' Core RWA algorithm used by bootstrap functions to avoid code duplication.
#' Returns raw weights and optionally rescaled weights and rsquare.
#'
#' @param thedata Prepared data frame with outcome and predictors
#' @param outcome Name of outcome variable
#' @param predictors Names of predictor variables
#' @param return_all If TRUE, returns list with raw weights, rescaled weights, and rsquare
#' @param use Method for handling missing data in correlations (passed to cor())
#' @param weight Optional name of weight variable for weighted correlations
#'
#' @return Numeric vector of raw weights, or list if return_all=TRUE
#' @noRd
rwa_core_calculation <- function(thedata, outcome, predictors, return_all = FALSE, 
                                 use = "pairwise.complete.obs", weight = NULL) {
  
  # Compute correlation matrix (weighted or unweighted)
  if (!is.null(weight) && weight %in% names(thedata)) {
    # Extract weights and variables for analysis
    weight_values <- thedata[[weight]]
    analysis_data <- thedata %>% dplyr::select(dplyr::all_of(c(outcome, predictors)))
    
    # Remove NA weights if present
    non_na_idx <- !is.na(weight_values)
    weight_values <- weight_values[non_na_idx]
    analysis_data <- analysis_data[non_na_idx, ]
    
    # Require complete cases for cov.wt
    complete_cases_idx <- stats::complete.cases(analysis_data)
    weight_values <- weight_values[complete_cases_idx]
    analysis_data <- analysis_data[complete_cases_idx, , drop = FALSE]
    
    if (nrow(analysis_data) == 0 || sum(weight_values) == 0) {
      stop("Insufficient data for weighted correlation computation.")
    }
    
    # Compute weighted covariance matrix using cov.wt
    cov_result <- stats::cov.wt(
      x = analysis_data,
      wt = weight_values,
      cor = TRUE,
      method = "unbiased"
    )
    
    cor_matrix <- cov_result$cor %>%
      as.data.frame(stringsAsFactors = FALSE, row.names = NULL)
    
  } else {
    # Unweighted correlation
    cor_matrix <- cor(thedata %>% dplyr::select(dplyr::all_of(c(outcome, predictors))), 
                      use = use) %>%
      as.data.frame(stringsAsFactors = FALSE, row.names = NULL)
  }
  
  cor_matrix <- cor_matrix %>%
    remove_all_na_cols() %>%
    tidyr::drop_na()

  matrix_data <- cor_matrix %>% as.matrix()
  
  # Handle single predictor case
  if (ncol(matrix_data) == 2) {
    RXX <- matrix(matrix_data[2, 2], nrow = 1, ncol = 1)
    RXY <- matrix_data[2, 1]
  } else {
    RXX <- matrix_data[2:ncol(matrix_data), 2:ncol(matrix_data)]
    RXY <- matrix_data[2:ncol(matrix_data), 1]
  }

  RXX.eigen <- eigen(RXX)
  D <- diag(RXX.eigen$val, nrow = length(RXX.eigen$val))
  delta <- sqrt(D)
  lambda <- RXX.eigen$vec %*% delta %*% t(RXX.eigen$vec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% RXY
  rsquare <- sum(beta^2)

  RawWgt <- as.vector(lambdasq %*% beta^2)

  if (return_all) {
    RescaledWgt <- (RawWgt / rsquare) * 100
    list(
      raw_weights = RawWgt,
      rescaled_weights = RescaledWgt,
      rsquare = rsquare,
      beta = beta,
      lambda = lambda,
      RXX = RXX,
      RXY = RXY
    )
  } else {
    RawWgt
  }
}

#' Bootstrap statistic function for basic RWA weights
#'
#' Computes raw relative weights for a bootstrap sample.
#'
#' @param data Data frame for bootstrap sampling
#' @param indices Bootstrap sample indices (provided by boot::boot)
#' @param outcome Outcome variable name
#' @param predictors Vector of predictor variable names
#' @param use Method for handling missing data in correlations
#' @param weight Optional name of weight variable
#'
#' @return Numeric vector of raw relative weights
#' @keywords internal
#' @noRd
rwa_boot_statistic <- function(data, indices, outcome, predictors, use = "pairwise.complete.obs", weight = NULL) {
  sample_data <- data[indices, ]

  if (!is.null(weight)) {
    thedata <- sample_data %>%
      dplyr::select(dplyr::all_of(c(outcome, predictors, weight))) %>%
      tidyr::drop_na(dplyr::all_of(outcome))
  } else {
    thedata <- sample_data %>%
      dplyr::select(dplyr::all_of(c(outcome, predictors))) %>%
      tidyr::drop_na(dplyr::all_of(outcome))
  }

  rwa_core_calculation(thedata, outcome, predictors, return_all = FALSE, use = use, weight = weight)
}

#' Bootstrap statistic function for rescaled RWA weights
#'
#' Computes rescaled relative weights (summing to 100) for a bootstrap sample.
#'
#' @param data Data frame for bootstrap sampling
#' @param indices Bootstrap sample indices (provided by boot::boot)
#' @param outcome Outcome variable name
#' @param predictors Vector of predictor variable names
#' @param use Method for handling missing data in correlations
#' @param weight Optional name of weight variable
#'
#' @return Numeric vector of rescaled relative weights (summing to 100)
#' @keywords internal
#' @noRd
rwa_boot_statistic_rescaled <- function(data, indices, outcome, predictors, use = "pairwise.complete.obs", weight = NULL) {
  sample_data <- data[indices, ]

  if (!is.null(weight)) {
    thedata <- sample_data %>%
      dplyr::select(dplyr::all_of(c(outcome, predictors, weight))) %>%
      tidyr::drop_na(dplyr::all_of(outcome))
  } else {
    thedata <- sample_data %>%
      dplyr::select(dplyr::all_of(c(outcome, predictors))) %>%
      tidyr::drop_na(dplyr::all_of(outcome))
  }

  result <- rwa_core_calculation(thedata, outcome, predictors, return_all = TRUE, use = use, weight = weight)
  result$rescaled_weights
}

#' Bootstrap statistic function for comprehensive RWA analysis
#'
#' Computes raw weights, random variable comparison, and focal variable comparison
#' for a bootstrap sample. Used for comprehensive bootstrap analysis.
#'
#' @param data Data frame for bootstrap sampling
#' @param indices Bootstrap sample indices (provided by boot::boot)
#' @param outcome Outcome variable name
#' @param predictors Vector of predictor variable names
#' @param focal Focal variable for comparisons (optional)
#' @param use Method for handling missing data in correlations
#' @param weight Optional name of weight variable
#'
#' @return Numeric vector containing raw weights, random comparison differences,
#'   and (if focal specified) focal comparison differences
#' @keywords internal
#' @noRd
rwa_boot_comprehensive <- function(data, indices, outcome, predictors, focal = NULL, use = "pairwise.complete.obs", weight = NULL) {
  sample_data <- data[indices, ]

  # Get raw weights using core calculation
  raw_weights <- rwa_boot_statistic(sample_data, seq_len(nrow(sample_data)), outcome, predictors, use = use, weight = weight)

  # Get random variable comparison (difference from random variable)
  rand_diff <- rwa_rand_internal(sample_data, outcome, predictors, use = use, weight = weight)

  # Get focal variable comparison if focal is specified
  if (!is.null(focal)) {
    focal_diff <- rwa_comp_internal(sample_data, outcome, predictors, focal, use = use, weight = weight)
    c(raw_weights, rand_diff, focal_diff)
  } else {
    c(raw_weights, rand_diff)
  }
}

#' Internal function for random variable comparison
#'
#' Adds a random variable to the model and computes the difference between
#' each predictor's weight and the random variable's weight. Used for
#' significance testing.
#'
#' @param df Data frame
#' @param outcome Outcome variable name
#' @param predictors Vector of predictor variable names
#' @param use Method for handling missing data in correlations
#' @param weight Optional name of weight variable
#'
#' @return Numeric vector of weight differences (predictor weight - random weight)
#' @keywords internal
#' @noRd
rwa_rand_internal <- function(df, outcome, predictors, use = "pairwise.complete.obs", weight = NULL) {
  if (!is.null(weight)) {
    thedata <- df %>%
      dplyr::select(all_of(c(outcome, predictors, weight))) %>%
      tidyr::drop_na(all_of(outcome)) %>%
      dplyr::mutate(rand = rnorm(dplyr::n(), 0, 1))
  } else {
    thedata <- df %>%
      dplyr::select(all_of(c(outcome, predictors))) %>%
      tidyr::drop_na(all_of(outcome)) %>%
      dplyr::mutate(rand = rnorm(dplyr::n(), 0, 1))
  }

  # Use core calculation with random variable added
  predictors_with_rand <- c(predictors, "rand")
  RawWgt <- rwa_core_calculation(thedata, outcome, predictors_with_rand, return_all = FALSE, use = use, weight = weight)
  
  RawWgt <- RawWgt - tail(RawWgt, n = 1)  # subtract random variable weight
  head(RawWgt, -1)  # remove random variable from output
}

#' Internal function for focal variable comparison
#'
#' Computes the difference between each predictor's weight and the focal
#' variable's weight. Used for comparing predictors against a reference.
#'
#' @param df Data frame
#' @param outcome Outcome variable name
#' @param predictors Vector of predictor variable names
#' @param focal Name of the focal variable to compare against
#' @param use Method for handling missing data in correlations
#' @param weight Optional name of weight variable
#'
#' @return Numeric vector of weight differences (predictor weight - focal weight)
#' @keywords internal
#' @noRd
rwa_comp_internal <- function(df, outcome, predictors, focal, use = "pairwise.complete.obs", weight = NULL) {
  if (!is.null(weight)) {
    thedata <- df %>%
      dplyr::select(all_of(c(outcome, predictors, weight))) %>%
      tidyr::drop_na(all_of(outcome)) %>%
      dplyr::relocate(all_of(focal), .after = dplyr::last_col())
  } else {
    thedata <- df %>%
      dplyr::select(all_of(c(outcome, predictors))) %>%
      tidyr::drop_na(all_of(outcome)) %>%
      dplyr::relocate(all_of(focal), .after = dplyr::last_col())
  }

  # Reorder predictors to match data
  predictors_reordered <- c(predictors[predictors != focal], focal)
  RawWgt <- rwa_core_calculation(thedata, outcome, predictors_reordered, return_all = FALSE, use = use, weight = weight)

  RawWgt <- RawWgt - tail(RawWgt, n = 1)  # subtract focal variable weight
  head(RawWgt, -1)  # remove focal variable from output
}

#' Extract confidence intervals from bootstrap object
#'
#' Extracts confidence intervals from a boot object, trying BCA method first,
#' then falling back to percentile, then basic bootstrap if needed.
#'
#' @param boot_object Boot object from boot::boot()
#' @param conf_level Confidence level (default 0.95)
#' @param variable_names Names of variables for labeling
#' @param ci_type Type of CI to extract ("raw", "rand_diff", "focal_diff")
#'
#' @return Data frame with columns: variable, weight_index, ci_lower, ci_upper,
#'   ci_method, ci_type
#' @keywords internal
#' @noRd
extract_ci <- function(boot_object, conf_level = 0.95, variable_names = NULL, ci_type = "raw") {
  n_weights <- ncol(boot_object$t)

  ci_results <- purrr::map_dfr(1:n_weights, function(i) {
    tryCatch({
      # Try BCA first
      ci <- boot::boot.ci(boot_object, type = "bca", index = i, conf = conf_level)

      if (!is.null(ci$bca) && !any(is.na(ci$bca[4:5]))) {
        ci_lower <- ci$bca[4]
        ci_upper <- ci$bca[5]
        ci_method <- "bca"
      } else {
        # Fallback to percentile method
        ci <- boot::boot.ci(boot_object, type = "perc", index = i, conf = conf_level)
        ci_lower <- ci$percent[4]
        ci_upper <- ci$percent[5]
        ci_method <- "percentile"
      }

      dplyr::tibble(
        variable = if (!is.null(variable_names) && i <= length(variable_names)) {
          variable_names[i]
        } else {
          paste0("Var", i)
        },
        weight_index = i,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        ci_method = ci_method,
        ci_type = ci_type
      )
    }, error = function(e) {
      # If both BCA and percentile fail, try basic bootstrap
      tryCatch({
        ci <- boot::boot.ci(boot_object, type = "basic", index = i, conf = conf_level)
        ci_lower <- ci$basic[4]
        ci_upper <- ci$basic[5]
        ci_method <- "basic"

        dplyr::tibble(
          variable = if (!is.null(variable_names) && i <= length(variable_names)) {
            variable_names[i]
          } else {
            paste0("Var", i)
          },
          weight_index = i,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          ci_method = ci_method,
          ci_type = ci_type
        )
      }, error = function(e2) {
        # If all methods fail, return NA values
        dplyr::tibble(
          variable = if (!is.null(variable_names) && i <= length(variable_names)) {
            variable_names[i]
          } else {
            paste0("Var", i)
          },
          weight_index = i,
          ci_lower = NA_real_,
          ci_upper = NA_real_,
          ci_method = "failed",
          ci_type = ci_type
        )
      })
    })
  })

  ci_results
}

#' Run bootstrap analysis for RWA
#'
#' Main internal function that orchestrates bootstrap analysis for relative
#' weights. Handles raw weights, rescaled weights (optional), and comprehensive
#' analysis with random/focal comparisons (optional).
#'
#' @param data Data frame
#' @param outcome Outcome variable name
#' @param predictors Vector of predictor variable names
#' @param n_bootstrap Number of bootstrap samples (default 1000)
#' @param conf_level Confidence level for intervals (default 0.95)
#' @param focal Focal variable for comparisons (optional)
#' @param comprehensive Whether to run comprehensive analysis with random
#'   variable and focal comparisons
#' @param include_rescaled Whether to bootstrap rescaled weights
#' @param use Method for handling missing data in correlations
#' @param weight Optional name of weight variable
#'
#' @return List containing:
#'   - boot_object: Raw bootstrap object
#'   - ci_results: List of confidence interval data frames
#'   - n_bootstrap, conf_level, comprehensive, focal: Input parameters
#' @keywords internal
#' @noRd
run_rwa_bootstrap <- function(data, outcome, predictors, n_bootstrap = 1000,
                              conf_level = 0.95, focal = NULL, comprehensive = FALSE,
                              include_rescaled = FALSE, use = "pairwise.complete.obs", weight = NULL) {

  # Prepare data
  if (!is.null(weight)) {
    bootstrap_data <- data %>%
      dplyr::select(dplyr::all_of(c(outcome, predictors, weight))) %>%
      tidyr::drop_na(dplyr::all_of(outcome))
  } else {
    bootstrap_data <- data %>%
      dplyr::select(dplyr::all_of(c(outcome, predictors))) %>%
      tidyr::drop_na(dplyr::all_of(outcome))
  }

  # Check sample size
  if (nrow(bootstrap_data) < 50) {
    warning("Sample size is small for bootstrap (n < 50). Results may be unreliable.")
  }

  # Always bootstrap raw weights
  boot_result_raw <- boot::boot(
    data = bootstrap_data,
    statistic = rwa_boot_statistic,
    R = n_bootstrap,
    outcome = outcome,
    predictors = predictors,
    use = use,
    weight = weight
  )

  # Extract CIs for raw weights
  raw_ci <- extract_ci(boot_result_raw, conf_level, predictors, "raw")

  # Initialize results list
  ci_results <- list(raw_weights = raw_ci)
  return_objects <- list(boot_object = boot_result_raw)

  # Bootstrap rescaled weights if requested
  if (include_rescaled) {
    boot_result_rescaled <- boot::boot(
      data = bootstrap_data,
      statistic = rwa_boot_statistic_rescaled,
      R = n_bootstrap,
      outcome = outcome,
      predictors = predictors,
      use = use,
      weight = weight
    )

    rescaled_ci <- extract_ci(boot_result_rescaled, conf_level, predictors, "rescaled")
    ci_results$rescaled_weights <- rescaled_ci
    return_objects$boot_object_rescaled <- boot_result_rescaled
  }

  # Handle comprehensive analysis if requested
  if (comprehensive && !is.null(focal)) {
    boot_result_comp <- boot::boot(
      data = bootstrap_data,
      statistic = rwa_boot_comprehensive,
      R = n_bootstrap,
      outcome = outcome,
      predictors = predictors,
      focal = focal,
      use = use,
      weight = weight
    )

    n_vars <- length(predictors)

    # Extract CIs for random comparison
    if (ncol(boot_result_comp$t) >= 2 * n_vars) {
      rand_ci <- extract_ci(boot_result_comp, conf_level, predictors, "rand_diff")
      # Take the right slice for random comparison
      if (nrow(rand_ci) >= 2 * n_vars) {
        rand_ci <- rand_ci[(n_vars + 1):(2 * n_vars), ]
        rand_ci$weight_index <- 1:n_vars
      }
      ci_results$random_comparison <- rand_ci
    }

    # Extract CIs for focal comparison
    focal_others <- predictors[predictors != focal]
    if (ncol(boot_result_comp$t) >= 2 * n_vars + length(focal_others)) {
      focal_ci <- extract_ci(boot_result_comp, conf_level, focal_others, "focal_diff")
      # Take the right slice for focal comparison
      start_idx <- 2 * n_vars + 1
      end_idx <- start_idx + length(focal_others) - 1
      if (nrow(focal_ci) >= end_idx) {
        focal_ci <- focal_ci[start_idx:end_idx, ]
        focal_ci$weight_index <- seq_along(focal_others)
      }
      ci_results$focal_comparison <- focal_ci
    }

    return_objects$boot_object_comprehensive <- boot_result_comp
  }

  c(return_objects, list(
    ci_results = ci_results,
    n_bootstrap = n_bootstrap,
    conf_level = conf_level,
    comprehensive = comprehensive,
    focal = focal
  ))
}
