#' @title Bootstrap Relative Weights Analysis
#'
#' @description Internal bootstrap functions for calculating confidence intervals
#' around relative weights from RWA.
#'
#' @param data Data frame for bootstrap sampling
#' @param indices Bootstrap sample indices
#' @param outcome Outcome variable name
#' @param predictors Vector of predictor variable names
#' @param focal Focal variable for comparisons (optional)
#'
#' @importFrom boot boot boot.ci
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble select all_of mutate relocate last_col n
#' @importFrom tidyr drop_na
#' @importFrom magrittr %>%
#' @importFrom stats rnorm
#' @importFrom utils head tail
#'
#' @keywords internal
#' Bootstrap statistic function for basic RWA weights
#' @noRd
rwa_boot_statistic <- function(data, indices, outcome, predictors) {
  sample_data <- data[indices, ]

  # Simplified RWA calculation to avoid circular dependency
  thedata <- sample_data %>%
    dplyr::select(dplyr::all_of(c(outcome, predictors))) %>%
    tidyr::drop_na(dplyr::all_of(outcome))

  cor_matrix <- cor(thedata, use = "pairwise.complete.obs") %>%
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>%
    remove_all_na_cols() %>%
    tidyr::drop_na()

  matrix_data <- cor_matrix %>% as.matrix()
  RXX <- matrix_data[2:ncol(matrix_data), 2:ncol(matrix_data)]
  RXY <- matrix_data[2:ncol(matrix_data), 1]

  RXX.eigen <- eigen(RXX)
  D <- diag(RXX.eigen$val)
  delta <- sqrt(D)
  lambda <- RXX.eigen$vec %*% delta %*% t(RXX.eigen$vec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% RXY

  RawWgt <- lambdasq %*% beta^2
  return(as.vector(RawWgt))
}

#' @keywords internal
#' Bootstrap statistic function for rescaled RWA weights
rwa_boot_statistic_rescaled <- function(data, indices, outcome, predictors) {
  sample_data <- data[indices, ]

  # Simplified RWA calculation
  thedata <- sample_data %>%
    dplyr::select(dplyr::all_of(c(outcome, predictors))) %>%
    tidyr::drop_na(dplyr::all_of(outcome))

  cor_matrix <- cor(thedata, use = "pairwise.complete.obs") %>%
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>%
    remove_all_na_cols() %>%
    tidyr::drop_na()

  matrix_data <- cor_matrix %>% as.matrix()
  RXX <- matrix_data[2:ncol(matrix_data), 2:ncol(matrix_data)]
  RXY <- matrix_data[2:ncol(matrix_data), 1]

  RXX.eigen <- eigen(RXX)
  D <- diag(RXX.eigen$val)
  delta <- sqrt(D)
  lambda <- RXX.eigen$vec %*% delta %*% t(RXX.eigen$vec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% RXY
  rsquare <- sum(beta^2)

  RawWgt <- lambdasq %*% beta^2
  # Calculate rescaled weights properly for each bootstrap sample
  RescaledWgt <- (RawWgt / rsquare) * 100
  return(as.vector(RescaledWgt))
}

#' @keywords internal
#' Bootstrap statistic function for comprehensive RWA analysis
#' Includes: raw weights, random variable comparison, focal variable comparison
rwa_boot_comprehensive <- function(data, indices, outcome, predictors, focal = NULL) {
  sample_data <- data[indices, ]

  # Get raw weights using simplified calculation
  raw_weights <- rwa_boot_statistic(sample_data, 1:nrow(sample_data), outcome, predictors)

  # Get random variable comparison (difference from random variable)
  rand_diff <- rwa_rand_internal(sample_data, outcome, predictors)

  # Get focal variable comparison if focal is specified
  if (!is.null(focal)) {
    focal_diff <- rwa_comp_internal(sample_data, outcome, predictors, focal)
    return(c(raw_weights, rand_diff, focal_diff))
  } else {
    return(c(raw_weights, rand_diff))
  }
}

#' @keywords internal
#' Internal function for random variable comparison
rwa_rand_internal <- function(df, outcome, predictors) {
  thedata <- df %>%
    dplyr::select(all_of(c(outcome, predictors))) %>%
    tidyr::drop_na(all_of(outcome)) %>%
    dplyr::mutate(rand = rnorm(dplyr::n(), 0, 1))

  cor_matrix <- cor(thedata, use = "pairwise.complete.obs") %>%
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>%
    remove_all_na_cols() %>%
    tidyr::drop_na()

  matrix_data <- cor_matrix %>% as.matrix()
  RXX <- matrix_data[2:ncol(matrix_data), 2:ncol(matrix_data)]
  RXY <- matrix_data[2:ncol(matrix_data), 1]

  RXX.eigen <- eigen(RXX)
  D <- diag(RXX.eigen$val)
  delta <- sqrt(D)
  lambda <- RXX.eigen$vec %*% delta %*% t(RXX.eigen$vec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% RXY

  RawWgt <- as.vector(lambdasq %*% beta^2)
  RawWgt <- RawWgt - tail(RawWgt, n = 1)  # subtract random variable weight
  head(RawWgt, -1)  # remove random variable from output
}

#' @keywords internal
#' Internal function for focal variable comparison
rwa_comp_internal <- function(df, outcome, predictors, focal) {
  thedata <- df %>%
    dplyr::select(all_of(c(outcome, predictors))) %>%
    tidyr::drop_na(all_of(outcome)) %>%
    dplyr::relocate(all_of(focal), .after = dplyr::last_col())

  cor_matrix <- cor(thedata, use = "pairwise.complete.obs") %>%
    as.data.frame(stringsAsFactors = FALSE, row.names = NULL) %>%
    remove_all_na_cols() %>%
    tidyr::drop_na()

  matrix_data <- cor_matrix %>% as.matrix()
  RXX <- matrix_data[2:ncol(matrix_data), 2:ncol(matrix_data)]
  RXY <- matrix_data[2:ncol(matrix_data), 1]

  RXX.eigen <- eigen(RXX)
  D <- diag(RXX.eigen$val)
  delta <- sqrt(D)
  lambda <- RXX.eigen$vec %*% delta %*% t(RXX.eigen$vec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% RXY

  RawWgt <- as.vector(lambdasq %*% beta^2)
  RawWgt <- RawWgt - tail(RawWgt, n = 1)  # subtract focal variable weight
  head(RawWgt, -1)  # remove focal variable from output
}

#' Extract confidence intervals from bootstrap object
#'
#' @param boot_object Boot object from boot::boot()
#' @param conf_level Confidence level (default 0.95)
#' @param variable_names Names of variables for labeling
#' @param ci_type Type of CI to extract ("raw", "rand_diff", "focal_diff")
#'
#' @return Data frame with confidence intervals
#' @keywords internal
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
        variable = if (!is.null(variable_names) && i <= length(variable_names)) variable_names[i] else paste0("Var", i),
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
          variable = if (!is.null(variable_names) && i <= length(variable_names)) variable_names[i] else paste0("Var", i),
          weight_index = i,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          ci_method = ci_method,
          ci_type = ci_type
        )
      }, error = function(e2) {
        # If all methods fail, return NA values
        dplyr::tibble(
          variable = if (!is.null(variable_names) && i <= length(variable_names)) variable_names[i] else paste0("Var", i),
          weight_index = i,
          ci_lower = NA_real_,
          ci_upper = NA_real_,
          ci_method = "failed",
          ci_type = ci_type
        )
      })
    })
  })

  return(ci_results)
}

#' Run bootstrap analysis for RWA
#'
#' @param data Data frame
#' @param outcome Outcome variable
#' @param predictors Predictor variables
#' @param n_bootstrap Number of bootstrap samples
#' @param conf_level Confidence level
#' @param focal Focal variable for comparisons (optional)
#' @param comprehensive Whether to run comprehensive analysis
#' @param include_rescaled Whether to bootstrap rescaled weights
#'
#' @return List with bootstrap results and confidence intervals
#' @keywords internal
run_rwa_bootstrap <- function(data, outcome, predictors, n_bootstrap = 1000,
                              conf_level = 0.95, focal = NULL, comprehensive = FALSE,
                              include_rescaled = FALSE) {

  # Prepare data
  bootstrap_data <- data %>%
    dplyr::select(dplyr::all_of(c(outcome, predictors))) %>%
    tidyr::drop_na(dplyr::all_of(outcome))

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
    predictors = predictors
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
      predictors = predictors
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
      focal = focal
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
        focal_ci$weight_index <- 1:length(focal_others)
      }
      ci_results$focal_comparison <- focal_ci
    }

    return_objects$boot_object_comprehensive <- boot_result_comp
  }

  return(c(return_objects, list(
    ci_results = ci_results,
    n_bootstrap = n_bootstrap,
    conf_level = conf_level,
    comprehensive = comprehensive,
    focal = focal
  )))
}
