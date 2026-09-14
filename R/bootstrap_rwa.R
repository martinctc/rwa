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
  prepared <- prepare_rwa_data(thedata, outcome, predictors, use, weight)
  result <- calculate_rwa(prepared, outcome, predictors, use)
  if (return_all) {
    result
  } else {
    result$raw_weights
  }
}

#' @keywords internal
#' @noRd
with_rwa_bootstrap_errors <- function(expr) {
  tryCatch(expr, error = function(e) {
    stop(paste0("Cannot estimate RWA for bootstrap sample: ", conditionMessage(e),
                " Every sample must retain the requested predictors; no samples are skipped or retried."),
         call. = FALSE)
  })
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
rwa_boot_statistic <- function(data, indices, outcome, predictors, use = "pairwise.complete.obs", weight_var = NULL) {
  with_rwa_bootstrap_errors(
    rwa_core_calculation(data[indices, , drop = FALSE], outcome, predictors,
                         return_all = FALSE, use = use, weight = weight_var)
  )
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
rwa_boot_statistic_rescaled <- function(data, indices, outcome, predictors, use = "pairwise.complete.obs", weight_var = NULL) {
  with_rwa_bootstrap_errors({
    result <- rwa_core_calculation(data[indices, , drop = FALSE], outcome, predictors,
                                   return_all = TRUE, use = use, weight = weight_var)
    result$rescaled_weights
  })
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
rwa_boot_comprehensive <- function(data, indices, outcome, predictors, focal = NULL, use = "pairwise.complete.obs", weight_var = NULL) {
  with_rwa_bootstrap_errors({
    sample_data <- data[indices, , drop = FALSE]

    raw_weights <- rwa_core_calculation(sample_data, outcome, predictors, use = use, weight = weight_var)

    rand_diff <- rwa_rand_internal(sample_data, outcome, predictors, use = use, weight = weight_var)

    if (!is.null(focal)) {
      focal_diff <- rwa_comp_internal(sample_data, outcome, predictors, focal, use = use, weight = weight_var)
      c(raw_weights, rand_diff, focal_diff)
    } else {
      c(raw_weights, rand_diff)
    }
  })
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
  thedata <- df[!is.na(df[[outcome]]), , drop = FALSE]
  random_name <- "rand"
  while (random_name %in% names(thedata)) {
    random_name <- paste0(random_name, "_")
  }
  thedata[[random_name]] <- stats::rnorm(nrow(thedata), 0, 1)

  # Use core calculation with random variable added
  predictors_with_rand <- c(predictors, random_name)
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
  if (!is.character(focal) || length(focal) != 1L || is.na(focal) ||
      !focal %in% predictors) {
    stop("`focal` must name one of the requested predictors.")
  }

  # Reorder predictors to match data
  predictors_reordered <- c(predictors[predictors != focal], focal)
  RawWgt <- rwa_core_calculation(df, outcome, predictors_reordered, return_all = FALSE, use = use, weight = weight)

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
#' @param indices Statistic columns to extract, in variable_names order
#'
#' @return Data frame with columns: variable, weight_index, ci_lower, ci_upper,
#'   ci_method, ci_type
#' @keywords internal
#' @noRd
extract_ci <- function(boot_object, conf_level = 0.95, variable_names = NULL, ci_type = "raw",
                       indices = seq_len(ncol(boot_object$t))) {
  if (!is.null(variable_names) && length(variable_names) != length(indices)) {
    stop("Bootstrap statistic columns must match the supplied variable names.")
  }

  ci_results <- purrr::map_dfr(seq_along(indices), function(i) {
    statistic_index <- indices[i]
    tryCatch({
      # Try BCA first
      ci <- boot::boot.ci(boot_object, type = "bca", index = statistic_index, conf = conf_level)

      if (!is.null(ci$bca) && !any(is.na(ci$bca[4:5]))) {
        ci_lower <- ci$bca[4]
        ci_upper <- ci$bca[5]
        ci_method <- "bca"
      } else {
        # Fallback to percentile method
        ci <- boot::boot.ci(boot_object, type = "perc", index = statistic_index, conf = conf_level)
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
        ci <- boot::boot.ci(boot_object, type = "basic", index = statistic_index, conf = conf_level)
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

  if (comprehensive && !is.null(focal) &&
      (!is.character(focal) || length(focal) != 1L || is.na(focal) ||
       !focal %in% predictors)) {
    stop("`focal` must name one of the requested predictors.")
  }
  prepared <- prepare_rwa_data(data, outcome, predictors, use, weight)
  bootstrap_data <- prepared$frame

  # Check sample size
  if (prepared$n < 50) {
    warning(sprintf("Sample size is small for bootstrap (n < 50; %s complete eligible observations). Results may be unreliable.",
                    prepared$n))
  }

  # Always bootstrap raw weights
  # Note: using weight_var to avoid partial matching with boot::boot's "weights" parameter
  boot_result_raw <- boot::boot(
    data = bootstrap_data,
    statistic = rwa_boot_statistic,
    R = n_bootstrap,
    outcome = outcome,
    predictors = predictors,
    use = use,
    weight_var = weight
  )

  # Extract CIs for raw weights
  raw_ci <- extract_ci(boot_result_raw, conf_level, predictors, "raw")

  # Initialize results list
  ci_results <- list(raw_weights = raw_ci)
  return_objects <- list(boot_object = boot_result_raw)

  # Bootstrap rescaled weights if requested
  if (include_rescaled) {
    # Note: using weight_var to avoid partial matching with boot::boot's "weights" parameter
    boot_result_rescaled <- boot::boot(
      data = bootstrap_data,
      statistic = rwa_boot_statistic_rescaled,
      R = n_bootstrap,
      outcome = outcome,
      predictors = predictors,
      use = use,
      weight_var = weight
    )

    rescaled_ci <- extract_ci(boot_result_rescaled, conf_level, predictors, "rescaled")
    ci_results$rescaled_weights <- rescaled_ci
    return_objects$boot_object_rescaled <- boot_result_rescaled
  }

  # Handle comprehensive analysis if requested
  if (comprehensive) {
    # Note: using weight_var to avoid partial matching with boot::boot's "weights" parameter
    boot_result_comp <- boot::boot(
      data = bootstrap_data,
      statistic = rwa_boot_comprehensive,
      R = n_bootstrap,
      outcome = outcome,
      predictors = predictors,
      focal = focal,
      use = use,
      weight_var = weight
    )

    n_vars <- length(predictors)
    focal_others <- if (is.null(focal)) character() else predictors[predictors != focal]
    expected_length <- 2L * n_vars + length(focal_others)
    if (length(boot_result_comp$t0) != expected_length ||
        ncol(boot_result_comp$t) != expected_length) {
      stop("Comprehensive bootstrap statistic length does not match the requested predictors and comparisons.")
    }

    # Extract CIs for random comparison
    ci_results$random_comparison <- extract_ci(
      boot_result_comp, conf_level, predictors, "rand_diff",
      indices = n_vars + seq_len(n_vars)
    )

    # Extract CIs for focal comparison
    if (length(focal_others)) {
      ci_results$focal_comparison <- extract_ci(
        boot_result_comp, conf_level, focal_others, "focal_diff",
        indices = 2L * n_vars + seq_along(focal_others)
      )
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
