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

#' @keywords internal
#' @noRd
unique_column_name <- function(existing, base) {
  name <- base
  while (name %in% existing) {
    name <- paste0(name, "_")
  }
  name
}

#' Attach jackknife influence values used for BCa acceleration
#'
#' When a bootstrap has fewer replicates than observations, `boot::boot.ci()`
#' estimates the BCa acceleration from jackknife influence values. It obtains
#' them by re-calling the statistic without the extra arguments supplied to
#' `boot::boot()`, which fails for RWA statistics. This computes the usual
#' jackknife influence values, `(n - 1) * (t_full - t_{-i})` as in `boot`, once
#' for all statistic columns and stores them on the boot object for
#' `extract_ci()`. When there are at least as many replicates as observations,
#' `boot` estimates the influence values by regression on the replicates, so
#' nothing is stored.
#'
#' Every RWA bootstrap statistic is a function of a single correlation matrix.
#' When the analysis uses one set of complete rows (weighted analyses,
#' listwise deletion, or no missing values), each leave-one-out matrix is
#' obtained by removing one row's contribution from the full-data weighted
#' sums, which avoids refitting from the data n times. Otherwise (pairwise
#' deletion with missing values), the statistic is re-evaluated on each
#' leave-one-out sample.
#'
#' @param boot_object Boot object from `boot::boot()`
#' @param statistic_args Named list of extra arguments passed to the statistic
#' @param matrix_statistic Optional function mapping the joint correlation
#'   matrix of `c(outcome, predictors, random_var)` to the statistic vector
#' @param fast Whether to use the leave-one-out matrix update when eligible
#'
#' @return `boot_object` with `rwa_influence` (an n x k matrix, or `NULL`) and
#'   `rwa_jackknife_error` (a message, or `NULL`)
#' @keywords internal
#' @noRd
attach_bootstrap_influence <- function(boot_object, statistic_args,
                                       matrix_statistic = NULL, fast = TRUE) {
  data <- boot_object$data
  n <- NROW(data)
  boot_object$rwa_influence <- NULL
  boot_object$rwa_jackknife_error <- NULL
  if (boot_object$R >= n) {
    return(boot_object)
  }
  rows_used <- if (fast && !is.null(matrix_statistic)) {
    jackknife_rows_used(data, statistic_args)
  }
  influence <- tryCatch({
    if (!is.null(rows_used)) {
      fast_jackknife_influence(data, statistic_args, rows_used, matrix_statistic)
    } else {
      if (n > 2000L) {
        message(sprintf(paste0(
          "Computing BCa jackknife influence values by refitting RWA %d times, ",
          "because pairwise deletion with missing values uses different rows per ",
          "correlation. This may take a while; `use = \"complete.obs\"` or ",
          "`n_bootstrap >= %d` avoids it."), n, n))
      }
      statistic <- function(indices) {
        do.call(boot_object$statistic, c(list(data, indices), statistic_args))
      }
      t_full <- statistic(seq_len(n))
      jackknife <- vapply(seq_len(n), function(i) statistic(seq_len(n)[-i]),
                          numeric(length(t_full)))
      jackknife <- matrix(jackknife, nrow = length(t_full))
      t((n - 1) * (t_full - jackknife))
    }
  }, error = function(e) {
    boot_object$rwa_jackknife_error <<- conditionMessage(e)
    NULL
  })
  boot_object$rwa_influence <- influence
  boot_object
}

#' Rows used by the RWA correlation matrix when it is a complete-case matrix
#'
#' @return Logical vector over the rows of `data`, or `NULL` when the matrix
#'   is not built from a single set of rows (pairwise deletion with missing
#'   values), in which case the fast jackknife does not apply
#' @keywords internal
#' @noRd
jackknife_rows_used <- function(data, statistic_args) {
  variables <- c(statistic_args$outcome, statistic_args$predictors,
                 statistic_args$random_var)
  use <- if (is.null(statistic_args$use)) "pairwise.complete.obs" else statistic_args$use
  complete <- stats::complete.cases(data[variables])
  weight <- statistic_args$weight_var
  if (!is.null(weight)) {
    complete & !is.na(data[[weight]])
  } else if (use %in% c("complete.obs", "na.or.complete") || all(complete)) {
    complete
  } else {
    NULL
  }
}

#' Jackknife influence values from leave-one-out correlation matrices
#'
#' Rows outside `rows_used` do not enter the statistic, so their influence is
#' exactly zero. For the remaining rows, the weighted mean and cross-product
#' sums are downdated by one row to give the leave-one-out correlation matrix,
#' matching `stats::cor()` and `stats::cov.wt(method = "ML", cor = TRUE)`.
#'
#' @keywords internal
#' @noRd
fast_jackknife_influence <- function(data, statistic_args, rows_used, matrix_statistic) {
  n <- NROW(data)
  variables <- c(statistic_args$outcome, statistic_args$predictors,
                 statistic_args$random_var)
  x <- as.matrix(data[rows_used, variables, drop = FALSE])
  weight <- statistic_args$weight_var
  w <- if (is.null(weight)) rep(1, nrow(x)) else data[[weight]][rows_used]
  w <- w / max(w)
  total <- sum(w)
  centered <- sweep(x, 2L, colSums(x * w) / total)
  cross <- crossprod(centered * sqrt(w))
  sums <- colSums(centered * w)
  # A spread below this is treated as zero variance, matching the slow path.
  min_sd <- sqrt(.Machine$double.eps) * sqrt(diag(cross) / total)

  correlation <- function(total_k, sums_k, cross_k) {
    mean_k <- sums_k / total_k
    covariance <- cross_k / total_k - tcrossprod(mean_k)
    sd <- sqrt(pmax(diag(covariance), 0))
    if (any(!is.finite(sd)) || any(sd <= min_sd)) {
      stop("A variable has zero variance in a jackknife sample.")
    }
    corr <- covariance / tcrossprod(sd)
    corr <- (corr + t(corr)) / 2
    diag(corr) <- 1
    dimnames(corr) <- list(variables, variables)
    corr
  }

  # Removing a row that dominates a variable's spread loses precision in the
  # downdate, so such rows are recomputed directly from the remaining rows.
  dominant <- apply(w * centered^2, 1L, function(r) any(r > 0.5 * diag(cross)))

  t_full <- matrix_statistic(correlation(total, sums, cross))
  jackknife <- vapply(seq_len(nrow(x)), function(k) {
    if (dominant[k]) {
      rest <- x[-k, , drop = FALSE]
      w_rest <- w[-k]
      rest <- sweep(rest, 2L, colSums(rest * w_rest) / sum(w_rest))
      return(matrix_statistic(correlation(sum(w_rest), rep(0, ncol(rest)),
                                          crossprod(rest * sqrt(w_rest)))))
    }
    row <- centered[k, ]
    matrix_statistic(correlation(total - w[k], sums - w[k] * row,
                                 cross - w[k] * tcrossprod(row)))
  }, numeric(length(t_full)))
  jackknife <- matrix(jackknife, nrow = length(t_full))

  influence <- matrix(0, nrow = n, ncol = length(t_full))
  influence[which(rows_used), ] <- t((n - 1) * (t_full - jackknife))
  influence
}

#' Map a joint correlation matrix to an RWA bootstrap statistic
#'
#' Mirrors `rwa_boot_statistic()`, `rwa_boot_statistic_rescaled()`,
#' `rwa_boot_rand_statistic()` and `rwa_boot_comprehensive()`, using the joint
#' correlation matrix of `c(outcome, predictors, random_var)`.
#'
#' @keywords internal
#' @noRd
rwa_matrix_statistic <- function(type, outcome, predictors, focal = NULL,
                                 random_var = NULL) {
  weights_for <- function(corr, vars) {
    rwa_from_matrix(corr[c(outcome, vars), c(outcome, vars), drop = FALSE],
                    outcome, vars)
  }
  difference_from_last <- function(corr, vars) {
    raw <- weights_for(corr, vars)$raw_weights
    head(raw, -1) - tail(raw, 1)
  }
  switch(type,
    raw = function(corr) weights_for(corr, predictors)$raw_weights,
    rescaled = function(corr) weights_for(corr, predictors)$rescaled_weights,
    rand = function(corr) difference_from_last(corr, c(predictors, random_var)),
    comprehensive = function(corr) {
      out <- c(weights_for(corr, predictors)$raw_weights,
               difference_from_last(corr, c(predictors, random_var)))
      if (!is.null(focal)) {
        out <- c(out, difference_from_last(corr, c(predictors[predictors != focal], focal)))
      }
      out
    },
    stop("Unknown RWA matrix statistic type.")
  )
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

#' Bootstrap statistic function for random-variable comparison
#'
#' Computes the difference between each predictor's raw relative weight and the
#' raw relative weight of a randomly generated variable added to the model.
#' This is the comparison used to assess statistical significance, following
#' Tonidandel, LeBreton and Johnson (2009). A relative weight cannot be tested
#' against zero directly, because relative weights are non-negative and a
#' predictor with no true relationship still receives a small positive weight.
#'
#' Following step 1 of Tonidandel, LeBreton and Johnson (2009, p. 392), the
#' random variable is added once to the original data and is then resampled
#' with the rows; `run_rwa_bootstrap()` supplies it through `random_var`.
#'
#' @param data Data frame for bootstrap sampling
#' @param indices Bootstrap sample indices (provided by boot::boot)
#' @param outcome Outcome variable name
#' @param predictors Vector of predictor variable names
#' @param use Method for handling missing data in correlations
#' @param weight_var Optional name of weight variable
#' @param random_var Name of the column in `data` holding the random variable.
#'   If `NULL`, a new random variable is drawn for this call.
#'
#' @return Numeric vector of weight differences, one per predictor
#' @keywords internal
#' @noRd
rwa_boot_rand_statistic <- function(data, indices, outcome, predictors,
                                    use = "pairwise.complete.obs", weight_var = NULL,
                                    random_var = NULL) {
  with_rwa_bootstrap_errors({
    rwa_rand_internal(data[indices, , drop = FALSE], outcome, predictors,
                      use = use, weight = weight_var, random_var = random_var)
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
#' @param weight_var Optional name of weight variable
#' @param random_var Name of the column in `data` holding the random variable
#'   (see `rwa_boot_rand_statistic()`). If `NULL`, a new one is drawn per call.
#'
#' @return Numeric vector containing raw weights, random comparison differences,
#'   and (if focal specified) focal comparison differences
#' @keywords internal
#' @noRd
rwa_boot_comprehensive <- function(data, indices, outcome, predictors, focal = NULL, use = "pairwise.complete.obs", weight_var = NULL,
                                   random_var = NULL) {
  with_rwa_bootstrap_errors({
    sample_data <- data[indices, , drop = FALSE]

    raw_weights <- rwa_core_calculation(sample_data, outcome, predictors, use = use, weight = weight_var)

    rand_diff <- rwa_rand_internal(sample_data, outcome, predictors, use = use, weight = weight_var,
                                   random_var = random_var)

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
#' @param random_var Optional name of an existing column to use as the random
#'   predictor; if `NULL`, a new standard normal variable is drawn
#'
#' @return Numeric vector of weight differences (predictor weight - random weight)
#' @keywords internal
#' @noRd
rwa_rand_internal <- function(df, outcome, predictors, use = "pairwise.complete.obs", weight = NULL,
                              random_var = NULL) {
  thedata <- df[!is.na(df[[outcome]]), , drop = FALSE]
  if (is.null(random_var)) {
    random_name <- unique_column_name(names(thedata), "rand")
    thedata[[random_name]] <- stats::rnorm(nrow(thedata), 0, 1)
  } else {
    random_name <- random_var
  }

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
#' Extracts confidence intervals from a boot object using BCa intervals, as
#' recommended by Tonidandel, LeBreton and Johnson (2009, p. 392). If a BCa
#' interval cannot be computed, falls back to the percentile interval (the
#' method of Johnson, 2004, as described by Tonidandel et al., 2009, p. 392),
#' then to the basic interval, which is kept only from earlier package versions,
#' and warns which statistics fell back and why.
#'
#' BCa acceleration uses empirical influence values (Efron, 1987). When there
#' are fewer replicates than observations, these are the jackknife values stored
#' on the boot object by `attach_bootstrap_influence()`; otherwise `boot`
#' estimates them by regression on the replicates.
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

  influence <- boot_object$rwa_influence
  influence_error <- boot_object$rwa_jackknife_error
  fallback_reasons <- character()

  ci_results <- purrr::map_dfr(seq_along(indices), function(i) {
    statistic_index <- indices[i]
    variable <- if (!is.null(variable_names)) variable_names[i] else paste0("Var", i)

    bca <- if (!is.null(influence_error)) {
      simpleError(paste("jackknife influence values could not be computed:", influence_error))
    } else {
      tryCatch({
        args <- list(boot_object, type = "bca", index = statistic_index, conf = conf_level)
        if (!is.null(influence)) {
          args$L <- influence[, statistic_index]
        }
        ci <- do.call(boot::boot.ci, args)
        if (is.null(ci$bca) || any(!is.finite(ci$bca[4:5]))) {
          stop("BCa interval endpoints are not finite")
        }
        ci$bca[4:5]
      }, error = function(e) e)
    }

    if (!inherits(bca, "error")) {
      bounds <- bca
      ci_method <- "bca"
    } else {
      fallback_reasons[[variable]] <<- conditionMessage(bca)
      fallback <- tryCatch({
        ci <- boot::boot.ci(boot_object, type = "perc", index = statistic_index, conf = conf_level)
        list(bounds = ci$percent[4:5], method = "percentile")
      }, error = function(e) {
        tryCatch({
          ci <- boot::boot.ci(boot_object, type = "basic", index = statistic_index, conf = conf_level)
          list(bounds = ci$basic[4:5], method = "basic")
        }, error = function(e2) list(bounds = c(NA_real_, NA_real_), method = "failed"))
      })
      bounds <- fallback$bounds
      ci_method <- fallback$method
    }

    dplyr::tibble(
      variable = variable,
      weight_index = i,
      ci_lower = bounds[1],
      ci_upper = bounds[2],
      ci_method = ci_method,
      ci_type = ci_type
    )
  })

  if (length(fallback_reasons)) {
    by_reason <- tapply(names(fallback_reasons), unname(fallback_reasons),
                        paste, collapse = ", ")
    details <- paste(sprintf("%s (%s)", names(by_reason), by_reason), collapse = "; ")
    warning(sprintf(paste0("BCa confidence intervals could not be computed for %s: %s. ",
                           "Fell back to the method shown in `ci_method`."),
                    ci_type, details),
            call. = FALSE)
  }

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

  statistic_args <- list(outcome = outcome, predictors = predictors, use = use,
                         weight_var = weight)

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
  boot_result_raw <- attach_bootstrap_influence(
    boot_result_raw, statistic_args,
    rwa_matrix_statistic("raw", outcome, predictors)
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
    boot_result_rescaled <- attach_bootstrap_influence(
      boot_result_rescaled, statistic_args,
      rwa_matrix_statistic("rescaled", outcome, predictors)
    )

    rescaled_ci <- extract_ci(boot_result_rescaled, conf_level, predictors, "rescaled")
    ci_results$rescaled_weights <- rescaled_ci
    return_objects$boot_object_rescaled <- boot_result_rescaled
  }

  # Tonidandel, LeBreton and Johnson (2009, p. 392), step 1: add the random
  # variable once to the original data, then resample it with the rows.
  random_name <- unique_column_name(names(bootstrap_data), "rand")
  random_data <- bootstrap_data
  random_data[[random_name]] <- stats::rnorm(nrow(random_data), 0, 1)
  random_args <- c(statistic_args, list(random_var = random_name))

  # Handle comprehensive analysis if requested
  if (comprehensive) {
    # Note: using weight_var to avoid partial matching with boot::boot's "weights" parameter
    boot_result_comp <- boot::boot(
      data = random_data,
      statistic = rwa_boot_comprehensive,
      R = n_bootstrap,
      outcome = outcome,
      predictors = predictors,
      focal = focal,
      use = use,
      weight_var = weight,
      random_var = random_name
    )
    boot_result_comp <- attach_bootstrap_influence(
      boot_result_comp, c(random_args, list(focal = focal)),
      rwa_matrix_statistic("comprehensive", outcome, predictors, focal = focal,
                           random_var = random_name)
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
  } else {
    # The significance test compares each weight against a random variable's
    # weight (Tonidandel, LeBreton & Johnson, 2009), so this comparison is
    # required whenever bootstrap results are produced. Comprehensive analysis
    # already computes it above, so it is only run separately here.
    boot_result_rand <- boot::boot(
      data = random_data,
      statistic = rwa_boot_rand_statistic,
      R = n_bootstrap,
      outcome = outcome,
      predictors = predictors,
      use = use,
      weight_var = weight,
      random_var = random_name
    )
    boot_result_rand <- attach_bootstrap_influence(
      boot_result_rand, random_args,
      rwa_matrix_statistic("rand", outcome, predictors, random_var = random_name)
    )

    if (length(boot_result_rand$t0) != length(predictors) ||
        ncol(boot_result_rand$t) != length(predictors)) {
      stop("Random-comparison bootstrap statistic length does not match the requested predictors.")
    }

    ci_results$random_comparison <- extract_ci(
      boot_result_rand, conf_level, predictors, "rand_diff"
    )
    return_objects$boot_object_random <- boot_result_rand
  }

  c(return_objects, list(
    ci_results = ci_results,
    n_bootstrap = n_bootstrap,
    conf_level = conf_level,
    comprehensive = comprehensive,
    focal = focal
  ))
}
