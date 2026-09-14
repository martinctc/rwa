#' @keywords internal
#' @noRd
validate_rwa_weights <- function(df, weight) {
  if (is.null(weight)) {
    return(invisible(NULL))
  }
  if (!is.character(weight) || length(weight) != 1L || is.na(weight)) {
    stop("`weight` must be a single character string specifying the weight variable name.")
  }
  if (!weight %in% names(df)) {
    stop(sprintf("Weight variable '%s' not found in data.", weight))
  }
  values <- df[[weight]]
  if (!is.numeric(values)) {
    stop(sprintf("Weight variable '%s' must be numeric.", weight))
  }
  if (any(!is.finite(values[!is.na(values)]))) {
    stop(sprintf("Weight variable '%s' must have finite values.", weight))
  }
  if (any(values <= 0, na.rm = TRUE)) {
    stop(sprintf("Weight variable '%s' must have positive values.", weight))
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
prepare_rwa_data <- function(df, outcome, predictors, use, weight = NULL) {
  valid_use <- c("everything", "all.obs", "complete.obs",
                 "na.or.complete", "pairwise.complete.obs")
  if (length(use) != 1L || is.na(use) || !use %in% valid_use) {
    stop(sprintf("`use` must be one of: %s", paste(valid_use, collapse = ", ")))
  }
  validate_rwa_weights(df, weight)
  if (!is.character(outcome) || length(outcome) != 1L || is.na(outcome)) {
    stop("`outcome` must be a single variable name.")
  }
  if (!outcome %in% names(df)) {
    stop(sprintf("Outcome variable '%s' not found in data.", outcome))
  }
  if (!is.character(predictors) || !length(predictors) ||
      anyNA(predictors) || anyDuplicated(predictors) || outcome %in% predictors) {
    stop("`predictors` must contain distinct variable names, excluding the outcome.")
  }
  missing_predictors <- predictors[!predictors %in% names(df)]
  if (length(missing_predictors)) {
    stop(sprintf("Predictor variable(s) not found in data: %s",
                 paste(missing_predictors, collapse = ", ")))
  }
  if (!is.numeric(df[[outcome]])) {
    stop(sprintf("Outcome variable '%s' must be numeric.", outcome))
  }
  non_numeric <- predictors[!vapply(df[predictors], is.numeric, logical(1))]
  if (length(non_numeric)) {
    stop(sprintf("All predictor variables must be numeric. Non-numeric: %s",
                 paste(non_numeric, collapse = ", ")))
  }

  variables <- c(outcome, predictors)
  # Keep this outcome-complete frame for bootstrap sampling, before other filters.
  frame <- df[!is.na(df[[outcome]]), unique(c(variables, weight)), drop = FALSE]
  analysis_data <- frame[, variables, drop = FALSE]
  weights <- NULL
  if (!is.null(weight)) {
    weights <- frame[[weight]]
    if (anyNA(weights) && use == "all.obs") {
      stop("Weight variable contains NA values and use = 'all.obs'. Set use = 'complete.obs' for listwise deletion.")
    }
    retained <- !is.na(weights) & stats::complete.cases(analysis_data)
    analysis_data <- analysis_data[retained, , drop = FALSE]
    weights <- weights[retained]
  } else if (use %in% c("complete.obs", "na.or.complete")) {
    analysis_data <- analysis_data[stats::complete.cases(analysis_data), , drop = FALSE]
  } else if (use == "all.obs" && anyNA(analysis_data)) {
    stop("Predictor variables contain missing values and use = 'all.obs'. Set `use` to a missing-data deletion method.")
  }

  if (nrow(analysis_data) < 2L) {
    stop("Insufficient data for RWA: at least two eligible observations are required; check outcome, predictor, and weight missingness.")
  }
  non_finite <- variables[vapply(analysis_data, function(x) {
    any(!is.finite(x[!is.na(x)]))
  }, logical(1))]
  if (length(non_finite)) {
    stop(sprintf("RWA requires finite values in analysis variables: %s.",
                 paste(non_finite, collapse = ", ")))
  }
  constant <- variables[vapply(analysis_data, function(x) {
    length(unique(x[!is.na(x)])) < 2L
  }, logical(1))]
  if (length(constant)) {
    stop(sprintf("Cannot estimate RWA: zero variance or insufficient observed values in variable(s): %s. Check constant columns and sample size.",
                 paste(constant, collapse = ", ")))
  }
  list(frame = frame, data = analysis_data, weights = weights,
       n = sum(stats::complete.cases(analysis_data)))
}

#' @keywords internal
#' @noRd
validate_rwa_matrix <- function(matrix_data, outcome, predictors) {
  variables <- c(outcome, predictors)
  if (!is.matrix(matrix_data) || !is.numeric(matrix_data) ||
      !identical(dim(matrix_data), rep(length(variables), 2L)) ||
      !identical(rownames(matrix_data), variables) ||
      !identical(colnames(matrix_data), variables)) {
    stop("RWA correlation matrix dimensions or variable identities do not match the requested outcome and predictors.")
  }
  if (any(!is.finite(matrix_data))) {
    affected <- variables[colSums(!is.finite(matrix_data)) > 0L]
    stop(sprintf("RWA correlation matrix contains non-finite values for: %s. Check constant columns, insufficient pairwise observations, or missing values; consider use = 'complete.obs'.",
                 paste(affected, collapse = ", ")))
  }
  tolerance <- sqrt(.Machine$double.eps)
  if (max(abs(matrix_data - t(matrix_data))) > tolerance ||
      any(abs(diag(matrix_data) - 1) > tolerance)) {
    stop("RWA requires a symmetric correlation matrix with unit diagonal; check variable variances and numerical precision.")
  }
  joint_values <- eigen(matrix_data, symmetric = TRUE, only.values = TRUE)$values
  joint_tolerance <- tolerance * max(1, max(abs(joint_values)))
  if (min(joint_values) < -joint_tolerance) {
    stop(sprintf("Joint outcome/predictor correlation matrix is not positive semidefinite (minimum eigenvalue %.3g; tolerance %.3g). Pairwise missing-data correlations can be incompatible; consider use = 'complete.obs' and inspect missingness. No matrix repair is applied.",
                 min(joint_values), joint_tolerance))
  }
  RXX <- matrix_data[-1L, -1L, drop = FALSE]
  predictor_eigen <- eigen(RXX, symmetric = TRUE)
  if (min(predictor_eigen$values) <= 0) {
    stop(sprintf("Predictor correlation matrix is singular or numerically non-positive-definite for: %s. Check collinearity and sample size (minimum eigenvalue %.3g).",
                 paste(predictors, collapse = ", "),
                 min(predictor_eigen$values)))
  }
  predictor_eigen
}

#' @keywords internal
#' @noRd
calculate_rwa <- function(prepared, outcome, predictors, use) {
  if (is.null(prepared$weights)) {
    matrix_data <- stats::cor(prepared$data, use = use)
  } else {
    # Scaling avoids overflow without changing the correlation estimand.
    normalized_weights <- prepared$weights / max(prepared$weights)
    # The unbiased covariance correction cancels in correlations; ML avoids it.
    matrix_data <- stats::cov.wt(prepared$data, wt = normalized_weights,
                                cor = TRUE, method = "ML")$cor
  }
  predictor_eigen <- validate_rwa_matrix(matrix_data, outcome, predictors)
  delta <- diag(sqrt(predictor_eigen$values), nrow = length(predictors))
  lambda <- predictor_eigen$vectors %*% delta %*% t(predictor_eigen$vectors)
  RXY <- matrix_data[-1L, 1L]
  beta <- solve(lambda) %*% RXY
  rsquare <- sum(beta^2)
  raw_weights <- as.vector(lambda^2 %*% beta^2)
  if (!is.finite(rsquare) || any(!is.finite(raw_weights))) {
    stop("Cannot estimate finite relative weights; check the predictor matrix and numerical precision.")
  }
  if (rsquare > 1 + sqrt(.Machine$double.eps)) {
    stop(sprintf("Calculated R-squared (%.8g) exceeds 1 beyond numerical tolerance. Pairwise correlations may be incompatible, or the predictor matrix may be numerically unstable; consider use = 'complete.obs' and inspect collinearity. No result repair is applied.",
                 rsquare))
  }
  if (rsquare == 0) {
    stop("R-squared is zero; rescaled relative weights are undefined. Check the outcome/predictor relationships.")
  }
  list(raw_weights = raw_weights, rescaled_weights = raw_weights / rsquare * 100,
       rsquare = rsquare, beta = beta, lambda = lambda,
       RXX = matrix_data[-1L, -1L], RXY = RXY)
}
