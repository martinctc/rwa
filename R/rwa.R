#' @title Create a Relative Weights Analysis (RWA)
#'
#' @description This function creates a Relative Weights Analysis (RWA) and
#'   returns a list of outputs. RWA provides a heuristic method for estimating
#'   the relative weight of predictor variables in multiple regression, which
#'   involves creating a multiple regression with on a set of transformed
#'   predictors which are orthogonal to each other but maximally related to the
#'   original set of predictors. `rwa()` is optimised for dplyr pipes and shows
#'   positive / negative signs for weights.
#'
#' @details
#' `rwa()` produces raw relative weight values (epsilons) as well as rescaled
#' weights (scaled as a percentage of predictable variance) for every predictor
#' in the model. Signs are added to the weights when the `applysigns` argument
#' is set to `TRUE`. See <https://www.scotttonidandel.com/rwa-web> for the
#' original implementation that inspired this package.
#'
#' This function is a wrapper around `rwa_multiregress()` and `rwa_logit()`,
#' automatically selecting the appropriate method based on the outcome variable
#' or the `method` argument.
#'
#' In brief, for the two missing-data and weighting arguments:
#' * Without `weight`, missing values are handled by `use`, which defaults to
#'   pairwise deletion when correlating predictors.
#' * With `weight`, the analysis always uses complete cases across the outcome,
#'   the predictors, and the weight. Weighted pairwise deletion is not
#'   implemented, so `use` does not change a weighted result.
#' * In both cases, rows with a missing outcome are dropped first.
#'
#' Use weights when the analysis should represent a target population rather
#' than the achieved sample. Comparing weighted with unweighted results is
#' informative: a large difference indicates that sample composition matters.
#'
#' Multiple-regression estimates require a finite joint correlation matrix.
#' Its smallest eigenvalue must be at least minus
#' `sqrt(.Machine$double.eps) * max(1, max(abs(eigenvalues)))`. The predictor
#' block must have strictly positive computed eigenvalues and the transformation
#' must be solvable. No additional conditioning cutoff is imposed on previously
#' estimable models; highly correlated predictors can still yield sensitive
#' estimates. Calculated R-squared must not exceed one by
#' more than `sqrt(.Machine$double.eps)`; the fit is checked separately because
#' small matrix errors can be amplified by nearly collinear predictors.
#' An exactly fitted outcome (a singular joint matrix) is
#' allowed when the predictor block is positive definite. Invalid matrices,
#' constant variables, and insufficient observations cause informative errors;
#' variables are not dropped and matrices are not silently repaired.
#'
#' Weighted analysis is observation-weighted RWA. Bootstrap intervals use
#' independent, identically distributed (iid) individual-row resampling, with
#' each row's original weight carried along, not sampling proportional to weights.
#' Rows with missing outcomes are removed before resampling; other missing-data
#' filters are applied within each sample, preserving the outcome-complete
#' sampling frame. A degenerate sample stops the bootstrap with an error:
#' samples are not skipped, retried, or allowed to lose predictors.
#' Clusters, strata, and replicate-weight survey designs are not supported;
#' a weight column alone does not provide general complex-survey variance.
#' See `vignette("weighted-missing-data")` for examples and limitations.
#'
#' @param df Data frame or tibble to be passed through.
#' @param outcome Outcome variable, to be specified as a string or bare input.
#'   Must be a numeric variable.
#' @param predictors Predictor variable(s), to be specified as a vector of
#'   string(s) or bare input(s). All variables must be numeric.
#' @param applysigns Logical value specifying whether to show an estimate that
#'   applies the sign. Defaults to `FALSE`.
#' @param method String to specify the method of regression to apply. Valid
#'   values include:
#'   - `"auto"`: automatically detect whether to use multiple regression or
#'     logistic regression based on the outcome variable provided.
#'   - `"multiple"`: use multiple regression.
#'   - `"logistic"`: use logistic regression.
#' @param sort Logical value specifying whether to sort results by rescaled
#'   relative weights in descending order. Defaults to `TRUE`.
#' @param bootstrap Logical value specifying whether to calculate bootstrap
#'   confidence intervals. Defaults to `FALSE`. Currently only supported for
#'   multiple regression.
#' @param n_bootstrap Number of bootstrap samples to use when bootstrap = TRUE.
#'   Defaults to 1000.
#' @param conf_level Confidence level for bootstrap intervals. Defaults to 0.95.
#' @param focal Focal variable for bootstrap comparisons (optional).
#' @param comprehensive Whether to run comprehensive bootstrap analysis
#'   including random variable comparisons and, when `focal` is supplied,
#'   comparisons against that predictor.
#' @param include_rescaled_ci Logical value specifying whether to include
#'   confidence intervals for rescaled weights. Defaults to `FALSE` due to
#'   compositional data constraints. Use with caution.
#' @param use Method for handling missing data when computing correlations. Options are:
#'   "everything" (remaining missing values propagate, causing a non-finite
#'   correlation matrix error if correlations cannot be estimated),
#'   "all.obs" (error for remaining missing predictors in unweighted analysis),
#'   "complete.obs" (listwise deletion),
#'   "na.or.complete" (listwise deletion; no complete cases produces an
#'   insufficient-data error rather than an unusable matrix of NAs),
#'   "pairwise.complete.obs" (pairwise deletion, default).
#'   See \code{\link[stats]{cor}} for more details. Only applicable for multiple regression.
#'   Rows with missing outcomes are always removed first, including for
#'   "all.obs". When \code{weight} is specified, remaining missing weights
#'   cause an error for "all.obs" and are removed otherwise. Missing predictors
#'   are then removed by listwise deletion for every weighted mode, including
#'   "all.obs". Thus weighted correlations always use complete cases, regardless
#'   of \code{use}; weighted pairwise correlation is not implemented.
#' @param weight Optional name of a weight variable in the data frame. If provided,
#'   a weighted correlation matrix will be computed using the specified weights.
#'   Non-missing weights must be numeric, finite, and strictly positive (zero
#'   weights are not supported). Missing weights follow the \code{use} rules.
#'   Defaults to \code{NULL}
#'   (unweighted analysis). Only applicable for multiple regression.
#'
#' @return `rwa()` returns a list of outputs, as follows:
#' - `predictors`: character vector of names of the predictor variables used.
#' - `rsquare`: the rsquare value of the regression model (multiple regression only).
#' - `result`: the final output of the importance metrics (sorted by
#'   Rescaled.RelWeight in descending order by default).
#'   - The `Rescaled.RelWeight` column sums up to 100.
#'   - The `Sign` column indicates whether a predictor is positively or
#'     negatively correlated with the outcome.
#'   - When bootstrap = TRUE, includes confidence interval columns for raw weights.
#'   - When bootstrap = TRUE, `Random.Diff.CI.Lower`/`Random.Diff.CI.Upper` give
#'     the interval for the difference between each predictor's weight and the
#'     weight of a randomly generated variable, and `Raw.Significant` is `TRUE`
#'     when `Random.Diff.CI.Lower` is above zero. Significance is assessed this
#'     way, and not from the interval around the weight itself, because raw
#'     relative weights are non-negative: an unrelated predictor still receives
#'     a small positive weight, so an interval around it would almost always
#'     exclude zero. The test is directional, so an interval lying entirely
#'     below zero indicates a predictor that performed worse than the random
#'     variable and is not significant. See Tonidandel, LeBreton and Johnson
#'     (2009).
#'   - Rescaled weight CIs are available via include_rescaled_ci = TRUE but not
#'     recommended for inference.
#' - `n`: complete-case observation count for the selected analysis variables
#'   (and weight, if supplied). Unweighted pairwise correlations may use more
#'   observations than this conservative count.
#' - `n_weighted`: weighted results only; sum of original weights after all
#'   analysis filters. This is a population-size estimate only for appropriately
#'   calibrated weights and the retained population scope.
#' - `n_effective`: weighted results only; Kish's unequal-weighting effective
#'   sample size, `(sum(w)^2) / sum(w^2)`, calculated using scaled weights for
#'   numerical stability. This diagnostic ignores clustering, stratification,
#'   and weight/outcome relationships; it is not model degrees of freedom or
#'   the exact precision of RWA.
#' - `bootstrap`: bootstrap results (only present when bootstrap = TRUE), containing:
#'   - `ci_results`: confidence intervals for weights
#'   - `boot_object`: raw bootstrap object for advanced analysis
#'   - `n_bootstrap`: number of bootstrap samples used
#' - `lambda`: lambda matrix from the RWA calculation.
#' - `RXX`: Correlation matrix of all the predictor variables against each
#'   other. Not available for logistic regression.
#' - `RXY`: Correlation values of the predictor variables against the outcome
#'   variable. Not available for logistic regression.
#'
#' @seealso [plot_rwa()] for plotting results, [rwa_multiregress()] and
#'   [rwa_logit()] for the underlying implementations.
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr drop_na
#' @importFrom stats cor var
#' @import dplyr
#' @examples
#' library(ggplot2)
#' # Basic RWA (results sorted by default)
#' rwa(diamonds, "price", c("depth", "carat"))
#'
#' # RWA without sorting (preserves original predictor order)
#' rwa(diamonds, "price", c("depth", "carat"), sort = FALSE)
#'
#' # Plot results using plot_rwa()
#' diamonds |>
#'   rwa("price", c("depth", "carat", "x", "y")) |>
#'   plot_rwa()
#'
#' \donttest{
#' # For faster examples, use a subset of data for bootstrap
#' diamonds_small <- diamonds[sample(nrow(diamonds), 1000), ]
#'
#' # RWA with different missing data handling
#' # Use complete.obs for listwise deletion
#' rwa(diamonds_small, "price", c("depth", "carat"), use = "complete.obs")
#'
#' # RWA with weights
#' diamonds_small$sample_weight <- runif(nrow(diamonds_small), 0.5, 2)
#' rwa(diamonds_small, "price", c("depth", "carat"), weight = "sample_weight")
#'
#' # RWA with bootstrap confidence intervals (raw weights only)
#' rwa(diamonds_small, "price", c("depth", "carat"),
#'     bootstrap = TRUE, n_bootstrap = 100)
#'
#' # Include rescaled weight CIs (use with caution for inference)
#' rwa(diamonds_small, "price", c("depth", "carat"),
#'     bootstrap = TRUE, include_rescaled_ci = TRUE, n_bootstrap = 100)
#'
#' # Comprehensive bootstrap analysis with focal variable
#' result <- rwa(diamonds_small, "price", c("depth", "carat", "table"),
#'               bootstrap = TRUE, comprehensive = TRUE, focal = "carat",
#'               n_bootstrap = 100)
#' # View confidence intervals
#' result$bootstrap$ci_results
#' }
#'
#' # Based on logistic regression (auto-detected from binary outcome)
#' diamonds$IsIdeal <- as.numeric(diamonds$cut == "Ideal")
#' rwa(diamonds, "IsIdeal", c("depth", "carat"))
#'
#' @export
rwa <- function(df,
                outcome,
                predictors,
                applysigns = FALSE,
                method = "auto",
                sort = TRUE,
                bootstrap = FALSE,
                n_bootstrap = 1000,
                conf_level = 0.95,
                focal = NULL,
                comprehensive = FALSE,
                include_rescaled_ci = FALSE,
                use = "pairwise.complete.obs",
                weight = NULL) {

  # ---- Input validation ----

  # Validate method

  if (!method %in% c("auto", "multiple", "logistic")) {
    stop("Invalid input for `method`. Must be one of: 'auto', 'multiple', 'logistic'.")
  }

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
  validate_rwa_weights(df, weight)

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

  # ---- Determine regression method ----

  outcome_values <- unique(df[[outcome]])
  outcome_var_unique <- dplyr::n_distinct(outcome_values[!is.na(outcome_values)])

  use_logistic <- (method == "logistic") ||
                  (method == "auto" && outcome_var_unique == 2)

  if (method == "auto") {
    if (use_logistic) {
      message(
        paste0("Parsing `", outcome, "` as a binary variable.\n"),
        "Applying logistic regression to calculate relative weights..."
      )
    } else {
      message(
        paste0("Parsing `", outcome, "` as a non-binary variable.\n"),
        "Applying multiple regression to calculate relative weights..."
      )
    }
  }

  # ---- Handle bootstrap for logistic regression ----

  if (bootstrap && use_logistic) {
    warning("Bootstrap confidence intervals are not yet implemented for logistic regression. ",
            "Proceeding without bootstrap.")
    bootstrap <- FALSE
  }

  # ---- Handle weight/use parameters for logistic regression ----

  if (use_logistic && (!is.null(weight) || use != "pairwise.complete.obs")) {
    warning("Weight and use parameters are only applicable for multiple regression. ",
            "They will be ignored for logistic regression.")
  }

  # ---- Call appropriate sub-function ----

  if (use_logistic) {
    result_list <- rwa_logit(
      df = df,
      outcome = outcome,
      predictors = predictors,
      applysigns = applysigns
    )
  } else {
    result_list <- rwa_multiregress(
      df = df,
      outcome = outcome,
      predictors = predictors,
      applysigns = applysigns,
      use = use,
      weight = weight
    )
  }

  # ---- Apply sorting ----

  if (sort) {
    result_list$result <- result_list$result %>%
      dplyr::arrange(dplyr::desc(Rescaled.RelWeight))
  }

  # ---- Run bootstrap analysis if requested (multiple regression only) ----

  if (bootstrap) {
    message("Running bootstrap analysis with ", n_bootstrap, " samples...")

    bootstrap_results <- run_rwa_bootstrap(
      data = df,
      outcome = outcome,
      predictors = predictors,
      n_bootstrap = n_bootstrap,
      conf_level = conf_level,
      focal = focal,
      comprehensive = comprehensive,
      include_rescaled = include_rescaled_ci,
      use = use,
      weight = weight
    )

    # Add confidence intervals to result dataframe
    if (!is.null(bootstrap_results$ci_results$raw_weights)) {
      ci_data <- bootstrap_results$ci_results$raw_weights

      # Add CI columns for raw weights
      result_list$result$Raw.RelWeight.CI.Lower <-
        ci_data$ci_lower[match(result_list$result$Variables, ci_data$variable)]
      result_list$result$Raw.RelWeight.CI.Upper <-
        ci_data$ci_upper[match(result_list$result$Variables, ci_data$variable)]
    }

    # Significance is assessed by comparing each weight against the weight of a
    # randomly generated variable (Tonidandel, LeBreton & Johnson, 2009). This
    # interval is deliberately not the interval around the weight itself: raw
    # relative weights are non-negative, so an interval around a weight nearly
    # always excludes zero and would flag even unrelated predictors.
    # The test is directional. The statistic is the predictor's weight minus the
    # random variable's weight, so only an interval lying entirely above zero
    # shows the predictor explains more than noise. An interval entirely below
    # zero means the opposite and must not be reported as significant.
    if (!is.null(bootstrap_results$ci_results$random_comparison)) {
      rand_ci <- bootstrap_results$ci_results$random_comparison
      matched <- match(result_list$result$Variables, rand_ci$variable)

      result_list$result$Random.Diff.CI.Lower <- rand_ci$ci_lower[matched]
      result_list$result$Random.Diff.CI.Upper <- rand_ci$ci_upper[matched]
      result_list$result$Raw.Significant <-
        !is.na(result_list$result$Random.Diff.CI.Lower) &
        result_list$result$Random.Diff.CI.Lower > 0
    }

    # Add rescaled weight CIs only if explicitly requested and warn user
    if (include_rescaled_ci && !is.null(bootstrap_results$ci_results$rescaled_weights)) {
      warning("Rescaled weight confidence intervals should be interpreted with caution ",
              "due to compositional data constraints. Use for descriptive purposes only, ",
              "not formal statistical inference.")

      rescaled_ci <- bootstrap_results$ci_results$rescaled_weights

      result_list$result$Rescaled.RelWeight.CI.Lower <-
        rescaled_ci$ci_lower[match(result_list$result$Variables, rescaled_ci$variable)]
      result_list$result$Rescaled.RelWeight.CI.Upper <-
        rescaled_ci$ci_upper[match(result_list$result$Variables, rescaled_ci$variable)]
    }

    result_list$bootstrap <- bootstrap_results
  }

  result_list
}
