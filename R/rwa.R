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
#' - `rsquare`: the rsquare value of the regression model (multiple regression only).
#' - `result`: the final output of the importance metrics (sorted by
#'   Rescaled.RelWeight in descending order by default).
#'   - The `Rescaled.RelWeight` column sums up to 100.
#'   - The `Sign` column indicates whether a predictor is positively or
#'     negatively correlated with the outcome.
#'   - When bootstrap = TRUE, includes confidence interval columns for raw weights.
#'   - Rescaled weight CIs are available via include_rescaled_ci = TRUE but not
#'     recommended for inference.
#' - `n`: indicates the number of observations used in the analysis.
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
#' # Plot results using plot_rwa()
#' diamonds |>
#'   rwa("price", c("depth", "carat", "x", "y")) |>
#'   plot_rwa()
#'
#' \donttest{
#' # For faster examples, use a subset of data for bootstrap
#' diamonds_small <- diamonds[sample(nrow(diamonds), 1000), ]
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
    if (any(df[[weight]] <= 0, na.rm = TRUE)) {
      stop(sprintf("Weight variable '%s' must have positive values.", weight))
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

  # Compute correlation matrix (weighted or unweighted)
  if (!is.null(weight)) {
    # Extract weights and variables for analysis
    weight_values <- thedata[[weight]]
    analysis_data <- thedata %>% dplyr::select(dplyr::all_of(c(outcome, predictors)))
    
    # Check for zero or NA weights
    if (any(is.na(weight_values))) {
      if (use %in% c("complete.obs", "pairwise.complete.obs")) {
        # Remove rows with NA weights for complete/pairwise cases
        non_na_idx <- !is.na(weight_values)
        weight_values <- weight_values[non_na_idx]
        analysis_data <- analysis_data[non_na_idx, ]
      } else {
        stop("Weight variable contains NA values. Set use = 'complete.obs' or 'pairwise.complete.obs' for automatic removal of NA weights.")
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
  # ---- Handle bootstrap for logistic regression ----

  if (bootstrap && use_logistic) {
    warning("Bootstrap confidence intervals are not yet implemented for logistic regression. ",
            "Proceeding without bootstrap.")
    bootstrap <- FALSE
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
      applysigns = applysigns
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
      include_rescaled = include_rescaled_ci,  # Only include if explicitly requested
      use = use,  # Pass missing data handling method
      weight = weight  # Pass weight variable
    )

    # Add confidence intervals to result dataframe
    if (!is.null(bootstrap_results$ci_results$raw_weights)) {
      ci_data <- bootstrap_results$ci_results$raw_weights

      # Add CI columns for raw weights
      result_list$result$Raw.RelWeight.CI.Lower <-
        ci_data$ci_lower[match(result_list$result$Variables, ci_data$variable)]
      result_list$result$Raw.RelWeight.CI.Upper <-
        ci_data$ci_upper[match(result_list$result$Variables, ci_data$variable)]

      # Add significance indicator for raw weights (if CI doesn't include 0)
      result_list$result$Raw.Significant <-
        !(result_list$result$Raw.RelWeight.CI.Lower <= 0 &
          result_list$result$Raw.RelWeight.CI.Upper >= 0)
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
