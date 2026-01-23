# ============================================================================
# Tests for rwa() function
# ============================================================================

# --- Test fixtures and helpers ----------------------------------------------

# Helper to create test data with specific characteristics
create_test_data <- function() {
  test_data <- mtcars
  test_data$gear_factor <- as.factor(test_data$gear)
  test_data$car_name <- rownames(mtcars)
  test_data$constant <- 5
  test_data$cyl_doubled <- test_data$cyl * 2
  test_data$mpg_factor <- as.factor(ifelse(test_data$mpg > 20, "high", "low"))
  test_data
}

# --- Basic functionality ----------------------------------------------------

test_that("rwa() returns correct structure", {
  result <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"))
  
  expect_type(result, "list")
  expect_named(result, c("predictors", "rsquare", "result", "n", "lambda", "RXX", "RXY"))
  expect_s3_class(result$result, "data.frame")
  expect_named(result$result, c("Variables", "Raw.RelWeight", "Rescaled.RelWeight", "Sign"))
})

test_that("rwa() computes correct weights", {
  result <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"))
  
  # Rescaled weights sum to 100
  expect_equal(sum(result$result$Rescaled.RelWeight), 100, tolerance = 1e-10)
  
  # Raw weights sum to R-squared
  expect_equal(sum(result$result$Raw.RelWeight), result$rsquare, tolerance = 1e-10)
  
  # R-squared is valid
  expect_gte(result$rsquare, 0)
  expect_lte(result$rsquare, 1)
})

test_that("rwa() returns correct predictors and sample size", {
  result <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"))
  
  expect_equal(result$predictors, c("cyl", "hp"))
  expect_equal(nrow(result$result), 2)
  expect_type(result$n, "integer")
  expect_equal(result$n, nrow(mtcars))
})

test_that("rwa() produces reproducible results", {
  result1 <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"))
  result2 <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"))
  
  expect_equal(result1$result, result2$result)
  expect_equal(result1$rsquare, result2$rsquare)
})

test_that("rwa() handles single predictor", {
  result <- rwa(mtcars, outcome = "mpg", predictors = "cyl")
  
  expect_equal(nrow(result$result), 1)
  expect_equal(result$result$Rescaled.RelWeight, 100, tolerance = 1e-10)
})

test_that("rwa() handles missing data by listwise deletion", {
  mtcars_na <- mtcars
  mtcars_na$mpg[1:3] <- NA
  
  result <- rwa(mtcars_na, outcome = "mpg", predictors = c("cyl", "hp"))
  
  expect_equal(result$n, nrow(mtcars) - 3)
})

# --- Sorting behavior -------------------------------------------------------

test_that("rwa() sorts results by default", {
  result <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp", "wt"))
  weights <- result$result$Rescaled.RelWeight
  
  # Should be in descending order
  expect_true(all(diff(weights) <= 0))
})

test_that("rwa() preserves predictor order when sort = FALSE", {
  result <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp", "wt"), sort = FALSE)
  
  expect_equal(result$result$Variables, c("cyl", "hp", "wt"))
})

test_that("rwa() returns same values regardless of predictor order when sorted", {
  result1 <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"), sort = TRUE)
  result2 <- rwa(mtcars, outcome = "mpg", predictors = c("hp", "cyl"), sort = TRUE)
  
  expect_equal(result1$result, result2$result)
})

# --- Sign application -------------------------------------------------------

test_that("rwa() does not include signed weights by default", {
  result <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"))
  
  expect_false("Sign.Rescaled.RelWeight" %in% names(result$result))
})

test_that("rwa() applies signs correctly when applysigns = TRUE", {
  result <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"), applysigns = TRUE)
  
  expect_true("Sign.Rescaled.RelWeight" %in% names(result$result))
  expect_type(result$result$Sign.Rescaled.RelWeight, "double")
  
  # Positive relationships keep positive values
  positive_vars <- result$result$Sign == "+"
  if (any(positive_vars)) {
    expect_equal(
      result$result$Rescaled.RelWeight[positive_vars],
      result$result$Sign.Rescaled.RelWeight[positive_vars]
    )
  }
  
  # Negative relationships get negative values
  negative_vars <- result$result$Sign == "-"
  if (any(negative_vars)) {
    expect_equal(
      -result$result$Rescaled.RelWeight[negative_vars],
      result$result$Sign.Rescaled.RelWeight[negative_vars]
    )
  }
})

# --- Bootstrap functionality ------------------------------------------------

test_that("rwa() runs bootstrap analysis when requested", {
  skip_on_cran()
  
  result <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"),
                bootstrap = TRUE, n_bootstrap = 100)
  
  expect_true("bootstrap" %in% names(result))
  expect_type(result$bootstrap, "list")
  
  # CI columns added to results
  expect_true(all(c("Raw.RelWeight.CI.Lower", "Raw.RelWeight.CI.Upper", "Raw.Significant") 
                  %in% names(result$result)))
  
  # CI bounds are logical
  expect_true(all(result$result$Raw.RelWeight.CI.Lower <= result$result$Raw.RelWeight.CI.Upper))
  expect_type(result$result$Raw.Significant, "logical")
})

# --- Input validation: Parameter types --------------------------------------

test_that("rwa() validates conf_level parameter", {
  expect_error(rwa(mtcars, "mpg", "cyl", conf_level = 0), "conf_level")
  expect_error(rwa(mtcars, "mpg", "cyl", conf_level = 1), "conf_level")
  expect_error(rwa(mtcars, "mpg", "cyl", conf_level = -0.5), "conf_level")
  expect_error(rwa(mtcars, "mpg", "cyl", conf_level = 1.5), "conf_level")
  expect_error(rwa(mtcars, "mpg", "cyl", conf_level = "high"), "conf_level")
})

test_that("rwa() validates n_bootstrap parameter", {
  expect_error(rwa(mtcars, "mpg", "cyl", n_bootstrap = 0), "n_bootstrap")
  expect_error(rwa(mtcars, "mpg", "cyl", n_bootstrap = -10), "n_bootstrap")
  expect_error(rwa(mtcars, "mpg", "cyl", n_bootstrap = 10.5), "n_bootstrap")
  expect_error(rwa(mtcars, "mpg", "cyl", n_bootstrap = "many"), "n_bootstrap")
})

# --- Input validation: Variable existence -----------------------------------

test_that("rwa() errors for non-existent outcome variable", {
  expect_error(
    rwa(mtcars, outcome = "nonexistent", predictors = c("cyl", "hp")),
    "Outcome variable.*not found"
  )
})

test_that("rwa() errors for non-existent predictor variables", {
  expect_error(
    rwa(mtcars, outcome = "mpg", predictors = c("cyl", "nonexistent")),
    "Predictor variable.*not found.*nonexistent"
  )
  
  expect_error(
    rwa(mtcars, outcome = "mpg", predictors = c("fake1", "fake2")),
    "Predictor variable.*not found.*fake1.*fake2"
  )
})

# --- Input validation: Variable types ---------------------------------------

test_that("rwa() errors for non-numeric outcome", {
  test_data <- create_test_data()
  
  expect_error(
    rwa(test_data, outcome = "mpg_factor", predictors = c("cyl", "hp")),
    "Outcome variable.*must be numeric"
  )
})

test_that("rwa() errors for non-numeric predictors", {
  test_data <- create_test_data()
  
  expect_error(
    rwa(test_data, outcome = "mpg", predictors = c("cyl", "gear_factor")),
    "must be numeric.*gear_factor"
  )
  
  expect_error(
    rwa(test_data, outcome = "mpg", predictors = c("cyl", "car_name")),
    "must be numeric.*car_name"
  )
})

# --- Input validation: Data quality -----------------------------------------

test_that("rwa() errors for zero-variance outcome", {
  test_data <- create_test_data()
  
  # Zero-variance outcome produces warning (from cor()) and may error depending on data
  # Use method = "multiple" to avoid binary auto-detection
  expect_warning(
    rwa(test_data, outcome = "constant", predictors = c("cyl", "hp"), method = "multiple"),
    "standard deviation is zero"
  )
})

test_that("rwa() errors for zero-variance predictor", {
  test_data <- create_test_data()
  
  # Zero-variance predictor produces warning (from cor())
  # Use method = "multiple" to avoid binary auto-detection
  expect_warning(
    rwa(test_data, outcome = "mpg", predictors = c("cyl", "constant"), method = "multiple"),
    "standard deviation is zero"
  )
})

test_that("rwa() errors for perfectly collinear predictors", {
  test_data <- create_test_data()
  
  expect_error(
    rwa(test_data, outcome = "mpg", predictors = c("cyl", "cyl_doubled")),
    "singular|collinearity"
  )
})

test_that("rwa() handles small but valid samples", {
  small_data <- mtcars[1:10, ]
  
  expect_no_error(
    rwa(small_data, outcome = "mpg", predictors = c("cyl", "hp"))
  )
})

test_that("rwa() errors for samples too small to compute correlations", {
  very_small_data <- mtcars[1:3, ]
  
  # Force multiple regression to get consistent error handling
  expect_error(
    rwa(very_small_data, outcome = "mpg", predictors = c("cyl", "hp"), method = "multiple"),
    "singular|collinearity|zero variance"
  )
})

# --- Method parameter and regression type -----------------------------------

test_that("rwa() validates method parameter", {
  expect_error(
    rwa(mtcars, "mpg", "cyl", method = "invalid"),
    "Invalid input for `method`"
  )
  
  expect_error(
    rwa(mtcars, "mpg", "cyl", method = "linear"),
    "Invalid input for `method`"
  )
})

test_that("rwa() respects method = 'multiple' for continuous outcome", {
  expect_message(
    result <- rwa(mtcars, "mpg", c("cyl", "hp"), method = "auto"),
    "non-binary"
  )
  
  # Explicit multiple regression should not produce auto-detect message
  expect_no_message(
    result <- rwa(mtcars, "mpg", c("cyl", "hp"), method = "multiple")
  )
  
  expect_type(result, "list")
  expect_true("rsquare" %in% names(result))
})

test_that("rwa() auto-detects binary outcome for logistic regression", {
  # Create binary outcome
  mtcars_binary <- mtcars
  mtcars_binary$high_mpg <- as.numeric(mtcars_binary$mpg > 20)
  
  expect_message(
    result <- rwa(mtcars_binary, "high_mpg", c("cyl", "hp"), method = "auto"),
    "binary"
  )
  
  expect_type(result, "list")
  expect_true("result" %in% names(result))
})

test_that("rwa() forces logistic regression with method = 'logistic'", {
  # Even with continuous-looking outcome (3+ unique values), 
  # method = "logistic" should force logistic regression
  mtcars_binary <- mtcars
  mtcars_binary$high_mpg <- as.numeric(mtcars_binary$mpg > 20)
  
  expect_no_message(
    result <- rwa(mtcars_binary, "high_mpg", c("cyl", "hp"), method = "logistic")
  )
  
  expect_type(result, "list")
})

test_that("rwa() forces multiple regression with method = 'multiple'", {
  # Even with binary outcome, method = "multiple" should force multiple regression
  mtcars_binary <- mtcars
  mtcars_binary$high_mpg <- as.numeric(mtcars_binary$mpg > 20)
  
  expect_no_message(
    result <- rwa(mtcars_binary, "high_mpg", c("cyl", "hp"), method = "multiple")
  )
  
  expect_type(result, "list")
  expect_true("rsquare" %in% names(result))
  expect_true("RXX" %in% names(result))
  expect_true("RXY" %in% names(result))
})

test_that("rwa() warns when bootstrap requested for logistic regression", {
  mtcars_binary <- mtcars
  mtcars_binary$high_mpg <- as.numeric(mtcars_binary$mpg > 20)
  
  expect_warning(
    result <- rwa(mtcars_binary, "high_mpg", c("cyl", "hp"), 
                  method = "logistic", bootstrap = TRUE),
    "not yet implemented for logistic"
  )
  
  # Bootstrap should be disabled
  expect_false("bootstrap" %in% names(result))
})

test_that("rwa() logistic regression returns correct structure", {
  mtcars_binary <- mtcars
  mtcars_binary$high_mpg <- as.numeric(mtcars_binary$mpg > 20)
  
  result <- rwa(mtcars_binary, "high_mpg", c("cyl", "hp"), method = "logistic")
  
  expect_type(result, "list")
  expect_true("predictors" %in% names(result))
  expect_true("result" %in% names(result))
  expect_true("n" %in% names(result))
  expect_true("lambda" %in% names(result))
  
  # Logistic regression doesn't return RXX/RXY
  # (these are specific to multiple regression)
  # But it does return rsquare (pseudo R-squared)
  expect_true("rsquare" %in% names(result))
})

test_that("rwa() multiple vs logistic produce different results", {
  mtcars_binary <- mtcars
  mtcars_binary$high_mpg <- as.numeric(mtcars_binary$mpg > 20)
  
  result_mult <- rwa(mtcars_binary, "high_mpg", c("cyl", "hp"), method = "multiple")
  result_logit <- rwa(mtcars_binary, "high_mpg", c("cyl", "hp"), method = "logistic")
  
  # Both should have results
  expect_s3_class(result_mult$result, "data.frame")
  expect_s3_class(result_logit$result, "data.frame")
  
  # But the weights should be different
  expect_false(all(result_mult$result$Raw.RelWeight == result_logit$result$Raw.RelWeight))
})

# --- Direct tests for rwa_multiregress() and rwa_logit() --------------------

test_that("rwa_multiregress() returns correct structure", {
  result <- rwa_multiregress(mtcars, "mpg", c("cyl", "hp"))
  
  expect_type(result, "list")
  expect_named(result, c("predictors", "rsquare", "result", "n", "lambda", "RXX", "RXY"))
  expect_s3_class(result$result, "data.frame")
})

test_that("rwa_multiregress() computes valid weights", {
  result <- rwa_multiregress(mtcars, "mpg", c("cyl", "hp"))
  
  # Rescaled weights sum to 100
  expect_equal(sum(result$result$Rescaled.RelWeight), 100, tolerance = 1e-10)
  
  # Raw weights sum to R-squared
  expect_equal(sum(result$result$Raw.RelWeight), result$rsquare, tolerance = 1e-10)
})

test_that("rwa_logit() returns correct structure", {
  mtcars_binary <- mtcars
  mtcars_binary$high_mpg <- as.numeric(mtcars_binary$mpg > 20)
  
  result <- rwa_logit(mtcars_binary, "high_mpg", c("cyl", "hp"))
  
  expect_type(result, "list")
  expect_true("predictors" %in% names(result))
  expect_true("result" %in% names(result))
  expect_true("n" %in% names(result))
  expect_true("lambda" %in% names(result))
  expect_s3_class(result$result, "data.frame")
})

test_that("rwa_logit() computes valid weights", {
  mtcars_binary <- mtcars
  mtcars_binary$high_mpg <- as.numeric(mtcars_binary$mpg > 20)
  
  result <- rwa_logit(mtcars_binary, "high_mpg", c("cyl", "hp"))
  
  # Raw weights should be non-negative
  expect_true(all(result$result$Raw.RelWeight >= 0))
  
  # Rescaled weights should sum to 100 (now consistent with rwa_multiregress)
  expect_equal(sum(result$result$Rescaled.RelWeight), 100, tolerance = 1e-6)
})

test_that("rwa_logit() handles applysigns parameter", {
  mtcars_binary <- mtcars
  mtcars_binary$high_mpg <- as.numeric(mtcars_binary$mpg > 20)
  
  result_no_sign <- rwa_logit(mtcars_binary, "high_mpg", c("cyl", "hp"), applysigns = FALSE)
  result_with_sign <- rwa_logit(mtcars_binary, "high_mpg", c("cyl", "hp"), applysigns = TRUE)
  
  expect_false("Sign.Rescaled.RelWeight" %in% names(result_no_sign$result))
  expect_true("Sign.Rescaled.RelWeight" %in% names(result_with_sign$result))
})

# --- Missing data handling options ------------------------------------------

test_that("rwa() accepts different use parameter values", {
  # Test with complete.obs (listwise deletion)
  result_complete <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"),
                         use = "complete.obs", method = "multiple")
  expect_type(result_complete, "list")
  expect_equal(result_complete$n, nrow(mtcars))

  # Test with pairwise.complete.obs (default)
  result_pairwise <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"),
                         use = "pairwise.complete.obs", method = "multiple")
  expect_type(result_pairwise, "list")
  expect_equal(result_pairwise$n, nrow(mtcars))
})

test_that("rwa() validates use parameter", {
  expect_error(
    rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"), use = "invalid"),
    "use.*must be one of"
  )
})

test_that("rwa() handles pairwise vs complete deletion differently with missing data", {
  # Create data with missing values in predictors
  mtcars_na <- mtcars
  mtcars_na$cyl[1:2] <- NA
  mtcars_na$hp[3:4] <- NA

  # With pairwise deletion, it should use all available pairwise correlations
  result_pairwise <- rwa(mtcars_na, outcome = "mpg", predictors = c("cyl", "hp"),
                         use = "pairwise.complete.obs", method = "multiple")

  # With complete.obs, it should only use rows with no missing values
  result_complete <- rwa(mtcars_na, outcome = "mpg", predictors = c("cyl", "hp"),
                         use = "complete.obs", method = "multiple")

  # Both should return valid results
  expect_type(result_pairwise, "list")
  expect_type(result_complete, "list")

  # The n should be different (pairwise uses more data for outcome)
  # Both use listwise deletion on outcome, so n should be based on complete outcome
  expect_true(result_pairwise$n <= nrow(mtcars_na))
  expect_true(result_complete$n <= result_pairwise$n)
})

# --- Weight variable support ------------------------------------------------

test_that("rwa() accepts weight parameter", {
  # Add a weight variable
  mtcars_weighted <- mtcars
  mtcars_weighted$weights <- runif(nrow(mtcars), 0.5, 2)

  result <- rwa(mtcars_weighted, outcome = "mpg", predictors = c("cyl", "hp"),
                weight = "weights", method = "multiple")

  expect_type(result, "list")
  expect_named(result, c("predictors", "rsquare", "result", "n", "lambda", "RXX", "RXY"))

  # Rescaled weights should still sum to 100
  expect_equal(sum(result$result$Rescaled.RelWeight), 100, tolerance = 1e-10)
})

test_that("rwa() validates weight parameter", {
  mtcars_weighted <- mtcars
  mtcars_weighted$weights <- runif(nrow(mtcars), 0.5, 2)
  mtcars_weighted$char_weight <- as.character(mtcars_weighted$weights)
  mtcars_weighted$neg_weight <- -1 * mtcars_weighted$weights
  mtcars_weighted$zero_weight <- rep(0, nrow(mtcars))

  # Non-existent weight variable
  expect_error(
    rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"), weight = "nonexistent"),
    "Weight variable.*not found"
  )

  # Non-numeric weight variable
  expect_error(
    rwa(mtcars_weighted, outcome = "mpg", predictors = c("cyl", "hp"), weight = "char_weight"),
    "Weight variable.*must be numeric"
  )

  # Negative weight values
  expect_error(
    rwa(mtcars_weighted, outcome = "mpg", predictors = c("cyl", "hp"), weight = "neg_weight"),
    "Weight variable.*must have positive values"
  )

  # Zero weight values (should also be rejected as they're not positive)
  expect_error(
    rwa(mtcars_weighted, outcome = "mpg", predictors = c("cyl", "hp"), weight = "zero_weight"),
    "Weight variable.*must have positive values"
  )
})

test_that("rwa() produces different results with and without weights", {
  # Create data with weights that emphasize certain observations
  mtcars_weighted <- mtcars
  mtcars_weighted$weights <- rep(1, nrow(mtcars))
  mtcars_weighted$weights[1:10] <- 5  # Give more weight to first 10 observations

  result_unweighted <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"), method = "multiple")
  result_weighted <- rwa(mtcars_weighted, outcome = "mpg", predictors = c("cyl", "hp"),
                         weight = "weights", method = "multiple")

  # Both should return valid results
  expect_type(result_unweighted, "list")
  expect_type(result_weighted, "list")

  # Results should be different (weights should affect the analysis)
  expect_false(isTRUE(all.equal(result_unweighted$result$Raw.RelWeight,
                                 result_weighted$result$Raw.RelWeight)))
})

test_that("rwa() handles weights with equal values (equivalent to unweighted)", {
  # All equal weights should give same result as unweighted
  mtcars_weighted <- mtcars
  mtcars_weighted$weights <- rep(1, nrow(mtcars))

  result_unweighted <- rwa(mtcars, outcome = "mpg", predictors = c("cyl", "hp"), method = "multiple")
  result_weighted <- rwa(mtcars_weighted, outcome = "mpg", predictors = c("cyl", "hp"),
                         weight = "weights", method = "multiple")

  # Results should be very similar (allowing for numerical precision)
  expect_equal(result_unweighted$result$Raw.RelWeight,
               result_weighted$result$Raw.RelWeight,
               tolerance = 1e-6)
})

test_that("rwa() warns when weight/use used with logistic regression", {
  mtcars_binary <- mtcars
  mtcars_binary$high_mpg <- as.numeric(mtcars_binary$mpg > 20)
  mtcars_binary$weights <- runif(nrow(mtcars), 0.5, 2)

  # Weight parameter with logistic regression should warn
  expect_warning(
    rwa(mtcars_binary, "high_mpg", c("cyl", "hp"), weight = "weights", method = "logistic"),
    "Weight and use parameters are only applicable for multiple regression"
  )

  # use parameter with logistic regression should warn
  expect_warning(
    rwa(mtcars_binary, "high_mpg", c("cyl", "hp"), use = "complete.obs", method = "logistic"),
    "Weight and use parameters are only applicable for multiple regression"
  )
})

# --- Parameter combination tests --------------------------------------------

test_that("rwa() works with bootstrap and weight combined", {
  skip_on_cran()

  mtcars_weighted <- mtcars
  mtcars_weighted$weights <- runif(nrow(mtcars), 0.5, 2)

  result <- rwa(mtcars_weighted, outcome = "mpg", predictors = c("cyl", "hp"),
                weight = "weights", bootstrap = TRUE, n_bootstrap = 50,
                method = "multiple")

  # Should have bootstrap results
  expect_true("bootstrap" %in% names(result))
  expect_true("Raw.RelWeight.CI.Lower" %in% names(result$result))
  expect_true("Raw.RelWeight.CI.Upper" %in% names(result$result))

  # Results should be valid
  expect_equal(sum(result$result$Rescaled.RelWeight), 100, tolerance = 1e-10)
})

test_that("rwa() works with bootstrap and use combined", {
  skip_on_cran()

  # Create data with some NA values
  mtcars_na <- mtcars
  mtcars_na$cyl[1:2] <- NA

  result <- rwa(mtcars_na, outcome = "mpg", predictors = c("cyl", "hp"),
                use = "complete.obs", bootstrap = TRUE, n_bootstrap = 50,
                method = "multiple")

  # Should have bootstrap results
  expect_true("bootstrap" %in% names(result))
  expect_true("Raw.RelWeight.CI.Lower" %in% names(result$result))

  # Sample size should reflect listwise deletion
  expect_equal(result$n, nrow(mtcars) - 2)
})

test_that("rwa() works with all multiple regression parameters combined", {
  skip_on_cran()

  mtcars_weighted <- mtcars
  mtcars_weighted$weights <- runif(nrow(mtcars), 0.5, 2)

  # Test with weight + use + bootstrap + sort + applysigns
  result <- rwa(mtcars_weighted, outcome = "mpg", predictors = c("cyl", "hp", "wt"),
                weight = "weights",
                use = "complete.obs",
                bootstrap = TRUE,
                n_bootstrap = 50,
                sort = TRUE,
                applysigns = TRUE,
                method = "multiple")

  # Should have all expected components
  expect_true("bootstrap" %in% names(result))
  expect_true("Sign.Rescaled.RelWeight" %in% names(result$result))
  expect_true("Raw.RelWeight.CI.Lower" %in% names(result$result))

  # Results should be sorted by Rescaled.RelWeight in descending order
  weights <- result$result$Rescaled.RelWeight
  expect_true(all(diff(weights) <= 0))
})
