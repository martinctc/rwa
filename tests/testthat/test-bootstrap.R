test_that("bootstrap functions exist and are accessible", {
  # Check that the bootstrap functions are available
  expect_true(exists("run_rwa_bootstrap", where = asNamespace("rwa"), inherits = FALSE))
})

test_that("bootstrap confidence intervals are reasonable", {
  skip_on_cran()  # Skip on CRAN due to computation time
  
  # Test basic bootstrap functionality
  result <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"), 
        bootstrap = TRUE, n_bootstrap = 100)
  
  # Check CI structure
  expect_true(all(c("Raw.RelWeight.CI.Lower", "Raw.RelWeight.CI.Upper") %in% 
                  names(result$result)))
  
  # Check CI bounds are reasonable
  ci_lower <- result$result$Raw.RelWeight.CI.Lower
  ci_upper <- result$result$Raw.RelWeight.CI.Upper
  point_est <- result$result$Raw.RelWeight
  
  # Lower bound should be <= point estimate <= upper bound
  expect_true(all(ci_lower <= point_est, na.rm = TRUE))
  expect_true(all(point_est <= ci_upper, na.rm = TRUE))
  
  # CIs should be positive (raw weights are always positive)
  expect_true(all(ci_lower >= 0, na.rm = TRUE))
  expect_true(all(ci_upper >= 0, na.rm = TRUE))
})

test_that("bootstrap with different confidence levels works", {
  skip_on_cran()
  
  # Use fixed seed for reproducibility to avoid flaky test failures
  # See: https://github.com/martinctc/rwa/issues/20
  set.seed(42)
  
  # Test with 90% confidence level
  result_90 <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"), 
        bootstrap = TRUE, n_bootstrap = 150, conf_level = 0.90)
  
  # Test with 99% confidence level  
  result_99 <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"), 
        bootstrap = TRUE, n_bootstrap = 150, conf_level = 0.99)
  
  # 99% CIs should be wider than 90% CIs
  width_90 <- result_90$result$Raw.RelWeight.CI.Upper - result_90$result$Raw.RelWeight.CI.Lower
  width_99 <- result_99$result$Raw.RelWeight.CI.Upper - result_99$result$Raw.RelWeight.CI.Lower
  
  expect_true(all(width_99 >= width_90, na.rm = TRUE))
})

test_that("bootstrap significance testing works", {
  skip_on_cran()
  
  result <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"), 
        bootstrap = TRUE, n_bootstrap = 100)
  
  # Check significance column exists
  expect_true("Raw.Significant" %in% names(result$result))
  expect_type(result$result$Raw.Significant, "logical")
  
  # Check significance logic
  ci_excludes_zero <- !(result$result$Raw.RelWeight.CI.Lower <= 0 & 
                        result$result$Raw.RelWeight.CI.Upper >= 0)
  expect_equal(result$result$Raw.Significant, ci_excludes_zero)
})

test_that("bootstrap with include_rescaled_ci works and warns", {
  skip_on_cran()
  
  # Should warn about rescaled CI interpretation
  expect_warning(
    result <- mtcars %>%
      rwa(outcome = "mpg", predictors = c("cyl", "hp"), 
          bootstrap = TRUE, n_bootstrap = 50, include_rescaled_ci = TRUE),
    "Rescaled weight confidence intervals should be interpreted with caution"
  )
  
  # Should include rescaled CI columns
  expect_true("Rescaled.RelWeight.CI.Lower" %in% names(result$result))
  expect_true("Rescaled.RelWeight.CI.Upper" %in% names(result$result))
})
