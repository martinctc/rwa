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
  
  # Significance follows the random-variable comparison, not the interval
  # around the weight itself (Tonidandel, LeBreton & Johnson, 2009), and is
  # directional: only a lower bound above zero beats noise.
  expect_equal(result$result$Raw.Significant,
               result$result$Random.Diff.CI.Lower > 0)
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

test_that("BCa intervals are returned when replicates are fewer than observations", {
  skip_on_cran()
  # Regression test for https://github.com/martinctc/rwa/issues/31: with
  # R < n, boot.ci() needs jackknife influence values and previously failed,
  # silently returning basic intervals instead.
  set.seed(31)
  n <- 150
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n), w = runif(n, 0.5, 2))
  d$y <- 0.5 * d$x1 + 0.3 * d$x2 + rnorm(n)
  predictors <- c("x1", "x2", "x3")

  for (weight in list(NULL, "w")) {
    result <- rwa(d, "y", predictors, bootstrap = TRUE, n_bootstrap = 99,
                  weight = weight)
    ci <- result$bootstrap$ci_results
    expect_true(all(ci$raw_weights$ci_method == "bca"))
    expect_true(all(ci$random_comparison$ci_method == "bca"))

    boot_raw <- result$bootstrap$boot_object
    expected_L <- boot::empinf(boot_raw, type = "jack", index = 2, outcome = "y",
                               predictors = predictors, use = "pairwise.complete.obs",
                               weight_var = weight)
    expect_equal(boot_raw$rwa_influence[, 2], expected_L, tolerance = 1e-10)
  }
})

test_that("BCa fallbacks are reported with a warning", {
  set.seed(5)
  boot_raw <- boot::boot(mtcars, rwa_boot_statistic, R = 99, outcome = "mpg",
                         predictors = c("cyl", "hp"), weight_var = NULL)
  boot_raw$rwa_influence_error <- "test failure"
  expect_warning(
    ci <- extract_ci(boot_raw, variable_names = c("cyl", "hp")),
    "BCa confidence intervals could not be computed for raw \\(cyl, hp\\).*test failure"
  )
  expect_true(all(ci$ci_method == "percentile"))
})
