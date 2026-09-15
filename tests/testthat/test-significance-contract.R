test_that("unrelated predictors are not reported as significant", {
  # The defect in #26: an interval around a non-negative relative weight almost
  # always excludes zero, so pure noise was flagged significant.
  set.seed(1)
  d <- data.frame(x = rnorm(100), y = rnorm(100))
  result <- suppressMessages(
    rwa(d, "y", "x", method = "multiple", bootstrap = TRUE, n_bootstrap = 399)
  )
  expect_gt(cor.test(d$x, d$y)$p.value, 0.9)
  expect_false(result$result$Raw.Significant)
  # The old rule would still flag this predictor, so the test discriminates.
  old_rule <- !(result$result$Raw.RelWeight.CI.Lower <= 0 &
                result$result$Raw.RelWeight.CI.Upper >= 0)
  expect_true(old_rule)
})

test_that("genuine drivers are significant and noise predictors are not", {
  set.seed(7)
  n <- 300
  d <- data.frame(real1 = rnorm(n), real2 = rnorm(n),
                  noise1 = rnorm(n), noise2 = rnorm(n), noise3 = rnorm(n))
  d$y <- 0.6 * d$real1 + 0.4 * d$real2 + rnorm(n)
  predictors <- c("real1", "real2", "noise1", "noise2", "noise3")
  result <- suppressMessages(
    rwa(d, "y", predictors, method = "multiple", bootstrap = TRUE, n_bootstrap = 399)
  )
  significant <- result$result$Variables[result$result$Raw.Significant]
  expect_setequal(significant, c("real1", "real2"))
})

test_that("significance is derived from the random comparison interval", {
  set.seed(11)
  d <- mtcars
  result <- suppressMessages(
    rwa(d, "mpg", c("hp", "wt"), method = "multiple", bootstrap = TRUE, n_bootstrap = 199)
  )
  expect_true(all(c("Random.Diff.CI.Lower", "Random.Diff.CI.Upper",
                    "Raw.RelWeight.CI.Lower", "Raw.RelWeight.CI.Upper",
                    "Raw.Significant") %in% names(result$result)))
  expect_equal(result$result$Raw.Significant,
               result$result$Random.Diff.CI.Lower > 0)
  # Columns are matched by variable name, not row order.
  rand_ci <- result$bootstrap$ci_results$random_comparison
  matched <- match(result$result$Variables, rand_ci$variable)
  expect_equal(result$result$Random.Diff.CI.Lower, rand_ci$ci_lower[matched])
  expect_equal(result$result$Random.Diff.CI.Upper, rand_ci$ci_upper[matched])
  expect_true(all(rand_ci$ci_type == "rand_diff"))
})

test_that("performing worse than the random variable is not significant", {
  # The comparison is directional. An interval lying entirely below zero means
  # the predictor explained less than noise, which must not be flagged. BCa
  # intervals on this near-degenerate statistic can land entirely below zero.
  set.seed(1)
  d <- data.frame(x = rnorm(100), y = rnorm(100))
  set.seed(19)
  result <- suppressWarnings(suppressMessages(
    rwa(d, "y", "x", method = "multiple", bootstrap = TRUE, n_bootstrap = 1999)
  ))
  expect_lt(result$result$Random.Diff.CI.Upper, 0)
  expect_false(result$result$Raw.Significant)
  # A two-sided exclude-zero rule would wrongly flag this predictor.
  two_sided <- !(result$result$Random.Diff.CI.Lower <= 0 &
                 result$result$Random.Diff.CI.Upper >= 0)
  expect_true(two_sided)
})

test_that("the random comparison is available whenever bootstrap runs", {
  set.seed(3)
  plain <- suppressWarnings(run_rwa_bootstrap(mtcars, "mpg", c("hp", "wt"),
                                              n_bootstrap = 39))
  expect_true("random_comparison" %in% names(plain$ci_results))
  expect_identical(plain$ci_results$random_comparison$variable, c("hp", "wt"))
  expect_equal(ncol(plain$boot_object_random$t), 2)
  # Comprehensive analysis supplies the same comparison without a second run.
  set.seed(3)
  comp <- suppressWarnings(run_rwa_bootstrap(mtcars, "mpg", c("hp", "wt"),
                                             n_bootstrap = 39, comprehensive = TRUE))
  expect_true("random_comparison" %in% names(comp$ci_results))
  expect_null(comp$boot_object_random)
  expect_false(is.null(comp$boot_object_comprehensive))
})

test_that("weighted and missing-data bootstraps still produce significance", {
  set.seed(5)
  d <- mtcars
  d$w <- runif(nrow(d), 0.5, 2)
  d$hp[1] <- NA
  for (weight in list(NULL, "w")) {
    result <- suppressWarnings(suppressMessages(
      rwa(d, "mpg", c("hp", "wt"), method = "multiple", weight = weight,
          bootstrap = TRUE, n_bootstrap = 99)
    ))
    expect_type(result$result$Raw.Significant, "logical")
    expect_false(anyNA(result$result$Raw.Significant))
    expect_equal(nrow(result$result), 2)
  }
})
