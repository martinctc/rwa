test_that("singularity errors name the sample-size cause when rows are scarce", {
  # Unit-test the annotation directly: whether a given rank-deficient dataset
  # trips the strictly-positive eigenvalue guard depends on floating-point
  # rounding, so assert the message contract on an exactly singular matrix.
  variables <- c("y", "a", "b")
  singular <- matrix(c(1, 0.5, 0.5,
                       0.5, 1, 1,
                       0.5, 1, 1), nrow = 3, byrow = TRUE,
                     dimnames = list(variables, variables))
  expect_error(validate_rwa_matrix(singular, "y", c("a", "b"), n_obs = 2),
               "singular.*2 usable observation\\(s\\).*2 predictor\\(s\\).*more observations than predictors")
  # With ample observations the same failure stays a pure collinearity report.
  ample <- tryCatch(validate_rwa_matrix(singular, "y", c("a", "b"), n_obs = 500),
                    error = conditionMessage)
  expect_match(ample, "singular")
  expect_false(grepl("more observations than predictors", ample))
  # Omitting the count preserves the previous message exactly.
  expect_identical(
    tryCatch(validate_rwa_matrix(singular, "y", c("a", "b")), error = conditionMessage),
    ample
  )
})

test_that("collinear models with ample data keep the plain singularity message", {
  collinear <- mtcars
  collinear$duplicate <- 2 * collinear$hp
  message <- tryCatch(rwa_multiregress(collinear, "mpg", c("hp", "duplicate")),
                      error = conditionMessage)
  expect_match(message, "singular")
  expect_false(grepl("more observations than predictors", message))
})

test_that("pairwise deletion still estimates models with few rows and many predictors", {
  # Pairwise correlations are computed on differing row subsets, so n <= p does
  # not imply a singular matrix. These models must keep working.
  d <- data.frame(
    y  = c(-.352,  .109,  .321, -.461, -.627,  .573),
    x1 = c(-.433,  .130,  .321, -.491, -.628,  .645),
    x2 = c(   NA, -.568, -1.166, -.719, -.990, -1.513),
    x3 = c(-.239,  .546,  .613, -.467,    NA, -.761),
    x4 = c(-1.655, -.076, 1.889,    NA,  .003, -1.693),
    x5 = c(1.552,  .138,  .566,  .817,    NA,  .207),
    x6 = c( .943, -1.765,   NA, 1.807, -.845, -1.475)
  )
  predictors <- paste0("x", 1:6)
  expect_equal(sum(complete.cases(d)), 2)
  result <- rwa_multiregress(d, "y", predictors)
  expect_true(is.finite(result$rsquare))
  expect_equal(result$rsquare, 0.9995884, tolerance = 1e-6)
  expect_equal(sum(result$result$Rescaled.RelWeight), 100)
})

test_that("pairwise deletion is not rejected by the small-sample diagnostic", {
  # No row is complete across every predictor, but pairwise correlations are
  # computed from far more observations than the complete-case count.
  set.seed(12)
  n <- 120
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- d$x1 + d$x2 + d$x3 + rnorm(n)
  d$x1[seq(1, n, 3)] <- NA
  d$x2[seq(2, n, 3)] <- NA
  d$x3[seq(3, n, 3)] <- NA
  expect_equal(sum(complete.cases(d)), 0)
  result <- rwa_multiregress(d, "y", c("x1", "x2", "x3"))
  expect_true(is.finite(result$rsquare))
  expect_equal(sum(result$result$Rescaled.RelWeight), 100)
})

test_that("weighted sample diagnostics are returned next to n and plotted", {
  d <- mtcars
  d$w <- rep(c(1, 2, 5, 3), 8)
  predictors <- c("hp", "wt")
  weighted <- rwa(d, "mpg", predictors, method = "multiple", weight = "w")
  unweighted <- rwa(d, "mpg", predictors, method = "multiple")

  # Discoverability: the weighted counts sit immediately after `n`.
  expect_identical(
    names(weighted)[seq(which(names(weighted) == "n"), length.out = 3)],
    c("n", "n_weighted", "n_effective")
  )
  # The unweighted contract is unchanged.
  expect_false(any(c("n_weighted", "n_effective") %in% names(unweighted)))

  weighted_caption <- ggplot2::ggplot_build(plot_rwa(weighted))$plot$labels$caption
  unweighted_caption <- ggplot2::ggplot_build(plot_rwa(unweighted))$plot$labels$caption
  expect_equal(weighted$n_weighted, sum(d$w))
  expect_match(weighted_caption, "Weighted analysis: sum of weights = 88")
  expect_match(weighted_caption, "effective n = 24\\.8")
  expect_match(weighted_caption, "R-squared")
  expect_false(grepl("Weighted analysis", unweighted_caption))
  expect_match(unweighted_caption, "^Note: Absolute Rescaled Relative Weights sum to 100%\\. n = 32\\. R-squared")

  # Partial or handmade result lists must still plot.
  partial <- weighted
  partial$n_effective <- NULL
  expect_no_error(plot_rwa(partial))
  expect_match(ggplot2::ggplot_build(plot_rwa(partial))$plot$labels$caption,
               "Weighted analysis: sum of weights = 88\\. ")
  partial2 <- weighted
  partial2$n_weighted <- NULL
  expect_match(ggplot2::ggplot_build(plot_rwa(partial2))$plot$labels$caption,
               "Weighted analysis: effective n = 24\\.8\\. ")
})
