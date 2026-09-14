test_that("joint indefiniteness is rejected even with an invertible predictor block", {
  d <- data.frame(
    y = c(-1, 0, 1, -10, 10, -10, 10),
    x1 = c(-1, 0, 1, -10, 10, NA, NA),
    x2 = c(1, -1, 0, NA, NA, -10, 10)
  )
  joint <- cor(d, use = "pairwise.complete.obs")
  expect_gt(min(eigen(joint[-1, -1], symmetric = TRUE)$values), 0)
  expect_lt(min(eigen(joint, symmetric = TRUE)$values), 0)
  invalid_rsquare <- as.numeric(t(joint[-1, 1]) %*% solve(joint[-1, -1], joint[-1, 1]))
  expect_gt(invalid_rsquare, 1)
  expect_error(rwa(d, "y", c("x1", "x2"), method = "multiple"),
               "Joint.*not positive semidefinite.*complete.obs")
  expect_error(rwa_multiregress(d, "y", c("x1", "x2")), "Joint.*not positive semidefinite")
  expect_error(rwa_boot_statistic(d, seq_len(nrow(d)), "y", c("x1", "x2")),
               "bootstrap sample: Joint.*not positive semidefinite")
  expect_equal(rwa_multiregress(d, "y", c("x1", "x2"), use = "complete.obs")$rsquare, 1)
})

test_that("singular predictors fail but full joint exact fits remain valid", {
  d <- mtcars
  d$duplicate <- 2 * d$hp
  d$y <- 3 * d$wt - d$hp
  d$w <- rep(c(1, 2), 16)
  for (weight in list(NULL, "w")) {
    expect_error(rwa_multiregress(d, "mpg", c("hp", "duplicate"), weight = weight),
                 "singular.*hp, duplicate.*collinearity")
    exact <- rwa_multiregress(d, "y", c("wt", "hp"), weight = weight)
    expect_equal(exact$rsquare, 1, tolerance = 1e-12)
    expect_equal(sum(exact$result$Raw.RelWeight), 1, tolerance = 1e-12)
    expect_equal(rwa_boot_statistic(d, seq_len(nrow(d)), "y", c("wt", "hp"),
                                    weight_var = weight), exact$result$Raw.RelWeight)
  }
})

test_that("matrix validation preserves dimensions, identities, and numerical tolerance", {
  variables <- c("y", "a", "b")
  joint <- diag(3)
  dimnames(joint) <- list(variables, variables)
  expect_no_error(validate_rwa_matrix(joint, "y", c("a", "b")))
  expect_error(validate_rwa_matrix(joint[-1, -1], "y", c("a", "b")), "dimensions or variable identities")
  expect_error(validate_rwa_matrix(joint, "y", c("b", "a")), "dimensions or variable identities")
  bad <- joint
  bad[1, 2] <- bad[2, 1] <- NA
  expect_error(validate_rwa_matrix(bad, "y", c("a", "b")), "non-finite.*y, a")
  bad <- joint
  bad[1, 1] <- 2
  expect_error(validate_rwa_matrix(bad, "y", c("a", "b")), "unit diagonal")
  bad <- joint
  bad[1, 2] <- 0.1
  expect_error(validate_rwa_matrix(bad, "y", c("a", "b")), "symmetric")
  rounded <- joint
  rounded[1, 2] <- rounded[2, 1] <- 1 + 1e-10
  expect_no_error(validate_rwa_matrix(rounded, "y", c("a", "b")))
  rounded[1, 2] <- rounded[2, 1] <- 1 + 1e-3
  expect_error(validate_rwa_matrix(rounded, "y", c("a", "b")), "not positive semidefinite")
  near_singular <- joint
  near_singular[2, 3] <- near_singular[3, 2] <- 1 - .Machine$double.eps
  expect_no_error(validate_rwa_matrix(near_singular, "y", c("a", "b")))
  near_singular[2, 3] <- near_singular[3, 2] <- 1
  expect_error(validate_rwa_matrix(near_singular, "y", c("a", "b")), "singular.*numerically")
})

test_that("small joint-matrix errors cannot produce materially impossible fits", {
  u <- c(-1, -1, 1, 1)
  v <- c(-1, 1, -1, 1)
  z <- c(-1, 1, 1, -1)
  rho <- 1 - 4e-8
  a <- sqrt(1e-7)
  d <- data.frame(
    y = c(z, a * u + sqrt(1 - a^2) * v, -a * u + sqrt(1 - a^2) * v),
    x1 = c(u, u, rep(NA_real_, 4)),
    x2 = c(rho * u + sqrt(1 - rho^2) * v, rep(NA_real_, 4), u)
  )
  joint <- cor(d, use = "pairwise.complete.obs")
  expect_no_error(validate_rwa_matrix(joint, "y", c("x1", "x2")))
  expect_error(rwa_multiregress(d, "y", c("x1", "x2")),
               "R-squared.*exceeds 1.*complete.obs")
  expect_error(rwa(d, "y", c("x1", "x2"), method = "multiple"),
               "R-squared.*exceeds 1.*complete.obs")
  expect_error(rwa_boot_statistic(d, seq_len(nrow(d)), "y", c("x1", "x2")),
               "bootstrap sample:.*R-squared.*exceeds 1")
})

test_that("estimable highly correlated predictors retain their valid fit", {
  u <- c(-1, -1, 1, 1)
  v <- c(-1, 1, -1, 1)
  z <- c(-1, 1, 1, -1)
  d <- data.frame(y = v + 0.5 * z, x1 = u, x2 = u + 1e-4 * v, w = 1)
  expected <- summary(lm(y ~ x1 + x2, data = d))$r.squared
  for (weight in list(NULL, "w")) {
    result <- rwa_multiregress(d, "y", c("x1", "x2"), weight = weight)
    wrapped <- rwa(d, "y", c("x1", "x2"), method = "multiple", weight = weight)
    expect_equal(result$rsquare, expected, tolerance = 1e-8)
    expect_equal(wrapped$rsquare, expected, tolerance = 1e-8)
    expect_equal(sum(result$result$Raw.RelWeight), expected, tolerance = 1e-8)
    expect_equal(rwa_boot_statistic(d, seq_len(nrow(d)), "y", c("x1", "x2"),
                                    weight_var = weight),
                 result$result$Raw.RelWeight, tolerance = 1e-12)
  }
})

test_that("additional predictors do not impose a new conditioning cutoff", {
  basis <- matrix(1, 1, 1)
  for (i in seq_len(6)) {
    basis <- rbind(cbind(basis, basis), cbind(basis, -basis))
  }
  x <- basis[, 2:21]
  x[, 2] <- x[, 1] + 1.1e-7 * x[, 2]
  d <- data.frame(y = basis[, 2] + 0.5 * basis[, 22], x)
  predictors <- paste0("x", seq_len(20))
  names(d)[-1] <- predictors
  expected <- summary(lm(y ~ ., data = d))$r.squared
  d$w <- 1
  for (weight in list(NULL, "w")) {
    for (selected in list(predictors[1:10], predictors)) {
      result <- rwa_multiregress(d, "y", selected, weight = weight)
      wrapped <- rwa(d, "y", selected, method = "multiple", weight = weight)
      expect_equal(result$rsquare, expected, tolerance = 1e-8)
      expect_equal(wrapped$rsquare, expected, tolerance = 1e-8)
      expect_equal(sum(result$result$Raw.RelWeight), expected, tolerance = 1e-8)
      expect_equal(rwa_boot_statistic(d, seq_len(nrow(d)), "y", selected,
                                      weight_var = weight),
                   result$result$Raw.RelWeight, tolerance = 1e-12)
      expect_equal(sum(result$result$Rescaled.RelWeight), 100)
    }
  }
})

test_that("constant columns and insufficient data fail with actionable diagnostics", {
  d <- mtcars
  d$constant <- 1
  d$w <- 1
  for (weight in list(NULL, "w")) {
    expect_error(rwa_multiregress(d, "mpg", c("hp", "constant"), weight = weight), "zero variance.*constant")
    expect_error(rwa_multiregress(d, "constant", "hp", weight = weight), "zero variance.*constant")
    expect_error(rwa_multiregress(d[1, ], "mpg", "hp", weight = weight), "Insufficient data.*two")
    expect_error(rwa_multiregress(d[FALSE, ], "mpg", "hp", weight = weight), "Insufficient data")
    for (use in c("complete.obs", "na.or.complete")) {
      missing <- d
      missing$hp <- NA_real_
      expect_error(rwa_multiregress(missing, "mpg", "hp", use = use, weight = weight), "Insufficient data")
    }
  }
  d$hp[1] <- Inf
  expect_error(rwa_multiregress(d, "mpg", "hp"), "finite values.*hp")
  d$hp <- NA_real_
  expect_error(rwa_multiregress(d, "mpg", "hp"), "zero variance or insufficient.*hp")
})

test_that("degenerate rare-binary resamples error rather than dropping and recycling", {
  set.seed(991)
  d <- data.frame(x1 = rnorm(80), x2 = c(1, rep(0, 79)), w = 1)
  d$y <- d$x1 + rnorm(80)
  bad_indices <- rep(2:80, length.out = 80)
  for (weight in list(NULL, "w")) {
    for (statistic in list(rwa_boot_statistic, rwa_boot_statistic_rescaled, rwa_boot_comprehensive)) {
      expect_error(statistic(d, bad_indices, "y", c("x1", "x2"), weight_var = weight),
                   "bootstrap sample:.*zero variance.*x2.*no samples are skipped or retried")
    }
    set.seed(22)
    expect_error(boot::boot(d, rwa_boot_statistic, R = 99, outcome = "y",
                            predictors = c("x1", "x2"), weight_var = weight),
                 "bootstrap sample:.*zero variance.*x2")
  }
})

test_that("undefined rescaled estimates are diagnosed rather than returned as NaN", {
  d <- data.frame(y = c(1, 1, -1, -1), x = c(1, -1, 1, -1))
  expect_error(rwa_multiregress(d, "y", "x"), "R-squared is zero.*undefined")
  expect_error(rwa_boot_statistic_rescaled(d, 1:4, "y", "x"),
               "bootstrap sample: R-squared is zero.*undefined")
})
