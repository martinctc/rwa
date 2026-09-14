test_that("weighted single predictor and multiple predictors match weighted lm", {
  set.seed(4)
  d <- mtcars
  d$w <- runif(nrow(d), 0.5, 2)
  for (predictors in list("hp", c("wt", "hp", "disp"))) {
    expected <- summary(lm(reformulate(predictors, "mpg"), d, weights = w))$r.squared
    backend <- rwa_multiregress(d, "mpg", predictors, weight = "w")
    wrapper <- rwa(d, "mpg", predictors, method = "multiple", weight = "w", sort = FALSE)
    expect_equal(backend, wrapper)
    expect_equal(backend$rsquare, expected, tolerance = 1e-12)
    expect_equal(sum(backend$result$Raw.RelWeight), expected, tolerance = 1e-12)
    expect_identical(backend$result$Variables, predictors)
    expect_equal(rwa_boot_statistic(d, seq_len(nrow(d)), "mpg", predictors,
                                    weight_var = "w"), backend$result$Raw.RelWeight)
  }
})

test_that("integer replication and weight scaling preserve the weighted estimator", {
  d <- mtcars
  d$w <- rep(c(1, 2, 5, 3), 8)
  predictors <- c("hp", "wt", "disp")
  original <- rwa_multiregress(d, "mpg", predictors, weight = "w")
  replicated <- rwa_multiregress(d[rep(seq_len(nrow(d)), d$w), ], "mpg", predictors)
  expect_equal(original$result, replicated$result, tolerance = 1e-12)
  expect_equal(original$rsquare, replicated$rsquare, tolerance = 1e-12)
  for (scale in c(100, 1e200, 1e-200)) {
    scaled <- d
    scaled$w <- scaled$w * scale
    result <- rwa(df = scaled, outcome = "mpg", predictors = predictors,
                  method = "multiple", sort = FALSE, weight = "w")
    expect_equal(result$result, original$result, tolerance = 1e-12)
    expect_equal(result$n_effective, original$n_effective, tolerance = 1e-12)
    expect_equal(result$n_weighted / scale, original$n_weighted)
    expect_identical(result$n, original$n)
  }
})

test_that("weighted counts use retained original weights in both exported paths", {
  d <- mtcars
  d$w <- seq_len(nrow(d))
  d$mpg[1] <- NA
  d$w[2] <- NA
  d$hp[3] <- NA
  d$wt[4] <- NA
  predictors <- c("wt", "hp")
  retained <- complete.cases(d[c("mpg", predictors, "w")])
  expected_weights <- d$w[retained]
  for (fun in list(rwa_multiregress, function(...) rwa(..., method = "multiple", sort = FALSE))) {
    result <- fun(d, "mpg", predictors, weight = "w")
    expect_identical(result$n, sum(retained))
    expect_equal(result$n_weighted, sum(expected_weights))
    expect_equal(result$n_effective, sum(expected_weights)^2 / sum(expected_weights^2))
    filtered <- fun(d[retained, ], "mpg", predictors, weight = "w")
    expect_equal(result, filtered)
    unweighted <- fun(mtcars, "mpg", predictors)
    expect_named(unweighted, c("predictors", "rsquare", "result", "n", "lambda", "RXX", "RXY"))
  }
  d$w <- 7
  equal <- rwa_multiregress(d, "mpg", predictors, weight = "w")
  expect_equal(equal$n_effective, equal$n)
})

test_that("all calculation paths share finite positive numeric weight validation", {
  paths <- list(
    function(d) rwa(d, "mpg", c("hp", "wt"), method = "multiple", weight = "w"),
    function(d) rwa_multiregress(d, "mpg", c("hp", "wt"), weight = "w"),
    function(d) rwa_boot_statistic(d, seq_len(nrow(d)), "mpg", c("hp", "wt"), weight_var = "w"),
    function(d) rwa_boot_statistic_rescaled(d, seq_len(nrow(d)), "mpg", c("hp", "wt"), weight_var = "w"),
    function(d) rwa_boot_comprehensive(d, seq_len(nrow(d)), "mpg", c("hp", "wt"), weight_var = "w")
  )
  for (path in paths) {
    for (value in list(0, -1, Inf, -Inf, "bad")) {
      d <- mtcars
      d$w <- 1
      d$w[1] <- value
      expected <- if (is.character(value)) "numeric" else if (is.infinite(value)) "finite" else "positive"
      expect_error(path(d), paste0("Weight variable 'w'.*", expected))
    }
    expect_error(path(mtcars), "Weight variable 'w' not found")
  }
})

test_that("missing-data modes retain legacy outcome and weighted filtering", {
  d <- mtcars
  d$w <- seq_len(nrow(d))
  d$mpg[1] <- NA
  d$w[1] <- NA
  predictors <- c("hp", "wt")
  modes <- c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")
  for (fun in list(rwa_multiregress, function(...) rwa(..., method = "multiple", sort = FALSE))) {
    for (use in modes) {
      expect_equal(fun(d, "mpg", predictors, use = use),
                   fun(d[-1, ], "mpg", predictors, use = use))
      expect_equal(fun(d, "mpg", predictors, use = use, weight = "w"),
                   fun(d[-1, ], "mpg", predictors, use = use, weight = "w"))
    }
    d$hp[2] <- NA
    for (use in modes) {
      expected <- fun(d[complete.cases(d[c("mpg", predictors, "w")]), ],
                      "mpg", predictors, weight = "w")
      expect_equal(fun(d, "mpg", predictors, weight = "w", use = use), expected)
    }
    expect_error(fun(d, "mpg", predictors, use = "all.obs"), "missing.*all.obs")
    expect_error(fun(d, "mpg", predictors, use = "everything"), "non-finite.*hp")
    expect_equal(fun(d, "mpg", predictors, use = "na.or.complete"),
                 fun(d, "mpg", predictors, use = "complete.obs"))
    d$w[2] <- NA
    expect_error(fun(d, "mpg", predictors, weight = "w", use = "all.obs"),
                 "Weight variable contains NA")
    for (use in setdiff(modes, "all.obs")) {
      expect_equal(fun(d, "mpg", predictors, weight = "w", use = use),
                   fun(d[-c(1, 2), ], "mpg", predictors, weight = "w"))
    }
    d$hp[2] <- mtcars$hp[2]
    d$w[2] <- 2
  }
})

test_that("pairwise default remains distinct from complete-case analysis", {
  d <- mtcars
  d$hp[1:2] <- NA
  d$wt[3:4] <- NA
  result <- rwa_multiregress(d, "mpg", c("wt", "hp"))
  joint <- cor(d[c("mpg", "wt", "hp")], use = "pairwise.complete.obs")
  expect_equal(result$RXX, joint[-1, -1])
  expect_equal(result$RXY, joint[-1, 1])
  expect_equal(result$n, 28)
  complete <- rwa_multiregress(d, "mpg", c("wt", "hp"), use = "complete.obs")
  expect_false(isTRUE(all.equal(result$rsquare, complete$rsquare)))
})

test_that("point and original-index bootstrap agree without changing the sampling frame", {
  set.seed(91)
  d <- data.frame(y = rnorm(100), b = rnorm(100), a = rnorm(100), w = runif(100, 1, 3))
  d$y[1] <- NA
  d$w[2:4] <- NA
  d$a[5:8] <- NA
  predictors <- c("b", "a")
  for (weight in list(NULL, "w")) {
    for (use in c("pairwise.complete.obs", "complete.obs", "na.or.complete")) {
      point <- rwa_multiregress(d, "y", predictors, use = use, weight = weight)
      expect_equal(rwa_boot_statistic(d, seq_len(nrow(d)), "y", predictors,
                                      use = use, weight_var = weight), point$result$Raw.RelWeight)
      expect_equal(rwa_boot_statistic_rescaled(d, seq_len(nrow(d)), "y", predictors,
                                               use = use, weight_var = weight), point$result$Rescaled.RelWeight)
    }
  }
  set.seed(23)
  result <- suppressWarnings(run_rwa_bootstrap(d, "y", predictors, n_bootstrap = 20, weight = "w"))
  frame <- result$boot_object$data
  expect_equal(nrow(frame), 99)
  expect_true(anyNA(frame$w))
  expect_true(anyNA(frame$a))
  point <- rwa_multiregress(d, "y", predictors, weight = "w")
  expect_equal(result$boot_object$t0, point$result$Raw.RelWeight)
  indices <- boot::boot.array(result$boot_object, indices = TRUE)
  for (i in c(1, 10, 20)) {
    expect_equal(result$boot_object$t[i, ],
                 rwa_multiregress(frame[indices[i, ], ], "y", predictors, weight = "w")$result$Raw.RelWeight)
  }
})

test_that("small-sample warning counts eligible rows without prefiltering bootstrap data", {
  set.seed(14)
  d <- data.frame(y = rnorm(80), x = rnorm(80), w = 1)
  d$w[1:40] <- NA
  suppressWarnings(expect_warning(
    result <- suppressMessages(run_rwa_bootstrap(d, "y", "x", n_bootstrap = 2, weight = "w")),
    "40 complete eligible observations"
  ))
  expect_equal(nrow(result$boot_object$data), 80)
})
