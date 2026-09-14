test_that("random comparisons never overwrite caller weights or predictors", {
  d <- mtcars
  d$rand <- rep(c(1, 2), 16)
  d$rand_ <- d$wt
  d$rand__ <- d$disp
  predictors <- c("hp", "rand_", "rand__")
  original <- d
  set.seed(71)
  actual <- rwa_rand_internal(d, "mpg", predictors, weight = "rand")
  set.seed(71)
  d$noise <- rnorm(nrow(d))
  augmented <- rwa_multiregress(d, "mpg", c(predictors, "noise"), weight = "rand")
  weights <- augmented$result$Raw.RelWeight
  expect_equal(actual, weights[1:3] - weights[4], tolerance = 1e-12)
  expect_identical(d[names(original)], original)
  set.seed(71)
  expect_equal(rwa_boot_comprehensive(original, seq_len(nrow(original)), "mpg",
                                      predictors, weight_var = "rand")[4:6], actual)
  set.seed(72)
  actual <- rwa_rand_internal(original, "mpg", c("rand", "hp"))
  set.seed(72)
  original$noise <- rnorm(nrow(original))
  weights <- rwa_multiregress(original, "mpg", c("rand", "hp", "noise"))$result$Raw.RelWeight
  expect_equal(actual, weights[1:2] - weights[3])
})

test_that("comprehensive statistic blocks have fixed original predictor order", {
  predictors <- c("wt", "hp", "disp")
  d <- mtcars
  d$rand <- rep(c(1, 2), 16)
  raw <- rwa_multiregress(d, "mpg", predictors, weight = "rand")$result$Raw.RelWeight
  set.seed(71)
  random <- rwa_rand_internal(d, "mpg", predictors, weight = "rand")
  set.seed(71)
  statistic <- rwa_boot_comprehensive(d, seq_len(nrow(d)), "mpg", predictors,
                                      focal = "hp", weight_var = "rand")
  expect_length(statistic, 8)
  expect_equal(statistic[1:3], raw)
  expect_equal(statistic[4:6], random)
  expect_equal(statistic[7:8], raw[c(1, 3)] - raw[2], tolerance = 1e-12)
})

test_that("comprehensive mode supports no focal and labels exact CI statistic blocks", {
  set.seed(28)
  d <- data.frame(y = rnorm(90), third = rnorm(90), first = rnorm(90), second = rnorm(90),
                  rand = runif(90, 1, 3))
  predictors <- c("third", "first", "second")
  for (focal in list(NULL, "first")) {
    set.seed(54)
    result <- suppressWarnings(rwa(d, "y", predictors, method = "multiple",
                                   bootstrap = TRUE, n_bootstrap = 39, comprehensive = TRUE,
                                   focal = focal, weight = "rand"))
    ci <- result$bootstrap$ci_results
    comp <- result$bootstrap$boot_object_comprehensive
    expect_equal(ncol(comp$t), if (is.null(focal)) 6 else 8)
    expect_identical(ci$raw_weights$variable, predictors)
    expect_identical(ci$random_comparison$variable, predictors)
    expect_identical(ci$random_comparison$weight_index, 1:3)
    expect_true(all(ci$random_comparison$ci_type == "rand_diff"))
    random <- suppressWarnings(extract_ci(comp, variable_names = predictors,
                                          ci_type = "rand_diff", indices = 4:6))
    expect_equal(ci$random_comparison, random)
    for (i in seq_along(predictors)) {
      index <- i + 3
      direct <- suppressWarnings(boot::boot.ci(comp, index = index,
                                              type = switch(random$ci_method[i],
                                                            bca = "bca", percentile = "perc", basic = "basic")))
      bounds <- switch(random$ci_method[i], bca = direct$bca[4:5],
                       percentile = direct$percent[4:5], basic = direct$basic[4:5])
      expect_equal(c(random$ci_lower[i], random$ci_upper[i]), bounds)
    }
    if (is.null(focal)) {
      expect_false("focal_comparison" %in% names(ci))
    } else {
      expect_identical(ci$focal_comparison$variable, c("third", "second"))
      expect_identical(ci$focal_comparison$weight_index, 1:2)
      expect_true(all(ci$focal_comparison$ci_type == "focal_diff"))
      expected <- suppressWarnings(extract_ci(comp, variable_names = c("third", "second"),
                                               ci_type = "focal_diff", indices = 7:8))
      expect_equal(ci$focal_comparison, expected)
      expect_equal(comp$t[, 7:8], comp$t[, c(1, 3)] - comp$t[, 2], tolerance = 1e-12)
    }
    expect_identical(result$result$Raw.RelWeight.CI.Lower,
                     ci$raw_weights$ci_lower[match(result$result$Variables, predictors)])
  }
})

test_that("invalid focal references fail clearly and single-focal models omit empty comparisons", {
  expect_error(run_rwa_bootstrap(mtcars, "mpg", c("hp", "wt"), comprehensive = TRUE,
                                 focal = "missing"), "focal.*requested predictors")
  set.seed(4)
  result <- suppressWarnings(run_rwa_bootstrap(mtcars, "mpg", "hp", n_bootstrap = 19,
                                               comprehensive = TRUE, focal = "hp"))
  expect_equal(ncol(result$boot_object_comprehensive$t), 2)
  expect_named(result$ci_results, c("raw_weights", "random_comparison"))
  expect_identical(result$ci_results$random_comparison$variable, "hp")
})
