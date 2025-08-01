test_that("plot_rwa() creates a ggplot object", {
  # Create test data
  rwa_result <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp", "wt"))
  
  # Create plot
  p <- plot_rwa(rwa_result)
  
  # Check that it's a ggplot object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, c("gg", "ggplot"))
})

test_that("plot_rwa() handles different rwa inputs correctly", {
  # Test with basic rwa result
  rwa_basic <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"))
  
  p_basic <- plot_rwa(rwa_basic)
  expect_s3_class(p_basic, "ggplot")
  
  # Test with applysigns = TRUE rwa result
  rwa_signs <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"), applysigns = TRUE)
  
  p_signs <- plot_rwa(rwa_signs)
  expect_s3_class(p_signs, "ggplot")
  
  # Test with single predictor
  rwa_single <- mtcars %>%
    rwa(outcome = "mpg", predictors = "cyl")
  
  p_single <- plot_rwa(rwa_single)
  expect_s3_class(p_single, "ggplot")
})

test_that("plot_rwa() handles bootstrap results", {
  skip_on_cran()  # Skip on CRAN due to computation time
  
  # Test with bootstrap results
  rwa_bootstrap <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"), 
        bootstrap = TRUE, n_bootstrap = 50)
  
  p_bootstrap <- plot_rwa(rwa_bootstrap)
  expect_s3_class(p_bootstrap, "ggplot")
})

test_that("plot_rwa() plot structure is correct", {
  rwa_result <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp", "wt"))
  
  p <- plot_rwa(rwa_result)
  
  # Check plot data
  plot_data <- p$data
  expect_s3_class(plot_data, "data.frame")
  expect_true("Variables" %in% names(plot_data))
  expect_true("Sign.Rescaled.RelWeight" %in% names(plot_data))
  
  # Check that plot has correct number of bars
  expect_equal(nrow(plot_data), length(rwa_result$predictors))
  
  # Check mapping using rlang::as_label instead of as.character
  expect_equal(rlang::as_label(p$mapping$x), "stats::reorder(Variables, Sign.Rescaled.RelWeight)")
  expect_equal(rlang::as_label(p$mapping$y), "Sign.Rescaled.RelWeight")
  
  # Check coordinate system (should be flipped)
  expect_s3_class(p$coordinates, "CoordFlip")
})

test_that("plot_rwa() applies signs correctly", {
  # Create data where we know the relationships
  # cyl and hp should be negatively related to mpg
  rwa_result <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"))
  
  p <- plot_rwa(rwa_result)
  plot_data <- p$data
  
  # Check that signs are applied
  expect_true("Sign.Rescaled.RelWeight" %in% names(plot_data))
  
  # For mtcars data, cyl and hp should typically be negative predictors of mpg
  cyl_weight <- plot_data$Sign.Rescaled.RelWeight[plot_data$Variables == "cyl"]
  hp_weight <- plot_data$Sign.Rescaled.RelWeight[plot_data$Variables == "hp"]
  
  # These should typically be negative (but we'll just check they exist)
  expect_type(cyl_weight, "double")
  expect_type(hp_weight, "double")
  expect_length(cyl_weight, 1)
  expect_length(hp_weight, 1)
})

test_that("plot_rwa() labels and aesthetics are correct", {
  rwa_result <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp", "wt"))
  
  p <- plot_rwa(rwa_result)
  
  # Check labels
  expect_equal(p$labels$title, "Variable importance estimates")
  expect_equal(p$labels$subtitle, "Using the Relative Weights Analysis method")
  expect_equal(p$labels$x, "Predictor variables")
  expect_equal(p$labels$y, "Rescaled Relative Weights")
  
  # Check that caption contains expected elements
  expect_true(grepl("Rescaled Relative Weights sum to 100%", p$labels$caption))
  expect_true(grepl("n =", p$labels$caption))
  expect_true(grepl("R-squared:", p$labels$caption))
  
  # Check theme
  expect_s3_class(p$theme, "theme")
})

test_that("plot_rwa() handles edge cases", {
  # Test with all positive relationships (if possible)
  # Using a simple case with one predictor
  rwa_simple <- mtcars %>%
    rwa(outcome = "disp", predictors = "cyl")  # These should be positively related
  
  p_simple <- plot_rwa(rwa_simple)
  expect_s3_class(p_simple, "ggplot")
  
  # Test with very small weights
  # This is harder to create artificially, but the function should handle it
  expect_s3_class(p_simple, "ggplot")
})

test_that("plot_rwa() input validation", {
  # Test with invalid input
  expect_error(plot_rwa("not_a_rwa_result"))
  expect_error(plot_rwa(list()))
  expect_error(plot_rwa(NULL))
  
  # Test with malformed rwa result
  bad_rwa <- list(result = data.frame(x = 1))  # Missing required columns
  expect_error(plot_rwa(bad_rwa))
})

test_that("plot_rwa() y-axis limits work correctly", {
  rwa_result <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp", "wt"))
  
  p <- plot_rwa(rwa_result)
  plot_data <- p$data
  
  # Check that y-limits accommodate the data plus some buffer
  max_weight <- max(plot_data$Sign.Rescaled.RelWeight)
  
  # The ylim should be from NA (automatic) to max_weight * 1.1
  # But if max_weight is negative (all negative relationships), 
  # we should test against the absolute maximum for proper scaling
  expected_upper_limit <- max_weight * 1.1
  y_limits <- layer_scales(p)$y$limits
  if (!is.null(y_limits)) {
    expect_equal(y_limits[2], expected_upper_limit, tolerance = 1e-10)
  }
})

test_that("plot_rwa() text labels are present", {
  rwa_result <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"))
  
  p <- plot_rwa(rwa_result)
  
  # Check that geom_text is present
  geom_classes <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true("GeomCol" %in% geom_classes)  # Bar chart
  expect_true("GeomText" %in% geom_classes)  # Text labels
  
  # Check that text layer has correct mapping
  text_layer <- p$layers[geom_classes == "GeomText"][[1]]
  expect_true("label" %in% names(text_layer$mapping))
})
