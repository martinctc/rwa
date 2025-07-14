test_that("rwa() and plot_rwa() integration works", {
  # Test complete workflow
  result <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp", "wt"))
  
  # Should be able to pipe directly to plot
  expect_s3_class(
    plot_obj <- result %>% plot_rwa(),
    "ggplot"
  )
  
  # Test piped workflow
  expect_s3_class(
    mtcars %>%
      rwa(outcome = "mpg", predictors = c("cyl", "hp")) %>%
      plot_rwa(),
    "ggplot"
  )
})

test_that("package functions work with real data examples", {
  # Test with diamonds data (as in examples)
  skip_if_not_installed("ggplot2")
  
  library(ggplot2)
  
  # Basic diamonds example
  diamonds_result <- diamonds %>%
    dplyr::slice_sample(n = 1000) %>%  # Sample for speed
    rwa(outcome = "price", predictors = c("depth", "carat"))
  
  expect_s3_class(diamonds_result, "list")
  expect_true(all(c("predictors", "rsquare", "result", "n") %in% names(diamonds_result)))
  
  # Should be able to plot
  diamonds_plot <- plot_rwa(diamonds_result)
  expect_s3_class(diamonds_plot, "ggplot")
})

test_that("sorting behavior is consistent across functions", {
  # Test that sorting affects both result table and plot order
  result_sorted <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp", "wt"), sort = TRUE)
  
  result_unsorted <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp", "wt"), sort = FALSE)
  
  # Create plots
  plot_sorted <- plot_rwa(result_sorted)
  plot_unsorted <- plot_rwa(result_unsorted)
  
  # Both should create valid plots
  expect_s3_class(plot_sorted, "ggplot")
  expect_s3_class(plot_unsorted, "ggplot")
  
  # Data should have same variables but potentially different orders
  expect_setequal(plot_sorted$data$Variables, plot_unsorted$data$Variables)
  expect_setequal(plot_sorted$data$Rescaled.RelWeight, plot_unsorted$data$Rescaled.RelWeight)
})

test_that("bootstrap results work with plotting", {
  skip_on_cran()
  
  # Test bootstrap + plotting workflow
  bootstrap_result <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"), 
        bootstrap = TRUE, n_bootstrap = 50)
  
  # Should be able to plot bootstrap results
  bootstrap_plot <- plot_rwa(bootstrap_result)
  expect_s3_class(bootstrap_plot, "ggplot")
  
  # Plot data should have the extra bootstrap columns
  # but plot_rwa should still work (it doesn't use the CI columns)
  expect_true("Raw.RelWeight.CI.Lower" %in% names(bootstrap_result$result))
  expect_s3_class(bootstrap_plot, "ggplot")
})
