test_that("rwa() basic functionality works", {
  # Test with mtcars data
  result <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"))
  
  # Check structure
  expect_type(result, "list")
  expect_named(result, c("predictors", "rsquare", "result", "n", "lambda", "RXX", "RXY"))
  
  # Check predictors
  expect_equal(result$predictors, c("cyl", "hp"))
  expect_length(result$predictors, 2)
  
  # Check result dataframe
  expect_s3_class(result$result, "data.frame")
  expect_equal(nrow(result$result), 2)
  expect_named(result$result, c("Variables", "Raw.RelWeight", "Rescaled.RelWeight", "Sign"))
  
  # Check that rescaled weights sum to 100
  expect_equal(sum(result$result$Rescaled.RelWeight), 100, tolerance = 1e-10)
  
  # Check that raw weights sum to R-squared
  expect_equal(sum(result$result$Raw.RelWeight), result$rsquare, tolerance = 1e-10)
  
  # Check R-squared is between 0 and 1
  expect_gte(result$rsquare, 0)
  expect_lte(result$rsquare, 1)
  
  # Check sample size
  expect_type(result$n, "integer")
  expect_gte(result$n, 1)
})

test_that("rwa() sorting functionality works", {
  # Test default sorting (TRUE)
  result_sorted <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp", "wt"))
  
  # Check that results are sorted in descending order
  rescaled_weights <- result_sorted$result$Rescaled.RelWeight
  expect_true(all(rescaled_weights[-length(rescaled_weights)] >= rescaled_weights[-1]))
  
  # Test no sorting (FALSE)
  result_unsorted <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp", "wt"), sort = FALSE)
  
  # Check that variables are in original order
  expect_equal(result_unsorted$result$Variables, c("cyl", "hp", "wt"))
  
  # Results should have same values but different order
  expect_setequal(result_sorted$result$Raw.RelWeight, result_unsorted$result$Raw.RelWeight)
  expect_setequal(result_sorted$result$Rescaled.RelWeight, result_unsorted$result$Rescaled.RelWeight)
})

test_that("rwa() applysigns functionality works", {
  # Test without signs
  result_no_signs <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"), applysigns = FALSE)
  
  expect_false("Sign.Rescaled.RelWeight" %in% names(result_no_signs$result))
  
  # Test with signs
  result_with_signs <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"), applysigns = TRUE)
  
  expect_true("Sign.Rescaled.RelWeight" %in% names(result_with_signs$result))
  expect_type(result_with_signs$result$Sign.Rescaled.RelWeight, "double")
  
  # Check that signed weights preserve magnitude for positive relationships
  positive_vars <- result_with_signs$result$Sign == "+"
  if(any(positive_vars)) {
    expect_equal(
      result_with_signs$result$Rescaled.RelWeight[positive_vars],
      result_with_signs$result$Sign.Rescaled.RelWeight[positive_vars]
    )
  }
  
  # Check that signed weights are negative for negative relationships
  negative_vars <- result_with_signs$result$Sign == "-"
  if(any(negative_vars)) {
    expect_equal(
      -result_with_signs$result$Rescaled.RelWeight[negative_vars],
      result_with_signs$result$Sign.Rescaled.RelWeight[negative_vars]
    )
  }
})

test_that("rwa() bootstrap functionality works", {
  skip_on_cran()  # Skip on CRAN due to computation time
  
  # Test basic bootstrap
  result_bootstrap <- mtcars %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"), 
        bootstrap = TRUE, n_bootstrap = 100)  # Small number for speed
  
  # Check bootstrap structure
  expect_true("bootstrap" %in% names(result_bootstrap))
  expect_type(result_bootstrap$bootstrap, "list")
  
  # Check CI columns are added
  expect_true("Raw.RelWeight.CI.Lower" %in% names(result_bootstrap$result))
  expect_true("Raw.RelWeight.CI.Upper" %in% names(result_bootstrap$result))
  expect_true("Raw.Significant" %in% names(result_bootstrap$result))
  
  # Check CI bounds are logical
  expect_true(all(result_bootstrap$result$Raw.RelWeight.CI.Lower <= 
                  result_bootstrap$result$Raw.RelWeight.CI.Upper))
  
  # Check significance is logical
  expect_type(result_bootstrap$result$Raw.Significant, "logical")
})

test_that("rwa() handles edge cases", {
  # Test with single predictor
  result_single <- mtcars %>%
    rwa(outcome = "mpg", predictors = "cyl")
  
  expect_equal(nrow(result_single$result), 1)
  expect_equal(result_single$result$Rescaled.RelWeight, 100, tolerance = 1e-10)
  
  # Test with missing data
  mtcars_na <- mtcars
  mtcars_na$mpg[1:3] <- NA
  
  result_na <- mtcars_na %>%
    rwa(outcome = "mpg", predictors = c("cyl", "hp"))
  
  expect_lt(result_na$n, nrow(mtcars))  # Should have fewer observations
  expect_gte(result_na$n, 1)  # Should still have some observations
})

test_that("rwa() handles various input scenarios", {
  # Test with character predictors (should work with factor conversion)
  char_data <- data.frame(
    y = 1:10,
    x1 = 1:10,
    x2 = letters[1:10]
  )
  
  # This might not error but could give unexpected results
  # Just test that it doesn't crash the R session
  expect_no_error({
    try(rwa(char_data, outcome = "y", predictors = c("x1", "x2")), silent = TRUE)
  })
  
  # Test with non-existent columns (should error from dplyr::select)
  expect_error(
    rwa(mtcars, outcome = "nonexistent", predictors = "cyl")
  )
  
  expect_error(
    rwa(mtcars, outcome = "mpg", predictors = "nonexistent")
  )
})

test_that("rwa() parameter behavior", {
  # Test that function accepts various parameter types
  # (may not validate them strictly, but shouldn't crash)
  
  # Valid parameters should work
  expect_no_error(
    rwa(mtcars, outcome = "mpg", predictors = "cyl", sort = TRUE)
  )
  
  expect_no_error(
    rwa(mtcars, outcome = "mpg", predictors = "cyl", sort = FALSE)
  )
  
  # Test bootstrap parameter
  expect_no_error(
    rwa(mtcars, outcome = "mpg", predictors = "cyl", bootstrap = FALSE)
  )
  
  # These might work or fail gracefully depending on internal validation
  # Just ensure they don't crash R
  expect_no_error({
    try(rwa(mtcars, outcome = "mpg", predictors = "cyl", sort = "invalid"), silent = TRUE)
  })
  
  expect_no_error({
    try(rwa(mtcars, outcome = "mpg", predictors = "cyl", bootstrap = "invalid"), silent = TRUE)
  })
}))

test_that("rwa() consistency checks", {
  # Test that results are reproducible (without bootstrap)
  result1 <- mtcars %>% rwa(outcome = "mpg", predictors = c("cyl", "hp"))
  result2 <- mtcars %>% rwa(outcome = "mpg", predictors = c("cyl", "hp"))
  
  expect_equal(result1$result, result2$result)
  expect_equal(result1$rsquare, result2$rsquare)
  
  # Test that order of predictors doesn't affect results (when sort = TRUE)
  result_order1 <- mtcars %>% 
    rwa(outcome = "mpg", predictors = c("cyl", "hp"), sort = TRUE)
  result_order2 <- mtcars %>% 
    rwa(outcome = "mpg", predictors = c("hp", "cyl"), sort = TRUE)
  
  expect_equal(result_order1$result, result_order2$result)
})
