test_that("remove_all_na_cols() works correctly", {
  # Test with data frame with no NA columns
  df_no_na <- data.frame(
    x = 1:5,
    y = 6:10,
    z = 11:15
  )
  
  result_no_na <- remove_all_na_cols(df_no_na)
  expect_equal(result_no_na, df_no_na)
  expect_equal(ncol(result_no_na), 3)
  
  # Test with data frame with some NA columns
  df_with_na <- data.frame(
    x = 1:5,
    y = rep(NA, 5),  # All NA column
    z = 11:15,
    w = rep(NA, 5)   # Another all NA column
  )
  
  result_with_na <- remove_all_na_cols(df_with_na)
  expect_equal(ncol(result_with_na), 2)
  expect_named(result_with_na, c("x", "z"))
  expect_equal(result_with_na$x, 1:5)
  expect_equal(result_with_na$z, 11:15)
  
  # Test with data frame with partial NA columns
  df_partial_na <- data.frame(
    x = c(1, 2, NA, 4, 5),  # Partial NA
    y = rep(NA, 5),         # All NA
    z = c(11, NA, 13, 14, 15)  # Partial NA
  )
  
  result_partial_na <- remove_all_na_cols(df_partial_na)
  expect_equal(ncol(result_partial_na), 2)
  expect_named(result_partial_na, c("x", "z"))
  
  # Test with empty data frame
  df_empty <- data.frame()
  result_empty <- remove_all_na_cols(df_empty)
  expect_equal(result_empty, df_empty)
  
  # Test with single column all NA
  df_single_na <- data.frame(x = rep(NA, 3))
  result_single_na <- remove_all_na_cols(df_single_na)
  expect_equal(ncol(result_single_na), 0)
  
  # Test with single column no NA
  df_single_no_na <- data.frame(x = 1:3)
  result_single_no_na <- remove_all_na_cols(df_single_no_na)
  expect_equal(result_single_no_na, df_single_no_na)
})

test_that("remove_all_na_cols() preserves data types", {
  # Test with different data types
  df_mixed <- data.frame(
    numeric_col = c(1.5, 2.5, 3.5),
    integer_col = 1:3,
    character_col = c("a", "b", "c"),
    factor_col = factor(c("x", "y", "z")),
    logical_col = c(TRUE, FALSE, TRUE),
    all_na_col = rep(NA, 3)
  )
  
  result_mixed <- remove_all_na_cols(df_mixed)
  
  # Should remove only the all_na_col
  expect_equal(ncol(result_mixed), 5)
  expect_false("all_na_col" %in% names(result_mixed))
  
  # Check data types are preserved
  expect_type(result_mixed$numeric_col, "double")
  expect_type(result_mixed$integer_col, "integer")
  expect_type(result_mixed$character_col, "character")
  expect_s3_class(result_mixed$factor_col, "factor")
  expect_type(result_mixed$logical_col, "logical")
})

test_that("remove_all_na_cols() handles different NA types", {
  # Test with different types of NA
  df_na_types <- data.frame(
    numeric_na = c(1, 2, NA_real_),
    integer_na = c(1L, 2L, NA_integer_),
    character_na = c("a", "b", NA_character_),
    all_numeric_na = rep(NA_real_, 3),
    all_integer_na = rep(NA_integer_, 3),
    all_character_na = rep(NA_character_, 3)
  )
  
  result_na_types <- remove_all_na_cols(df_na_types)
  
  # Should keep columns with partial NAs, remove columns with all NAs
  expect_equal(ncol(result_na_types), 3)
  expect_named(result_na_types, c("numeric_na", "integer_na", "character_na"))
})
