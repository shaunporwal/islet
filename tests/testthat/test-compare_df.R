library(testthat)
library(dplyr)

test_that("compare_df handles basic functionality", {
  # Create simple test data
  df1 <- mtcars
  df2 <- mtcars
  df2$mpg <- df2$mpg * 1.1 # Introduce some differences

  # Test with single dataset
  single_result <- compare_df(old_data = df1, group_col = "vs")
  expect_type(single_result, "list")
  expect_named(single_result, c(
    "numeric_join", "factor_join", "char_join",
    "bin_join", "date_join", "group_join"
  ))

  # Test with two datasets
  compare_result <- compare_df(old_data = df1, new_data = df2, group_col = "vs")
  expect_type(compare_result, "list")
  expect_named(compare_result, c(
    "numeric_join", "factor_join", "char_join",
    "bin_join", "date_join", "group_join"
  ))
})

test_that("compare_df validates inputs correctly", {
  expect_error(compare_df(old_data = mtcars), "The 'group_col' parameter is required")
  expect_error(
    compare_df(old_data = mtcars, group_col = NULL),
    "The 'group_col' parameter is required"
  )
})

test_that("compare_df handles group column correctly", {
  result <- compare_df(old_data = mtcars, new_data = mtcars, group_col = "vs")
  expect_true(is.null(result$group_join) || is.data.frame(result$group_join))

  # Test with non-existent group column
  expect_warning(
    compare_df(old_data = mtcars, new_data = mtcars, group_col = "nonexistent"),
    "Group column nonexistent not found in data"
  )
})

test_that("compare_df output structure is correct", {
  result <- compare_df(old_data = mtcars, new_data = mtcars, group_col = "vs")

  expect_true(all(sapply(result, function(x) is.null(x) || is.data.frame(x))))

  if (!is.null(result$numeric_join)) {
    expect_true("field" %in% names(result$numeric_join))
  }

  if (!is.null(result$bin_join)) {
    expect_true(all(c("field", "ratio_binary.x", "ratio_binary.y") %in%
      names(result$bin_join)))
  }

  if (!is.null(result$char_join)) {
    expect_true(all(c("field", "values_char.x", "values_char.y") %in%
      names(result$char_join)))
  }

  if (!is.null(result$date_join)) {
    expect_true(all(c("field", "min_date.x", "min_date.y") %in%
      names(result$date_join)))
  }
})




test_that("compare_df handles basic functionality", {
  # Create simple test data
  df1 <- mtcars
  df2 <- mtcars
  df2$mpg <- df2$mpg * 1.1 # Introduce some differences

  group_col <- 'vs'


  


  # Test with two datasets
  compare_result <- compare_df(old_data = df1, new_data = df2, group_col = "vs")
  expect_type(compare_result, "list")
  expect_named(compare_result, c(
    "numeric_join", "factor_join", "char_join",
    "bin_join", "date_join", "group_join"
  ))
})