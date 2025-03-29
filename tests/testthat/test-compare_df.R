library(testthat)
library(dplyr)

test_that("compare_df handles basic functionality", {
  # Create test data with a 'vs' column
  df1 <- data.frame(
    id = 1:10,
    value = rnorm(10),
    category = sample(LETTERS[1:3], 10, replace = TRUE),
    vs = sample(0:1, 10, replace = TRUE)
  )
  df2 <- df1
  df2$value <- df2$value * 1.1 # Introduce some differences

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
  # Create test data
  test_df <- data.frame(
    id = 1:5,
    value = rnorm(5)
  )

  expect_error(compare_df(old_data = test_df), "The 'group_col' parameter is required")
  expect_error(
    compare_df(old_data = test_df, group_col = NULL),
    "The 'group_col' parameter is required"
  )
})

test_that("compare_df handles group column correctly", {
  # Create test data with a 'vs' column
  test_df <- data.frame(
    id = 1:10,
    value = rnorm(10),
    vs = sample(0:1, 10, replace = TRUE)
  )

  result <- compare_df(old_data = test_df, new_data = test_df, group_col = "vs")
  expect_true(is.null(result$group_join) || is.data.frame(result$group_join))

  # Test with non-existent group column - warning is issued in the parse_function call
  # The parse_function warning is tested in test-parse_function.R
  result <- suppressWarnings(
    compare_df(old_data = test_df, new_data = test_df, group_col = "nonexistent")
  )
  # The result should have group_join as NULL
  expect_null(result$group_join)
})

test_that("compare_df output structure is correct", {
  # Create test data with a 'vs' column
  test_df <- data.frame(
    id = 1:10,
    value = rnorm(10),
    vs = sample(0:1, 10, replace = TRUE)
  )

  result <- compare_df(old_data = test_df, new_data = test_df, group_col = "vs")

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

test_that("compare_df handles multiple data types", {
  # Create test data with various data types and a 'vs' column
  test_df1 <- data.frame(
    id = 1:10,
    numeric_val = rnorm(10),
    character_val = sample(LETTERS, 10),
    logical_val = sample(c(TRUE, FALSE), 10, replace = TRUE),
    vs = sample(0:1, 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  test_df2 <- test_df1
  test_df2$numeric_val <- test_df2$numeric_val * 1.1 # Introduce differences

  # Test with two datasets
  compare_result <- compare_df(old_data = test_df1, new_data = test_df2, group_col = "vs")
  expect_type(compare_result, "list")
  expect_named(compare_result, c(
    "numeric_join", "factor_join", "char_join",
    "bin_join", "date_join", "group_join"
  ))
})
