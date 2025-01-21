# tests/testthat/test-compare_columns.R

test_that("compare_columns identifies differences correctly", {
  # Basic test with different columns
  df1 <- data.frame(a = 1, b = 2, c = 3)
  df2 <- data.frame(b = 2, c = 3, d = 4)
  result <- compare_columns(df1, df2)
  
  expect_equal(result$unique_to_df1, "a")
  expect_equal(result$unique_to_df2, "d")
  
  # Test with identical dataframes
  df3 <- data.frame(x = 1, y = 2)
  df4 <- data.frame(x = 3, y = 4)
  result2 <- compare_columns(df3, df4)
  
  expect_equal(length(result2$unique_to_df1), 0)
  expect_equal(length(result2$unique_to_df2), 0)
  
  # Test with completely different columns
  df5 <- data.frame(a = 1, b = 2)
  df6 <- data.frame(c = 3, d = 4)
  result3 <- compare_columns(df5, df6)
  
  expect_equal(result3$unique_to_df1, c("a", "b"))
  expect_equal(result3$unique_to_df2, c("c", "d"))
})

test_that("compare_columns handles custom names correctly", {
  df1 <- data.frame(a = 1, b = 2)
  df2 <- data.frame(b = 2, c = 3)
  
  result <- compare_columns(df1, df2, "first", "second")
  
  # Test custom naming
  expect_equal(names(result), c("unique_to_first", "unique_to_second"))
  expect_equal(result$unique_to_first, "a")
  expect_equal(result$unique_to_second, "c")
})

test_that("compare_columns handles empty dataframes", {
  df1 <- data.frame()
  df2 <- data.frame(a = 1)
  
  result1 <- compare_columns(df1, df2)
  expect_equal(length(result1$unique_to_df1), 0)
  expect_equal(result1$unique_to_df2, "a")
  
  result2 <- compare_columns(df2, df1)
  expect_equal(result2$unique_to_df1, "a")
  expect_equal(length(result2$unique_to_df2), 0)
})

test_that("compare_columns validates inputs correctly", {
  df1 <- data.frame(a = 1)
  not_df <- list(a = 1)
  
  # Test invalid dataframe inputs
  expect_error(compare_columns(not_df, df1), "Both inputs must be data frames or tibbles")
  expect_error(compare_columns(df1, not_df), "Both inputs must be data frames or tibbles")
  
  # Test invalid name inputs
  expect_error(compare_columns(df1, df1, 1, "second"), "Data frame names must be character strings")
  expect_error(compare_columns(df1, df1, "first", TRUE), "Data frame names must be character strings")
})

test_that("compare_columns works with tibbles", {
  skip_if_not_installed("tibble")
  library(tibble)
  
  tbl1 <- tibble(a = 1, b = 2)
  tbl2 <- tibble(b = 2, c = 3)
  
  result <- compare_columns(tbl1, tbl2)
  expect_equal(result$unique_to_df1, "a")
  expect_equal(result$unique_to_df2, "c")
})

test_that("compare_columns preserves column order", {
  df1 <- data.frame(c = 1, a = 2, b = 3)
  df2 <- data.frame(d = 1, b = 2, e = 3)
  
  result <- compare_columns(df1, df2)
  expect_equal(result$unique_to_df1, c("c", "a"))
  expect_equal(result$unique_to_df2, c("d", "e"))
})