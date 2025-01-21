test_that("compare_columns handles basic comparison correctly", {
  df1 <- data.frame(a = 1:3, b = 2:4, c = 3:5)
  df2 <- data.frame(b = 2:4, c = 3:5, d = 4:6)
  
  result <- compare_columns(df1, df2, "DF1", "DF2")
  
  # Test core functionality
  expect_equal(result$summary_data$unique_cols$DF1, "a")
  expect_equal(result$summary_data$unique_cols$DF2, "d")
  expect_equal(result$summary_data$mutual_cols, c("b", "c"))
  expect_equal(result$summary_data$total_cols$DF1, 3)
  expect_equal(result$summary_data$total_cols$DF2, 3)
})

test_that("compare_columns validates input correctly", {
  df1 <- data.frame(a = 1:3)
  not_df <- list(a = 1:3)
  
  # Test input validation
  expect_error(compare_columns(df1, not_df), "Both inputs must be data frames")
  expect_error(compare_columns(df1, df1, 1, "df2"), "Data frame names must be character")
})

test_that("compare_columns handles empty dataframes", {
  df1 <- data.frame(a = numeric(0))
  df2 <- data.frame(b = numeric(0))
  
  result <- compare_columns(df1, df2)
  
  expect_equal(result$summary_data$row_counts$df1, 0)
  expect_equal(result$summary_data$row_counts$df2, 0)
  expect_equal(result$summary_data$unique_cols$df1, "a")
})

test_that("compare_columns identifies duplicate rows correctly", {
  df1 <- data.frame(a = c(1,1,2), b = c(2,2,3))
  df2 <- data.frame(b = c(2,2,3), c = c(3,3,4))
  
  result <- compare_columns(df1, df2)
  
  expect_equal(result$summary_data$unique_rows$df1, 2)
  expect_equal(result$summary_data$unique_rows$df2, 2)
})