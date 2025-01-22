test_that("compare_columns handles basic dataframe comparison correctly", {
  df1 <- data.frame(a = 1:3, b = 2:4, c = 3:5)
  df2 <- data.frame(b = 2:4, c = 3:5, d = 4:6)
  
  result <- compare_columns(df1, df2, "DF1", "DF2")
  
  expect_equal(result$summary_data$unique_cols$DF1, "a")
  expect_equal(result$summary_data$unique_cols$DF2, "d")
  expect_equal(result$summary_data$mutual_cols, c("b", "c"))
  expect_equal(result$summary_data$total_cols$DF1, 3)
  expect_equal(result$summary_data$total_cols$DF2, 3)
})

test_that("compare_columns handles vector inputs correctly", {
  vec1 <- c("a", "b", "c")
  vec2 <- c("b", "c", "d")
  
  result <- compare_columns(vec1, vec2, "VEC1", "VEC2")
  
  expect_equal(result$summary_data$unique_cols$VEC1, "a")
  expect_equal(result$summary_data$unique_cols$VEC2, "d")
  expect_equal(result$summary_data$mutual_cols, c("b", "c"))
  expect_equal(result$summary_data$total_cols$VEC1, 3)
})

test_that("compare_columns validates input correctly", {
  df1 <- data.frame(a = 1:3)
  not_df <- list(a = 1:3)  # Neither data.frame nor vector
  null_input <- NULL
  
  expect_error(compare_columns(df1, not_df), "obj2 must be a data frame, tibble, or vector")
  expect_error(compare_columns(not_df, df1), "obj1 must be a data frame, tibble, or vector")
  expect_error(compare_columns(null_input, df1), "obj1 must be a data frame, tibble, or vector")
  expect_error(compare_columns(df1, df1, 1, "obj2"), "Object names must be character strings")
})

test_that("compare_columns handles empty dataframes", {
  df1 <- data.frame(a = numeric(0))
  df2 <- data.frame(b = numeric(0))
  
  result <- compare_columns(df1, df2)
  
  expect_equal(result$summary_data$row_counts$obj1, 0)
  expect_equal(result$summary_data$row_counts$obj2, 0)
  expect_equal(result$summary_data$unique_cols$obj1, "a")
})

test_that("compare_columns identifies duplicate rows correctly", {
  df1 <- data.frame(a = c(1,1,2), b = c(2,2,3))
  df2 <- data.frame(b = c(2,2,3), c = c(3,3,4))
  
  result <- compare_columns(df1, df2)
  
  expect_equal(result$summary_data$unique_rows$obj1, 2)
  expect_equal(result$summary_data$unique_rows$obj2, 2)
})

test_that("compare_columns handles mixed input types", {
  df1 <- data.frame(a = 1:3, b = 2:4)
  vec2 <- c("b", "c")
  
  result <- compare_columns(df1, vec2)
  
  expect_equal(result$summary_data$unique_cols$obj1, "a")
  expect_equal(result$summary_data$unique_cols$obj2, "c")
  expect_equal(result$summary_data$mutual_cols, "b")
})