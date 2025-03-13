library(testthat)

test_that("base_dataset_metrics returns a data frame with expected columns", {
  df1 <- data.frame(MRN = c(1, 2, 3, 4))
  df2 <- data.frame(MRN = c(3, 4, 5, 6))
  df3 <- data.frame(MRN = c(4, 7, 8))
  named_list_dfs <- list(df1 = df1, df2 = df2, df3 = df3)

  result <- base_dataset_metrics(named_list_dfs, "MRN", "numeric")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("DataFrame", "Unique_Count", "Total_Common", "Is_Main") %in% colnames(result)))
})

test_that("base_dataset_metrics computes metrics correctly", {
  df1 <- data.frame(MRN = c(1, 2, 3, 4))
  df2 <- data.frame(MRN = c(3, 4, 5, 6))
  df3 <- data.frame(MRN = c(4, 7, 8))
  named_list_dfs <- list(df1 = df1, df2 = df2, df3 = df3)

  result <- base_dataset_metrics(named_list_dfs, "MRN", "numeric")

  # Expected unique counts: df1 = 4, df2 = 4, df3 = 3
  expect_equal(result$Unique_Count, c(4, 4, 3))

  # Expected total common:
  # df1: intersect(df1, df2) = {3, 4} (2) + intersect(df1, df3) = {4} (1) = 3
  # df2: intersect(df2, df1) = {3, 4} (2) + intersect(df2, df3) = {4} (1) = 3
  # df3: intersect(df3, df1) = {4} (1) + intersect(df3, df2) = {4} (1) = 2
  expect_equal(result$Total_Common, c(3, 3, 2))

  # Expected main flag: TRUE if Total_Common equals the maximum (3)
  expect_equal(result$Is_Main, c(TRUE, TRUE, FALSE))
})

test_that("base_dataset_metrics works with a single dataframe", {
  df1 <- data.frame(MRN = c(1, 2, 3))
  named_list_dfs <- list(df1 = df1)

  result <- base_dataset_metrics(named_list_dfs, "MRN", "numeric")

  expect_equal(result$Unique_Count, 3)
  # With one dataframe, there are no pairwise comparisons; rowSums returns 0.
  expect_equal(result$Total_Common, 0)
  expect_equal(result$Is_Main, TRUE)
})
