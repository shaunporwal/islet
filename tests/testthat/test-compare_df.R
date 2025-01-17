library(testthat)
library(dplyr)

# Define paths to datasets
trial1_path <- "https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/trial.csv"
trial2_path <- "https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/trial2.csv"

# Load datasets
trial1 <- read.csv(trial1_path, stringsAsFactors = FALSE)
trial2 <- read.csv(trial2_path, stringsAsFactors = FALSE)

# Unit tests for compare_df
test_that("compare_df function works as expected", {
  
  # Test: Compare only trial1 dataset
  old_only_result <- compare_df(old_data = trial1, new_data = NULL)
  
  expect_type(old_only_result, "list")
  expect_named(old_only_result, c("summary_numeric", "factor_df", "char_df", "binary_df", "date_df", "group_df"))
  
  # Check if all returned dataframes have expected structure
  expect_true(all(sapply(old_only_result, is.data.frame)))
  
  # Ensure numeric summaries are generated correctly
  if (!is.null(old_only_result$summary_numeric)) {
    expect_true("field" %in% names(old_only_result$summary_numeric))
    expect_true("statistic" %in% names(old_only_result$summary_numeric))
    expect_true("value" %in% names(old_only_result$summary_numeric))
  }
  
  # Test: Compare trial1 and trial2 datasets
  compare_result <- compare_df(old_data = trial1, new_data = trial2)
  
  expect_type(compare_result, "list")
  expect_named(compare_result, c("numeric_join", "factor_join", "char_join", "bin_join", "date_join", "group_join"))
  
  # Check if merged datasets are dataframes
  expect_true(all(sapply(compare_result, is.data.frame)))
  
  # Ensure numeric_join contains old and new suffixes
  if (!is.null(compare_result$numeric_join)) {
    expect_true(all(c("field", "mean.old", "mean.new") %in% names(compare_result$numeric_join)))
  }
  
  # Ensure binary_join contains old and new suffixes
  if (!is.null(compare_result$bin_join)) {
    expect_true(all(c("field", "ratio_binary.old", "ratio_binary.new") %in% names(compare_result$bin_join)))
  }
  
  # Ensure character columns were joined properly
  if (!is.null(compare_result$char_join)) {
    expect_true(all(c("field", "values_char.old", "values_char.new") %in% names(compare_result$char_join)))
  }
  
  # Ensure factor columns were joined properly
  if (!is.null(compare_result$factor_join)) {
    expect_true(all(c("field", "levels_factor.old", "levels_factor.new") %in% names(compare_result$factor_join)))
  }
  
  # Ensure date fields were joined properly
  if (!is.null(compare_result$date_join)) {
    expect_true(all(c("field", "min_date.old", "min_date.new") %in% names(compare_result$date_join)))
  }
  
  # Test: Ensure no unexpected columns in group-specific summaries
  if (!is.null(compare_result$group_join)) {
    group_cols <- c("group", "outcome1.old", "outcome1.new")
    expect_true(all(group_cols %in% names(compare_result$group_join)))
  }
})
