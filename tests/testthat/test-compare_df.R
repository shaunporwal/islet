library(testthat)
library(dplyr)

# Define paths to datasets
trial1_path <- "https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/trial.csv"
trial2_path <- "https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/trial2.csv"

# Load datasets
trial1 <- read_raw_data(trial1_path)
trial2 <- read_raw_data(trial2_path)

# Unit tests for compare_df
test_that("compare_df function works as expected", {
  
  # Test: Compare only trial1 dataset (old_data only)
  old_only_result <- compare_df(
    old_data = trial1, 
    new_data = NULL, 
    group_col = "group"  # Explicit group column
  )
  
  expect_type(old_only_result, "list")
  expect_named(old_only_result, c("numeric_join", "factor_join", "char_join", "bin_join", "date_join", "group_join"))
  
  # Check if all returned components are either NULL or dataframes
  expect_true(all(sapply(old_only_result, function(x) is.null(x) || is.data.frame(x))))
  
  # Ensure numeric summaries are generated correctly
  if (!is.null(old_only_result$numeric_join)) {
    expect_true("field" %in% names(old_only_result$numeric_join))
    expect_true("statistic" %in% names(old_only_result$numeric_join))
    expect_true("value" %in% names(old_only_result$numeric_join))
  }
  
  # Test: Compare trial1 and trial2 datasets (old_data and new_data)
  compare_result <- compare_df(
    old_data = trial1, 
    new_data = trial2, 
    group_col = "group"  # Explicit group column
  )
  
  expect_type(compare_result, "list")
  expect_named(compare_result, c("numeric_join", "factor_join", "char_join", "bin_join", "date_join", "group_join"))
  
  # Check if all returned components are either NULL or dataframes
  expect_true(all(sapply(compare_result, function(x) is.null(x) || is.data.frame(x))))
  
  # Validate numeric_join
  if (!is.null(compare_result$numeric_join)) {
    expect_true(all(c("field", "statistic.x", "value.x", "statistic.y", "value.y") %in% names(compare_result$numeric_join)))
  }
  
  # Validate bin_join
  if (!is.null(compare_result$bin_join)) {
    expect_true(all(c("field", "ratio_binary.x", "perc_na_binary.x", "ratio_binary.y", "perc_na_binary.y") %in% names(compare_result$bin_join)))
  }
  
  # Validate char_join
  if (!is.null(compare_result$char_join)) {
    expect_true(all(c("field", "values_char.x", "distinct_char.x", "perc_na_char.x", "values_char.y", "distinct_char.y", "perc_na_char.y") %in% names(compare_result$char_join)))
  }
  
  # Factor types are seldom used
  # # Validate factor_join
  # if (!is.null(compare_result$factor_join)) {
  #   expect_true(all(c("field", "levels_factor.old", "levels_factor.new") %in% names(compare_result$factor_join)))
  # }
  
  # Validate date_join
  if (!is.null(compare_result$date_join)) {
    expect_true(all(c("field", "min_date.x", "max_date.x", "perc_na_date.x", "min_date.y", "max_date.y", "perc_na_date.y") %in% names(compare_result$date_join)))
  }
  
  # Test: Missing group_col parameter raises an error
  expect_error(
    compare_df(old_data = trial1, new_data = trial2),
    "The 'group_col' parameter is required and must be specified."
  )
})
