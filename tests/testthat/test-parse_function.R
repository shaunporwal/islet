library(testthat)
library(dplyr)
library(readr)

test_that("parse_function handles all column types correctly", {
  # Load test data
  trial <- read_csv("https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/trial.csv")
  
  # Convert date columns to proper Date type and character columns to factors where needed
  trial <- trial %>%
    mutate(
      visit_date = as.Date(visit_date),
      follow_up_date = as.Date(follow_up_date),
      stage = factor(stage),
      grade = factor(grade),
      insurance = factor(insurance),
      smoking_status = factor(smoking_status)
    )
  
  # Run function
  results <- parse_function(trial)
  
  # Test date columns
  expect_true("date_df" %in% names(results))
  expect_equal(nrow(results$date_df), 2)  # Should have visit_date and follow_up_date
  expect_true(all(c("min_date", "max_date", "perc_na_date") %in% colnames(results$date_df)))
  
  # Test binary columns
  expect_true("binary_df" %in% names(results))
  expect_true(all(c("has_side_effects", "enrolled_in_study") %in% 
                  results$binary_df$field))
  expect_true(all(c("ratio_binary", "perc_na_binary") %in% colnames(results$binary_df)))
  
  # Test character columns
  expect_true("char_df" %in% names(results))
  expect_true(all(c("patient_id", "hospital", "trt") %in% results$char_df$field))
  expect_true(all(c("values_char", "distinct_char", "perc_na_char") %in% colnames(results$char_df)))
  
  # Test factor columns
  expect_true("factor_df" %in% names(results))
  expect_true(all(c("stage", "grade", "insurance", "smoking_status") %in% results$factor_df$field))
  expect_true(all(c("levels_factor", "distinct_factor", "perc_na_factor") %in% colnames(results$factor_df)))
  
  # Test numeric columns
  expect_true("summary_numeric" %in% names(results))
  expect_true(all(c("age", "marker", "ttdeath", "bmi", "systolic_bp") %in% 
                  unique(gsub("_.*$", "", results$summary_numeric$field))))
})

test_that("parse_function handles grouping correctly", {
  trial <- read_csv("https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/trial.csv") %>%
    mutate(across(c(visit_date, follow_up_date), as.Date))
  
  # Test with single outcome
  results_single <- parse_function(trial, group_col = "trt", ind_outcomes = "age")
  expect_equal(nrow(results_single$group_df), 2)  # Should have two treatment groups
  expect_equal(ncol(results_single$group_df), 2)  # group column + age column
  
  # Test with multiple outcomes
  results_multiple <- parse_function(trial, group_col = "trt", 
                                   ind_outcomes = c("age", "bmi", "systolic_bp"))
  expect_equal(nrow(results_multiple$group_df), 2)
  expect_equal(ncol(results_multiple$group_df), 4)  # group column + 3 outcome columns
})

test_that("parse_function handles missing data correctly", {
  trial <- read_csv("https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/trial.csv") %>%
    mutate(across(c(visit_date, follow_up_date), as.Date))
  
  # Create some missing values
  trial_with_na <- trial %>%
    mutate(
      age = if_else(row_number() %% 5 == 0, NA_real_, age),
      visit_date = if_else(row_number() %% 7 == 0, as.Date(NA), visit_date),
      has_side_effects = if_else(row_number() %% 3 == 0, NA, has_side_effects)
    )
  
  results <- parse_function(trial_with_na)
  
  # Check NA percentages are calculated correctly
  expect_true(all(results$date_df$perc_na_date >= 0))
  expect_true(all(results$binary_df$perc_na_binary >= 0))
  expect_true(all(grepl("na_perc", results$summary_numeric$field) %>% 
                  {results$summary_numeric$summary_numeric[.]} >= 0))
})

test_that("parse_function handles empty/invalid inputs correctly", {
  # Test with empty dataframe
  empty_df <- data.frame(x = character(0))
  results_empty <- parse_function(empty_df)
  expect_type(results_empty, "list")
  
  # Test with invalid group column
  trial <- read_csv("https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/trial.csv")
  expect_error(
    parse_function(trial, group_col = "nonexistent_column", ind_outcomes = "age"),
    "The specified grouping column is missing in the dataset"
  )
  
  # Test with invalid outcome
  expect_error(
    parse_function(trial, group_col = "trt", ind_outcomes = "nonexistent_outcome"),
    "Some specified outcomes are missing in the dataset"
  )
})

test_that("parse_function handles subset of column types", {
  # Create dataset with only numeric columns
  numeric_only <- data.frame(
    x = 1:5,
    y = rnorm(5)
  )
  results_numeric <- parse_function(numeric_only)
  expect_true("summary_numeric" %in% names(results_numeric))
  expect_false("date_df" %in% names(results_numeric))
  expect_false("char_df" %in% names(results_numeric))
  
  # Create dataset with only character columns
  char_only <- data.frame(
    a = letters[1:5],
    b = LETTERS[1:5],
    stringsAsFactors = FALSE
  )
  results_char <- parse_function(char_only)
  expect_true("char_df" %in% names(results_char))
  expect_false("summary_numeric" %in% names(results_char))
  expect_false("date_df" %in% names(results_char))
})