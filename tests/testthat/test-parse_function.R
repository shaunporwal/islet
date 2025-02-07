library(testthat)
library(dplyr)

test_that("parse_function handles all column types correctly", {
  # Load test data
  trial <- read_raw_data("https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/df_trial.csv")

  # Convert date columns to proper Date type and character columns to factors where needed
  trial <- trial %>%
    mutate(
      VISIT_DATE = as.Date(VISIT_DATE),
      FOLLOW_UP_DATE = as.Date(FOLLOW_UP_DATE),
      STAGE = factor(STAGE),
      GRADE = factor(GRADE),
      INSURANCE = factor(INSURANCE),
      SMOKING_STATUS = factor(SMOKING_STATUS)
    )

  # Run function
  results <- parse_function(trial)

  # Test date columns
  expect_true("date_df" %in% names(results))
  expect_true(all(c("min_date", "max_date", "perc_na_date") %in% colnames(results$date_df)))

  # Test binary columns
  expect_true("binary_df" %in% names(results))
  expect_true(all(c("HAS_SIDE_EFFECTS", "ENROLLED_IN_STUDY") %in%
    results$binary_df$field))
  expect_true(all(c("ratio_binary", "perc_na_binary") %in% colnames(results$binary_df)))

  # Test character columns
  expect_true("char_df" %in% names(results))
  expect_true(all(c("PATIENT_ID", "HOSPITAL", "TRT") %in% results$char_df$field))
  expect_true(all(c("values_char", "distinct_char", "perc_na_char") %in% colnames(results$char_df)))

  # Test factor columns
  expect_true("factor_df" %in% names(results))
  expect_true(all(c("STAGE", "GRADE", "INSURANCE", "SMOKING_STATUS") %in% results$factor_df$field))
  expect_true(all(c("levels_factor", "distinct_factor", "perc_na_factor") %in% colnames(results$factor_df)))

  # Test numeric columns
  expect_true("summary_numeric" %in% names(results))
  expect_true(all(c("AGE", "MARKER", "TTDEATH", "BMI", "SYSTOLIC") %in%
    unique(gsub("_.*$", "", results$summary_numeric$field))))
})

test_that("parse_function handles grouping correctly", {
  trial <- read_raw_data("https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/df_trial.csv")

  # Test with single outcome
  results_single <- parse_function(trial, group_col = "TRT", ind_outcomes = "AGE")
  expect_equal(nrow(results_single$group_df), 2) # Should have two treatment groups
  expect_equal(ncol(results_single$group_df), 2) # group column + age column

  # Test with multiple outcomes
  results_multiple <- parse_function(trial,
    group_col = "TRT",
    ind_outcomes = c("AGE", "BMI", "SYSTOLIC_BP")
  )
  expect_equal(nrow(results_multiple$group_df), 2)
  expect_equal(ncol(results_multiple$group_df), 4) # group column + 3 outcome columns
})

test_that("parse_function handles missing data correctly", {
  trial <- read_raw_data("https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/df_trial.csv")

  # Create some missing values
  trial_with_na <- trial %>%
    mutate(
      age = if_else(row_number() %% 5 == 0, NA_real_, AGE),
      visit_date = if_else(row_number() %% 7 == 0, as.Date(NA), VISIT_DATE),
      has_side_effects = if_else(row_number() %% 3 == 0, NA, HAS_SIDE_EFFECTS)
    )

  results <- parse_function(trial_with_na)

  # Check NA percentages are calculated correctly
  expect_true(all(results$date_df$perc_na_date >= 0))
  expect_true(all(results$binary_df$perc_na_binary >= 0))
  expect_true(all(grepl("na_perc", results$summary_numeric$field) %>%
    {
      results$summary_numeric$summary_numeric[.]
    } >= 0))
})

test_that("parse_function handles empty/invalid inputs correctly", {
  # Test with empty dataframe
  empty_df <- data.frame(x = character(0))
  results_empty <- parse_function(empty_df)
  expect_type(results_empty, "list")

  # Test with invalid group column
  trial <- read_raw_data("https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/df_trial.csv")
  expect_error(
    parse_function(trial, group_col = "nonexistent_column", ind_outcomes = "AGE"),
    "The specified grouping column is missing in the dataset"
  )

  # Test with invalid outcome
  expect_error(
    parse_function(trial, group_col = "TRT", ind_outcomes = "nonexistent_outcome"),
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
