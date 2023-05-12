library(testthat)

# Define a temporary data_path
temp_data_path <- tempdir()

test_that("filter_cases correctly filters data", {
  # Let's create a copy of the trial data
  trial_copy <- trial

  # Create necessary directories for saving the RDS file
  dir.create(file.path(temp_data_path, "Metadata", "Filtered"), recursive = TRUE)

  # Test 1: Filter where age is greater than or equal to 18
  filtered_data <- filter_cases(data_df = trial_copy,
                                select = "age",
                                logic = "age >= 18",
                                data_path = temp_data_path,
                                rds = "cases_removed")
  # Check that all ages in the filtered data are greater than or equal to 18
  expect_true(all(filtered_data$age >= 18, na.rm = TRUE))

  # Test 2: Check that the function correctly handles NA values
  trial_copy$age[1] <- NA
  filtered_data <- filter_cases(data_df = trial_copy,
                                select = "age",
                                logic = "age >= 18",
                                data_path = temp_data_path,
                                rds = "cases_removed")
  # Check that the row with NA age has been removed
  expect_false(any(is.na(filtered_data$age)))

  # Test 3: Check that the function correctly groups by variables
  trial_copy <- trial
  filtered_data <- filter_cases(data_df = trial_copy,
                                select = "age",
                                logic = "age >= 18",
                                group_by_vars = "trt",
                                data_path = temp_data_path,
                                rds = "cases_removed")
  # Check that the output is grouped by the treatment variable
  expect_equal(group_vars(filtered_data), "trt")
})

test_that("filter_cases correctly handles an empty data frame", {
  data_df <- tibble(age = numeric())
  filtered_data <- filter_cases(data_df, select = "age", logic = "age >= 18")
  expect_equal(nrow(filtered_data), 0)
})

test_that("filter_cases correctly handles an invalid 'select' variable", {
  data_df <- tibble(age = 1:10)
  expect_error(filter_cases(data_df, select = "invalid_var", logic = "invalid_var >= 5"))
})

test_that("filter_cases correctly handles a non-logical 'remove' argument", {
  data_df <- tibble(age = 1:10)
  filtered_data <- filter_cases(data_df, select = "age", logic = "age >= 5", remove = "not_logical")
  expect_equal(nrow(filtered_data), nrow(data_df))
})

test_that("filter_cases correctly groups by multiple variables", {
  data_df <- tibble(age = c(1:5, NA, NA), group1 = c(rep("A", 3), rep("B", 4)), group2 = c(rep("X", 4), rep("Y", 3)))
  filtered_data <- filter_cases(data_df, select = "age", logic = "age >= 3", group_by_vars = c("group1", "group2"))
  expect_equal(length(group_vars(filtered_data)), 2)
  expect_equal(setequal(group_vars(filtered_data), c("group1", "group2")), TRUE)
})



