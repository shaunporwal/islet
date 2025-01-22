library(testthat)
library(dplyr)

test_that("compare_clean_data handles basic comparisons", {
  dir.create("test_output", showWarnings = FALSE)
  
  old_data <- data.frame(
    visit_date = as.Date(c("2023-01-01", "2023-01-02")),
    value = c(1, 2)
  )
  new_data <- old_data
  new_data$value <- c(1.1, 2.1)
  
  result <- suppressWarnings(compare_clean_data(
    old_data, new_data, "test_output",
    c("visit_date", "value"), "visit_date"
  ))
  
  expect_type(result, "list")
  unlink("test_output", recursive = TRUE)
})

test_that("compare_clean_data validates date column", {
  dir.create("test_output", showWarnings = FALSE)
  old_df <- data.frame(x = 1, y = 2)
  new_df <- data.frame(x = 1, y = 2)
  
  expect_error(
    compare_clean_data(
      old_df, new_df, "test_output",
      c("x", "y"), "missing_date"
    ),
    "Date column.*not found"
  )
  
  unlink("test_output", recursive = TRUE)
})

test_that("compare_clean_data handles date filtering", {
  dir.create("test_output", showWarnings = FALSE)
  
  old_data <- data.frame(
    visit_date = as.Date(c("2023-01-01", "2023-01-02")),
    value = c(1, 2)
  )
  new_data <- data.frame(
    visit_date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    value = c(1.1, 2.1, 3.1)
  )
  
  result <- suppressWarnings(compare_clean_data(
    old_data, new_data, "test_output",
    c("visit_date", "value"), "visit_date",
    limit_to_same_date = TRUE
  ))
  
  expect_type(result, "list")
  unlink("test_output", recursive = TRUE)
})