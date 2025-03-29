library(testthat)
library(fs)

test_that("here_data correctly retrieves data date from file", {
  # Create a temporary file with a test date
  temp_dir <- tempdir()
  date_file_path <- file.path(temp_dir, "data_date.txt")
  test_date <- "2023-01-15"
  writeLines(test_date, date_file_path)

  # Test direct file path with return_date_only = TRUE
  expect_equal(here_data(path_to_data_date = date_file_path, return_date_only = TRUE), test_date)

  # Test directory path with data_date.txt file
  expect_equal(here_data(path_to_data_date = temp_dir, return_date_only = TRUE), test_date)

  # Test with multi-line file (should return only first line)
  multi_line_path <- file.path(temp_dir, "multi_line.txt")
  writeLines(c(test_date, "other content"), multi_line_path)
  # First line should be returned
  expect_equal(here_data(path_to_data_date = multi_line_path, return_date_only = TRUE), test_date)

  # Clean up
  file.remove(date_file_path, multi_line_path)
})

test_that("here_data warns for non-standard date formats", {
  # Create a temporary file with non-standard date format
  temp_dir <- tempdir()
  date_file_path <- file.path(temp_dir, "data_date.txt")

  # Test with non-standard format
  writeLines("not-a-date", date_file_path)
  expect_warning(
    here_data(path_to_data_date = date_file_path, return_date_only = TRUE),
    "doesn't appear to be in a standard date format"
  )

  # Test with standard formats (should not warn)
  writeLines("2023-01-15", date_file_path)
  expect_silent(here_data(path_to_data_date = date_file_path, return_date_only = TRUE))

  writeLines("20230115", date_file_path)
  expect_silent(here_data(path_to_data_date = date_file_path, return_date_only = TRUE))

  # Clean up
  file.remove(date_file_path)
})

test_that("here_data handles errors correctly", {
  # Test with non-existent file
  nonexistent_file <- file.path(tempdir(), "nonexistent.txt")

  # We're expecting an error here about the file not existing
  expect_error(
    here_data(path_to_data_date = nonexistent_file, return_date_only = TRUE),
    "does not exist|importing data date"
  )

  # Test with empty file
  empty_file <- file.path(tempdir(), "empty.txt")
  writeLines("", empty_file)
  expect_error(
    here_data(path_to_data_date = empty_file, return_date_only = TRUE),
    "The data date file is empty"
  )

  # Clean up
  if (file.exists(empty_file)) file.remove(empty_file)
})

test_that("here_data searches for alternative filenames", {
  # Create a temporary directory
  temp_dir <- tempdir()

  # Test with dataDate file instead of data_date.txt
  alternate_file <- file.path(temp_dir, "dataDate")
  test_date <- "2023-02-20"
  writeLines(test_date, alternate_file)

  expect_equal(here_data(path_to_data_date = temp_dir, return_date_only = TRUE), test_date)

  # Test error when no valid files found
  file.remove(alternate_file)
  expect_error(
    here_data(path_to_data_date = temp_dir, return_date_only = TRUE),
    "No text file containing the data date could be found"
  )

  # Clean up
  if (file.exists(alternate_file)) file.remove(alternate_file)
})

# Skip the 'here' style path test if mockery is not available
if (requireNamespace("mockery", quietly = TRUE)) {
  test_that("here_data builds correct paths with 'here' style", {
    # Create a temporary file with a test date
    temp_dir <- tempdir()
    date_file_path <- file.path(temp_dir, "data_date.txt")
    test_date <- "2023-03-30"
    writeLines(test_date, date_file_path)

    # Mock here::here to return a predictable path
    mockery::stub(here_data, "here::here", function(...) {
      if (length(list(...)) == 0) {
        return(temp_dir)
      }
      file.path(temp_dir, ...)
    })

    # Test basic path with default style='here'
    expected_path <- file.path(temp_dir, "secure_data", test_date)
    actual_path <- here_data(path_to_data_date = date_file_path)
    expect_equal(as.character(actual_path), expected_path)

    # Test with additional path components
    expected_path_with_file <- file.path(temp_dir, "secure_data", test_date, "data.csv")
    actual_path <- here_data("data.csv", path_to_data_date = date_file_path)
    expect_equal(as.character(actual_path), expected_path_with_file)

    # Test with custom data folder name
    expected_custom_path <- file.path(temp_dir, "custom_data", test_date)
    actual_path <- here_data(data_folder_name = "custom_data", path_to_data_date = date_file_path)
    expect_equal(as.character(actual_path), expected_custom_path)

    # Clean up
    file.remove(date_file_path)
  })
} else {
  message("Skipping 'here' style path tests since 'mockery' package is not available")
}

test_that("here_data builds correct paths with 'path' style", {
  # Create a temporary file with a test date
  temp_dir <- tempdir()
  date_file_path <- file.path(temp_dir, "data_date.txt")
  test_date <- "2023-04-10"
  writeLines(test_date, date_file_path)

  # Test with explicit path and style='path'
  test_path <- "H:/project"
  expected_path <- file.path(test_path, "secure_data", test_date)
  result <- here_data(style = "path", path = test_path, path_to_data_date = date_file_path)
  expect_equal(as.character(result), expected_path)

  # Test with NULL path (should error)
  expect_error(
    here_data(style = "path", path = NULL, path_to_data_date = date_file_path),
    "path argument is NULL"
  )

  # Test with options
  withr::with_options(
    list(path_data = test_path),
    {
      result <- here_data(style = "path", path_to_data_date = date_file_path)
      expect_equal(as.character(result), expected_path)
    }
  )

  # Test with additional path components
  expected_path_with_file <- file.path(test_path, "secure_data", test_date, "data.xlsx")
  result <- here_data("data.xlsx", style = "path", path = test_path, path_to_data_date = date_file_path)
  expect_equal(as.character(result), expected_path_with_file)

  # Clean up
  file.remove(date_file_path)
})
