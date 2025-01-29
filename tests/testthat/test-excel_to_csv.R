# tests/testthat/test-excel_to_csv.R

library(testthat)
library(readxl)

withr::with_file(file.path(tempdir(), c("test.xlsx", "test.csv")), {
  test_that("excel_to_csv converts basic Excel file correctly", {
    # Create temp directory and file paths
    temp_dir <- tempdir()
    sample_data <- system.file("extdata", "datasets.xlsx", package = "readxl")
    file.copy(sample_data, file.path(temp_dir, "test.xlsx"))

    # Run conversion
    result <- excel_to_csv(temp_dir, "test.xlsx")

    # Check if CSV was created
    expect_true(file.exists(result))

    # Read and verify CSV contents matches original
    csv_data <- read.csv(result)
    excel_data <- read_xlsx(sample_data)
    expect_equal(csv_data, as.data.frame(excel_data))
  })

  test_that("excel_to_csv handles different sheets", {
    # Setup using readxl example file
    temp_dir <- tempdir()
    sample_data <- system.file("extdata", "datasets.xlsx", package = "readxl")
    file.copy(sample_data, file.path(temp_dir, "test.xlsx"))

    # Test with sheet number
    result <- excel_to_csv(temp_dir, "test.xlsx", sheet = 2)
    expect_true(file.exists(result))

    # Test with sheet name
    result <- excel_to_csv(temp_dir, "test.xlsx", sheet = "mtcars")
    expect_true(file.exists(result))
  })

  test_that("excel_to_csv handles errors appropriately", {
    temp_dir <- tempdir()

    # Test non-existent file
    expect_error(
      excel_to_csv(temp_dir, "nonexistent.xlsx")
    )

    # Test non-existent sheet
    sample_data <- system.file("extdata", "datasets.xlsx", package = "readxl")
    file.copy(sample_data, file.path(temp_dir, "test.xlsx"))
    expect_error(
      excel_to_csv(temp_dir, "test.xlsx", sheet = "NonexistentSheet")
    )

    # Test invalid path
    expect_error(
      excel_to_csv("/nonexistent/path", "test.xlsx")
    )
  })
}) # end of with_file block
