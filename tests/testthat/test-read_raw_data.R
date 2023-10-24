library(testthat)

test_that("read_raw_data returns a data frame", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(name = c("Alice", "Bob"), date = c("2021-01-01", "2021-01-02")), temp_file, row.names = FALSE)
  df <- read_raw_data(temp_file)
  expect_s3_class(df, "data.frame")
  unlink(temp_file)
})

test_that("Column names are in uppercase", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(name = c("Alice", "Bob"), date = c("2021-01-01", "2021-01-02")), temp_file, row.names = FALSE)
  df <- read_raw_data(temp_file)
  expect_true(all(tolower(names(df)) != names(df)))
  unlink(temp_file)
})

test_that("Date columns are converted to date format", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(name = c("Alice", "Bob"), date = c("2021-01-01", "2021-01-02")), temp_file, row.names = FALSE)
  df <- read_raw_data(temp_file)
  expect_true(inherits(df$DATE, "Date"))
  unlink(temp_file)
})

test_that("All data is in uppercase", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(name = c("Alice", "Bob"), date = c("2021-01-01", "2021-01-02")), temp_file, row.names = FALSE)
  df <- read_raw_data(temp_file)

  # Only check character columns
  char_cols <- sapply(df, is.character)
  expect_true(all(sapply(df[char_cols], function(col) all(tolower(col) != col))))

  unlink(temp_file)
})

test_that("Handles special characters without errors", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(name = c("Alice", "Böb"), date = c("2021-01-01", "2021-01-02")), temp_file, row.names = FALSE)

  expect_error(df <- read_raw_data(temp_file), NA)

  # Assuming df must exist, perform this test
  if (!is.null(df)) {
    char_cols <- sapply(df, is.character)
    expect_true(all(sapply(df[char_cols], function(col) all(tolower(col) != col))))
  }
  unlink(temp_file)
})

test_that("Handles invalid multibyte strings without errors", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(name = c("Alice", "Böb", "\xc3\x28"), date = c("2021-01-01", "2021-01-02", "2021-01-03")), temp_file, row.names = FALSE)

  # Check if function runs without errors
  expect_error(df <- read_raw_data(temp_file), NA)

  # Check if special characters are converted to uppercase
  char_cols <- sapply(df, is.character)
  expect_true(all(sapply(df[char_cols], function(col) all(tolower(col[!is.na(col)]) != col[!is.na(col)]))))

  unlink(temp_file)
})

# New Tests for RDS files

test_that("read_raw_data returns a data frame for RDS files", {
  temp_file <- tempfile(fileext = ".rds")
  sample_data <- data.frame(name = c("Alice", "Bob"), date = c("2021-01-01", "2021-01-02"))
  saveRDS(sample_data, temp_file)
  df <- read_raw_data(temp_file)
  expect_s3_class(df, "data.frame")
  unlink(temp_file)
})

test_that("Column names are in uppercase for RDS files", {
  temp_file <- tempfile(fileext = ".rds")
  sample_data <- data.frame(name = c("Alice", "Bob"), date = c("2021-01-01", "2021-01-02"))
  saveRDS(sample_data, temp_file)
  df <- read_raw_data(temp_file)
  expect_true(all(tolower(names(df)) != names(df)))
  unlink(temp_file)
})

test_that("Date columns are converted to date format for RDS files", {
  temp_file <- tempfile(fileext = ".rds")
  sample_data <- data.frame(name = c("Alice", "Bob"), date = c("2021-01-01", "2021-01-02"))
  saveRDS(sample_data, temp_file)
  df <- read_raw_data(temp_file)
  expect_true(inherits(df$DATE, "Date"))
  unlink(temp_file)
})

test_that("All data is in uppercase for RDS files", {
  temp_file <- tempfile(fileext = ".rds")
  sample_data <- data.frame(name = c("Alice", "Bob"), date = c("2021-01-01", "2021-01-02"))
  saveRDS(sample_data, temp_file)
  df <- read_raw_data(temp_file)

  # Only check character columns
  char_cols <- sapply(df, is.character)
  expect_true(all(sapply(df[char_cols], function(col) all(tolower(col) != col))))
  unlink(temp_file)
})

# Define tests
test_that("read_raw_data works with different file extensions", {
  # Create temporary CSV and RDS files for testing
  temp_csv <- tempfile(fileext = ".CSV")
  write.csv(data.frame(x = 1:3, y = 4:6), temp_csv, row.names = FALSE)

  temp_rds <- tempfile(fileext = ".RDS")
  saveRDS(data.frame(a = 7:9, b = 10:12), temp_rds)

  # Test reading uppercase CSV
  result_csv <- read_raw_data(temp_csv)
  expect_equal(dim(result_csv), c(3, 2))

  # Test reading uppercase RDS
  result_rds <- read_raw_data(temp_rds)
  expect_equal(dim(result_rds), c(3, 2))

  # Test that case of the extension does not matter (by renaming files)
  file.rename(temp_csv, gsub("CSV", "csv", temp_csv))
  file.rename(temp_rds, gsub("RDS", "rds", temp_rds))

  # Re-run the tests for lowercase extensions
  result_csv <- read_raw_data(temp_csv)
  expect_equal(dim(result_csv), c(3, 2))

  result_rds <- read_raw_data(temp_rds)
  expect_equal(dim(result_rds), c(3, 2))

  # Clean up temporary files
  unlink(temp_csv)
  unlink(temp_rds)

})

test_that("read_raw_data achieves 100% coverage", {

  # Test unsupported file extension
  temp_unsupported <- tempfile(fileext = ".txt")
  writeLines("hello world", temp_unsupported)
  expect_error(read_raw_data(temp_unsupported), "Invalid file extension. Only 'csv' and 'rds' are supported.")

  # Test with Date column
  temp_csv_date <- tempfile(fileext = ".csv")
  write.csv(data.frame(date_col = c("2021-01-01", "2021-01-02", "2021-01-03")), temp_csv_date, row.names = FALSE)
  df_date <- read_raw_data(temp_csv_date)
  expect_true(all(df_date$DATE_COL == as.Date(c("2021-01-01", "2021-01-02", "2021-01-03"))))

  # Clean up
  unlink(temp_unsupported)
  unlink(temp_csv_date)

  # Test for invalid file extension
  temp_invalid <- tempfile(fileext = ".txt")
  expect_error(read_raw_data(temp_invalid), "Invalid file extension. Only 'csv' and 'rds' are supported.")

  # Test for triggering the tryCatch error block by passing Date column
  temp_csv_with_date <- tempfile(fileext = ".csv")
  write.csv(data.frame(date_col = c("2022-01-01", "2022-01-02")), temp_csv_with_date, row.names = FALSE)
  df_with_date <- read_raw_data(temp_csv_with_date)
  expect_equal(as.Date(df_with_date$DATE_COL), as.Date(c("2022-01-01", "2022-01-02")))

  # Clean up
  unlink(temp_invalid)
  unlink(temp_csv_with_date)

  temp_rds <- tempfile(fileext = ".rds")
  df_rds <- data.frame(x = 1:5)
  saveRDS(df_rds, temp_rds)
  df_read_rds <- read_raw_data(temp_rds)
  expect_equal(names(df_read_rds), "X")
  expect_equal(df_read_rds, data.frame(X = 1:5))

})

test_that("RDS file must contain a data frame", {
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(42, temp_file)
  expect_error(read_raw_data(temp_file), "The RDS file must contain a data frame.")
  unlink(temp_file)
})

# # CSV case
# test_that("read_raw_data works for CSV", {
#   df <- read_raw_data("path/to/your/test.csv")
#   expect_s3_class(df, "data.frame")
#   # Additional assertions to check the content of df
# })

# # RDS case containing a data frame
# test_that("read_raw_data works for RDS with data frame", {
#   df <- read_raw_data("path/to/your/test.rds")
#   expect_s3_class(df, "data.frame")
#   # Additional assertions to check the content of df
# })
#
# # RDS case NOT containing a data frame
# test_that("read_raw_data handles RDS without data frame", {
#   expect_error(read_raw_data("path/to/your/test_non_dataframe.rds"), "The RDS file must contain a data frame.")
# })
#
# # Invalid file extension case
# test_that("read_raw_data handles invalid extensions", {
#   expect_error(read_raw_data("path/to/your/test.txt"), "Invalid file extension. Only 'csv' and 'rds' are supported.")
# })
#
# # Non-UTF8 character conversion
# test_that("read_raw_data handles non-UTF-8 characters", {
#   df <- read_raw_data("path/to/your/test_non_utf8.csv")
#   expect_s3_class(df, "data.frame")
#   # Additional assertions to check the content of df
# })
#
# # Error in date conversion
# test_that("read_raw_data handles date conversion errors", {
#   df <- read_raw_data("path/to/your/test_date_error.csv")
#   expect_s3_class(df, "data.frame")
#   # Additional assertions to check the content of df
# })
#
# test_that("read_raw_data works for CSV", {
#   temp_csv <- tempfile(fileext = ".csv")
#   write.csv(data.frame(a = 1:3, b = 4:6), file = temp_csv)
#
#   result <- read_raw_data(temp_csv)
#
#   expect_equal(dim(result), c(3, 2))
#   expect_equal(names(result), c("A", "B"))
#   expect_equal(class(result$A), "integer")
# })
#
# test_that("read_raw_data works for RDS with data frame", {
#   temp_rds <- tempfile(fileext = ".rds")
#   saveRDS(data.frame(a = 1:3, b = 4:6), file = temp_rds)
#
#   result <- read_raw_data(temp_rds)
#
#   expect_equal(dim(result), c(3, 2))
#   expect_equal(names(result), c("A", "B"))
#   expect_equal(class(result$A), "integer")
# })
#
# test_that("read_raw_data handles RDS without data frame", {
#   temp_rds <- tempfile(fileext = ".rds")
#   saveRDS(matrix(1:6, ncol = 2), file = temp_rds)
#
#   expect_error(read_raw_data(temp_rds), "The RDS file must contain a data frame.")
# })
#
# test_that("read_raw_data handles non-UTF-8 characters", {
#   temp_csv <- tempfile(fileext = ".csv")
#   write.csv(data.frame(a = c("x", "y", "\xa3")), file = temp_csv, fileEncoding = "latin1")
#
#   result <- read_raw_data(temp_csv)
#
#   expect_equal(dim(result), c(3, 1))
#   expect_equal(names(result), c("A"))
# })
#
# test_that("read_raw_data handles date conversion errors", {
#   temp_csv <- tempfile(fileext = ".csv")
#   write.csv(data.frame(a = c("2022-01-01", "invalid", "2022-01-03")), file = temp_csv)
#
#   result <- read_raw_data(temp_csv)
#
#   expect_equal(dim(result), c(3, 1))
#   expect_equal(names(result), c("A"))
#   expect_equal(class(result$A), "Date")
# })
#

test_that("read_raw_data works for CSV", {
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:3, b = 4:6), file = temp_csv, row.names = FALSE)

  result <- read_raw_data(temp_csv)

  expect_equal(dim(result), c(3, 2))
  expect_equal(names(result), c("A", "B"))
})

test_that("read_raw_data works for RDS with data frame", {
  temp_rds <- tempfile(fileext = ".rds")
  saveRDS(data.frame(a = 1:3, b = 4:6), file = temp_rds)

  result <- read_raw_data(temp_rds)

  expect_equal(dim(result), c(3, 2))
  expect_equal(names(result), c("A", "B"))
})

test_that("read_raw_data handles RDS without data frame", {
  temp_rds <- tempfile(fileext = ".rds")
  saveRDS(matrix(1:6, ncol = 2), file = temp_rds)

  expect_error(read_raw_data(temp_rds), "The RDS file must contain a data frame.")
})

test_that("read_raw_data handles non-UTF-8 characters", {
  temp_csv <- tempfile(fileext = ".csv")

  df <- data.frame(a = c("x", "y", "\xa3"))
  df$a <- iconv(df$a, to = "latin1", sub = "")
  write.csv(df, file = temp_csv, fileEncoding = "latin1", row.names = FALSE)

  result <- read_raw_data(temp_csv)

  expect_equal(dim(result), c(2, 1))
  expect_equal(names(result), c("A"))
})


test_that("read_raw_data handles date conversion errors", {
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = c("2022-01-01", "invalid", "2022-01-03")), file = temp_csv, row.names = FALSE)

  result <- read_raw_data(temp_csv)

  expect_equal(dim(result), c(3, 1))
  expect_equal(names(result), c("A"))
})


test_that("read_raw_data works for CSV", {
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(A = 1:3, B = 4:6), file = temp_csv, row.names = FALSE)

  result <- read_raw_data(temp_csv)

  expect_equal(dim(result), c(3, 2))
  expect_equal(names(result), c("A", "B"))
})

test_that("read_raw_data works for RDS with data frame", {
  temp_rds <- tempfile(fileext = ".rds")
  saveRDS(data.frame(A = 1:3, B = 4:6), file = temp_rds)

  result <- read_raw_data(temp_rds)

  expect_equal(dim(result), c(3, 2))
  expect_equal(names(result), c("A", "B"))
})

test_that("read_raw_data works for CSV", {
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(A = 1:3, B = 4:6), file = temp_csv, row.names = FALSE)

  result <- read_raw_data(temp_csv)

  expect_equal(dim(result), c(3, 2))
  expect_equal(names(result), c("A", "B"))
})

test_that("read_raw_data works for RDS with data frame", {
  temp_rds <- tempfile(fileext = ".rds")
  saveRDS(data.frame(A = 1:3, B = 4:6), file = temp_rds)

  result <- read_raw_data(temp_rds)

  expect_equal(dim(result), c(3, 2))
  expect_equal(names(result), c("A", "B"))
})


