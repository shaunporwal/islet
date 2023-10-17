library(testthat)

test_that("read_raw_data returns a data frame", {
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(name = c("Alice", "Bob"), date = c("2021-01-01", "2021-01-02")), temp_file, row.names = FALSE)
  df <- read_raw_data(temp_file)
  expect_type(df, "list")
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

  # Check if function runs without errors
  expect_error(df <- read_raw_data(temp_file), NA)

  # Check if special characters are converted to uppercase
  char_cols <- sapply(df, is.character)
  expect_true(all(sapply(df[char_cols], function(col) all(tolower(col) != col))))

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


