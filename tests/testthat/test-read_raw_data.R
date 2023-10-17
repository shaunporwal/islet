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



