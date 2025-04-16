library(testthat)
library(dplyr)

# Helper function to create test data
create_test_data <- function() {
  data.frame(
    name = c("Alice", "bob", "Charlie"),
    date = c("2021-01-01", "2021-01-02", "2021-01-03"),
    value = c(10, 20, 30),
    `mixed Case` = c("a", "B", "c"), # Example of mixed case col name
    check.names = FALSE # Allow spaces and mixed case in names
  )
}

# Helper function to run common tests
run_common_tests <- function(df) {
  expect_true(is.data.frame(df))

  # Check column names are uppercase
  expect_true(all(names(df) == toupper(names(df))))

  # Check date column is Date type
  expect_true(inherits(df$DATE, "Date"))

  # Check character columns are uppercase
  char_cols <- sapply(df, is.character)
  if (any(char_cols)) {
    expect_true(all(sapply(df[, char_cols, drop = FALSE], function(col) {
      # Only check non-NA values for uppercasing
      valid_chars <- col[!is.na(col)]
      if (length(valid_chars) > 0) {
        all(valid_chars == toupper(valid_chars))
      } else {
        TRUE # Column is all NA
      }
    })))
  }
}

test_that("read_raw_data reads CSV files correctly", {
  test_data <- create_test_data()
  temp_csv <- tempfile(fileext = ".csv")
  readr::write_csv(test_data, temp_csv)

  df_csv <- read_raw_data(temp_csv)
  run_common_tests(df_csv)
  expect_equal(nrow(df_csv), 3)
  expect_equal(ncol(df_csv), 4)

  # Test col_caps = FALSE
  df_csv_nocaps <- read_raw_data(temp_csv, col_caps = FALSE)
  # Expect original mixed case name when col_caps = FALSE
  expect_true("mixed Case" %in% names(df_csv_nocaps))
  # Character columns should still be uppercase even if col_caps=FALSE
  expect_equal(df_csv_nocaps$`mixed Case`, c("A", "B", "C"))

  # Test str_caps = FALSE (should still result in uppercase strings now)
  df_csv_nostrcaps <- read_raw_data(temp_csv, str_caps = FALSE)
  # Expect uppercase strings even when str_caps = FALSE
  expect_equal(df_csv_nostrcaps$NAME, c("ALICE", "BOB", "CHARLIE"))
  expect_equal(df_csv_nostrcaps$MIXED_CASE, c("A", "B", "C"))

  unlink(temp_csv)
})

# Only run Excel tests if writexl is available
if (requireNamespace("writexl", quietly = TRUE)) {
  test_that("read_raw_data reads Excel (.xlsx) files correctly", {
    test_data <- create_test_data()
    temp_xlsx <- tempfile(fileext = ".xlsx")
    writexl::write_xlsx(list(Sheet1 = test_data), temp_xlsx)

    df_xlsx <- read_raw_data(temp_xlsx)
    run_common_tests(df_xlsx)
    expect_equal(nrow(df_xlsx), 3)
    expect_equal(ncol(df_xlsx), 4)

    # Test reading specific sheet
    writexl::write_xlsx(list(MySheet = test_data), temp_xlsx)
    df_xlsx_sheet <- read_raw_data(temp_xlsx, sheet = "MySheet")
    run_common_tests(df_xlsx_sheet)

    unlink(temp_xlsx)
  })
} else {
  message("Skipping Excel tests because 'writexl' package is not available.")
}

test_that("read_raw_data handles unsupported file types", {
  temp_txt <- tempfile(fileext = ".txt")
  writeLines(c("col1,col2", "val1,val2"), temp_txt)
  expect_error(read_raw_data(temp_txt), "Unsupported file type")
  unlink(temp_txt)
})
