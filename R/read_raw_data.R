#' Read and Clean Raw Data
#'
#' This function reads a CSV file and performs several cleaning steps:
#' 1. Optionally converts column names to uppercase using janitor::clean_names.
#' 2. Attempts to parse character columns as dates in ymd format.
#' 3. Optionally converts character columns to uppercase if not parsed as dates.
#'
#' @param file A string specifying the path to the CSV file to be read.
#' @param col_caps Logical. Whether to convert column names to uppercase. Default is TRUE.
#' @param str_caps Logical. Whether to convert string column values to uppercase. Default is TRUE.
#'
#' @return A data frame with cleaned column names and data.
#' @export
read_raw_data <- function(file, col_caps = TRUE, str_caps = TRUE) {
  df_raw_data <- readr::read_csv(file)

  if (col_caps) {
    df_raw_data <- janitor::clean_names(df_raw_data, case = "all_caps")
  }

  df_raw_data <- df_raw_data %>%
    dplyr::mutate(dplyr::across(where(is.character), function(col) {
      if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", col))) {
        lubridate::ymd(col)
      } else if (str_caps) {
        toupper(col)
      } else {
        col
      }
    }))

  df_raw_data
}
