#' Read and Clean Raw Data from CSV or Excel
#'
#' This function reads a CSV or Excel (.xlsx) file and performs several cleaning steps:
#' 1. Detects file type based on extension.
#' 2. Optionally converts column names to uppercase using janitor::clean_names.
#' 3. Attempts to parse character columns as dates in ymd format.
#' 4. Converts remaining character columns to uppercase.
#'
#' @param file A string specifying the path to the CSV or Excel file.
#' @param sheet Optional: sheet name or index if reading an Excel file. Defaults to the first sheet.
#' @param col_caps Logical. Whether to convert column names to uppercase. Default is TRUE.
#' @param str_caps Deprecated. Character strings are now always converted to uppercase.
#'
#' @return A data frame with cleaned column names and data.
#' @export
#' @importFrom tools file_ext
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across
#' @importFrom lubridate ymd
read_raw_data <- function(file, sheet = 1, col_caps = TRUE, str_caps = TRUE) {
  # Deprecate str_caps argument - now always TRUE effectively
  if (!isTRUE(str_caps)) {
    # warning("`str_caps` argument is deprecated and ignored. Character columns are always converted to uppercase.", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(file))

  if (ext == "csv") {
    df_raw_data <- readr::read_csv(file, show_col_types = FALSE)
  } else if (ext == "xlsx") {
    df_raw_data <- readxl::read_excel(file, sheet = sheet)
  } else {
    stop("Unsupported file type. Please provide a .csv or .xlsx file.", call. = FALSE)
  }

  if (col_caps) {
    df_raw_data <- janitor::clean_names(df_raw_data, case = "all_caps")
  }

  # Apply date parsing and string capitalization
  df_raw_data <- df_raw_data %>%
    dplyr::mutate(dplyr::across(where(is.character), function(col) {
      # Try parsing as date first
      parsed_date <- suppressWarnings(lubridate::ymd(col, quiet = TRUE))

      # Check if parsing as date was largely successful (handle NAs correctly)
      was_date <- !all(is.na(parsed_date[which(!is.na(col))]))

      if (was_date) {
        return(parsed_date) # Return dates if successful
      } else {
        # Always convert non-date character columns to uppercase
        return(toupper(col))
      }
    }))

  df_raw_data
}
