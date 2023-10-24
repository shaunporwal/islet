#' Read and Clean Raw Data from a CSV or RDS File
#'
#' @param file A string specifying the full path to the CSV or RDS file.
#' @return A data frame with cleaned column names and data.
#' @export
#'
#' @examples
#' \dontrun{
#'   cleaned_data_csv <- read_raw_data("data.csv")
#'   cleaned_data_rds <- read_raw_data("data.rds")
#' }
#' @importFrom utils read.csv
#' @importFrom tools file_ext
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across where
#' @importFrom stringi stri_enc_isutf8

read_raw_data <- function(file) {
  file_extension <- tolower(tools::file_ext(file))

  if (file_extension == "csv") {
    df_raw_data <- read.csv(file = file, stringsAsFactors = FALSE, fileEncoding = "latin1")
  } else if (file_extension == "rds") {
    df_raw_data <- readRDS(file = file)
    if (!inherits(df_raw_data, "data.frame")) {
      stop("The RDS file must contain a data frame.")
    }
    names(df_raw_data) <- gsub("^X", "", names(df_raw_data))
  } else {
    stop("Invalid file extension. Only 'csv' and 'rds' are supported.")
  }

  mutate_logic <- function(col) {
    tryCatch({
      converted <- as.Date(col, format="%Y-%m-%d")
      if (all(is.na(converted))) {
        if (all(stringi::stri_enc_isutf8(col))) {
          return(toupper(col))
        } else {
          return(toupper(iconv(col, from = "latin1", to = "UTF-8")))
        }
      } else {
        return(converted)
      }
    }, error = function(e) {
      message("Error in converting column: ", conditionMessage(e))
      return(col)
    })
  }

  df_raw_data <- df_raw_data %>%
    janitor::clean_names(case = "all_caps") %>%
    dplyr::mutate(across(where(is.character), mutate_logic))

  return(df_raw_data)
}
