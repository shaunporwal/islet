#' #' Read and Clean Raw Data from a CSV File
#' #'
#' #' @param file A string specifying the full path to the CSV file.
#' #' @return A data frame with cleaned column names and data.
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #'   cleaned_data <- read_raw_data("data.csv")
#' #' }
#' #' @importFrom utils read.csv
#' #' @importFrom janitor clean_names
#' #' @importFrom dplyr mutate across where
#' read_raw_data <- function(file) {
#'   df_raw_data <- read.csv(file = file, stringsAsFactors = FALSE) |>
#'     janitor::clean_names(case = "all_caps") |>
#'     dplyr::mutate(across(where(is.character), function(col) {
#'       tryCatch({
#'         converted <- as.Date(col)
#'         if (all(is.na(converted))) {
#'           if (all(stringi::stri_enc_isutf8(col))) {
#'             return(toupper(iconv(col, to = "UTF-8")))
#'           } else {
#'             return(col)
#'           }
#'         } else {
#'           return(converted)
#'         }
#'       }, error = function(e) {
#'         return(toupper(iconv(col, to = "UTF-8")))
#'       })
#'     }))
#'
#'   return(df_raw_data)
#' }


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
#' @importFrom utils read.csv readRDS
#' @importFrom tools file_ext
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across where
read_raw_data <- function(file) {
  file_extension <- tools::file_ext(file)

  if (file_extension == "csv") {
    df_raw_data <- read.csv(file = file, stringsAsFactors = FALSE)
  } else if (file_extension == "rds") {
    df_raw_data <- readRDS(file = file)
  } else {
    stop("Invalid file extension. Only 'csv' and 'rds' are supported.")
  }

  df_raw_data <- df_raw_data |>
    janitor::clean_names(case = "all_caps") |>
    dplyr::mutate(across(where(is.character), function(col) {
      tryCatch({
        converted <- as.Date(col)
        if (all(is.na(converted))) {
          if (all(stringi::stri_enc_isutf8(col))) {
            return(toupper(iconv(col, to = "UTF-8")))
          } else {
            return(col)
          }
        } else {
          return(converted)
        }
      }, error = function(e) {
        return(toupper(iconv(col, to = "UTF-8")))
      })
    }))

  return(df_raw_data)
}
