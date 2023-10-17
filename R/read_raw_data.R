#' Read and Clean Raw Data from a CSV File
#'
#' @param file A string specifying the full path to the CSV file.
#' @return A data frame with cleaned column names and data.
#' @export
#'
#' @examples
#' \dontrun{
#'   cleaned_data <- read_raw_data("data.csv")
#' }
#' @importFrom utils read.csv
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across where
read_raw_data <- function(file) {
  df_raw_data <- read.csv(file = file, stringsAsFactors = FALSE) |>
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
