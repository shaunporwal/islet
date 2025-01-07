#' Read and Clean Raw Data
#'
#' This function reads a CSV file and performs several cleaning steps:
#' 1. Converts column names to uppercase.
#' 2. Attempts to parse character columns as dates in ymd format.
#' 3. Converts character columns to uppercase if not parsed as dates.
#'
#' @param file A string specifying the path to the CSV file to be read.
#'
#' @return A data frame with cleaned column names and data.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming 'data.csv' contains appropriate data
#'   cleaned_data <- read_raw_data("data.csv")
#' }
read_raw_data <- function(file) {
  df_raw_data <- utils::read.csv(file = file, stringsAsFactors = FALSE) %>%
    janitor::clean_names(case = "all_caps") %>%
    mutate(across(where(is.character), function(col) {
      tryCatch({
        converted <- lubridate::ymd(col)
        if (all(is.na(converted))) {
          return(toupper(col))
        } else {
          return(converted)
        }
      }, error = function(e) {
        return(toupper(col))
      })
    }))

  return(df_raw_data)
}
