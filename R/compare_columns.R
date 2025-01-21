#' Compare Column Names Between Two Data Frames
#'
#' This function identifies the differences in column names between two data frames,
#' showing which columns are unique to each data frame.
#'
#' @param df1 A data frame or tibble
#' @param df2 A data frame or tibble
#' @param df1_name Character string naming the first data frame (default: "df1")
#' @param df2_name Character string naming the second data frame (default: "df2")
#'
#' @return A list with two character vectors:
#' \itemize{
#'   \item `unique_to_df1`: Column names present only in df1
#'   \item `unique_to_df2`: Column names present only in df2
#' }
#'
#' @examples
#' df1 <- data.frame(a = 1, b = 2, c = 3)
#' df2 <- data.frame(b = 2, c = 3, d = 4)
#' compare_columns(df1, df2, "first_df", "second_df")
#'
#' @export
compare_columns <- function(df1, df2, df1_name = "df1", df2_name = "df2") {
  # Input validation
  if (!is.data.frame(df1) || !is.data.frame(df2)) {
    stop("Both inputs must be data frames or tibbles")
  }
  
  if (!is.character(df1_name) || !is.character(df2_name)) {
    stop("Data frame names must be character strings")
  }
  
  # Find unique columns in each dataframe
  cols_unique_to_df1 <- setdiff(names(df1), names(df2))
  cols_unique_to_df2 <- setdiff(names(df2), names(df1))
  
  # Create return list
  result <- list(
    unique_to_df1 = cols_unique_to_df1,
    unique_to_df2 = cols_unique_to_df2
  )
  
  # Add names attribute for clarity
  names(result) <- c(
    paste0("unique_to_", df1_name),
    paste0("unique_to_", df2_name)
  )
  
  return(result)
}