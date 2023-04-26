#' Filter out problematic rows for review
#'
#' @description `filter_cases` returns a DataFrame without the rows filtered out by the
#' logic, and saves the filtered-out rows to an RDS file.
#'
#' @param data_df A DataFrame to filter.
#' @param select A character vector of column names to select for filtering.
#' @param logic A character string representing the filtering condition.
#' @param group_by_vars A character vector of column names to group by (default is NULL).
#' @param rds A character string for the RDS file name to save the filtered-out rows.
#' @param data_path A character string representing the path to the data folder (default is get_data_path()).
#' @param remove A logical indicating whether to remove the filtered rows from the returned DataFrame (default is TRUE).
#'
#' @return A DataFrame without the rows filtered out by the logic.
#' @export
#'
#' @examples
#' # Assuming you have a DataFrame called 'my_data'
#' # Filter out rows where the value in the 'age' column is less than 18
#' filtered_data <- filter_cases(
#'   data_df = my_data,
#'   select = "age",
#'   logic = "age >= 18",
#'   rds = "age_below_18",
#'   data_path = "path/to/your/data"
#' )
filter_cases <- function(data_df,
                         select,
                         logic,
                         group_by_vars = NULL,
                         rds,
                         data_path = get_data_path(),
                         remove = TRUE
                         ){
  data_df <-
    data_df %>%
    dplyr::group_by_at(c(group_by_vars))

  remove_cases <- data_df %>%
    dplyr::select(one_of(select)) %>%
    dplyr::filter((!eval(parse(text = logic))) %>%
                    tidyr::replace_na(FALSE))

  keep_cases <- data_df %>%
    dplyr::filter(eval(parse(text = logic)) %>%
                    tidyr::replace_na(TRUE))

  cat("\nCases removed: \n\n")
  print(remove_cases,
        row.names = FALSE)

  saveRDS(remove_cases,
          glue::glue("{data_path}/Metadata/Filtered/{rds}.rds"))



  return(invisible(keep_cases))

}
