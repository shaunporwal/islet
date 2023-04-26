#' Filter out problematic rows for review
#'
#' `filter_cases` returns a DataFrame without the rows filtered out by the
#' logic, and saved the filtered-out rows
#'
#' @param data_df
#' @param select
#' @param logic
#' @param group_by_vars
#' @param rds
#' @param data_path
#'
#' @return
#' @export
#'
#' @examples
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
