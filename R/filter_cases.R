#' Filter Cases from a Data Frame
#'
#' This function filters cases from a data frame based on the given logical expression.
#' It can also group the data by certain variables if needed.
#'
#' @param data_df A data frame to be filtered.
#' @param select A character vector of variable names used in the logical expression.
#' @param logic A character string representing the logical expression to filter cases.
#' @param group_by_vars A character vector of variable names to group by (default: NULL).
#' @param rds A character string specifying the filename for the RDS file storing removed cases.
#' @param data_path A character string specifying the file path for saving the RDS file (default: "secure_data/<data_date>").
#' @param remove A logical value indicating whether to remove cases that don't meet the logical expression (default: TRUE).
#'
#' @return An invisible data frame with either the cases that meet the logical expression (if remove = TRUE) or the ungrouped input data (if remove = FALSE).
#' @export
#'
#' @examples
#' # Filter cases where age is greater than or equal to 18
#' filtered_data <- filter_cases(data_df = my_data,
#' select = "age",
#' logic = "age >= 18",
#' rds = "cases_removed")
filter_cases <- function(data_df,
                         select,
                         logic,
                         group_by_vars = NULL,
                         rds,
                         data_path = file.path(getwd(), "secure_data", PROJECT_DICT$data_date),
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


  if (remove == TRUE){
    return(invisible(keep_cases))
  } else{
    return(invisible(data_df |> dplyr::ungroup()))
  }

}
