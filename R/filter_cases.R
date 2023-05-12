#' Filter Cases from a Data Frame
#'
#' This function filters cases from a data frame based on a given logical expression.
#' It also groups the data by specified variables if required.
#' NA values in the selected columns are also excluded during the filtering.
#'
#' @param data_df A data frame to be filtered.
#' @param select A character vector of variable names used in the logical expression.
#' @param logic A character string representing the logical expression to filter cases.
#' @param data_path A character string specifying the file path for saving the RDS file (default: "secure_data/<data_date>").
#' @param group_by_vars A character vector of variable names to group by (default: NULL).
#' @param rds A character string specifying the filename for the RDS file storing removed cases.
#' @param remove A logical value indicating whether to remove cases that don't meet the logical expression (default: TRUE).
#'
#' @return An invisible data frame with either the cases that meet the logical expression (if remove = TRUE) or the ungrouped input data (if remove = FALSE).
#' @export
#'
#' @examples
#' # Filter cases where age is greater than or equal to 18 and not NA
#' filtered_data <- filter_cases(data_df = my_data,
#' select = "age",
#' logic = "age >= 18",
#' rds = "cases_removed")
filter_cases <- function(data_df,
                         select,
                         logic,
                         data_path = file.path(getwd(), "secure_data", PROJECT_DICT$data_date),
                         group_by_vars = NULL,
                         rds = NULL,
                         remove = TRUE
){
  if (!is.data.frame(data_df)) {
    stop("data_df must be a data frame")
  }

  if (!all(select %in% names(data_df))) {
    stop(paste("The following columns are not in the data frame:", paste(select[!(select %in% names(data_df))], collapse = ", ")))
  }

  logic_parsed <- tryCatch(parse(text = logic),
                           error = function(e) stop("Invalid logic expression"))

  remove_cases <- data_df %>%
    dplyr::select(all_of(select)) %>%
    dplyr::filter(!eval(logic_parsed) | is.na(eval(logic_parsed)))

  keep_cases <- data_df %>%
    dplyr::filter(eval(logic_parsed) & !is.na(eval(logic_parsed)))

  if (!is.null(group_by_vars)) {
    keep_cases <- keep_cases %>%
      dplyr::group_by(across(all_of(group_by_vars)))
  }

  cat("\nCases removed: \n\n")
  print(remove_cases,
        row.names = FALSE)

  if (!is.null(rds)){
    if (!dir.exists(file.path(data_path, "Metadata", "Filtered"))){
      dir.create(file.path(data_path, "Metadata", "Filtered"), recursive = TRUE)
    }
    saveRDS(remove_cases,
            glue::glue("{data_path}/Metadata/Filtered/{rds}_removed.rds"))
  }

  if (remove == TRUE){
    return(invisible(keep_cases))
  } else{
    return(invisible(data_df))
  }

}
