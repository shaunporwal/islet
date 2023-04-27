#' Save issue data to a specified path
#'
#' This function saves issue data to a specified path and prints the saved data to the console.
#' It takes a data frame, an RDS object, and optional data and issues paths as inputs.
#'
#' @param data_df A data frame containing issue data to be saved
#' @param rds An RDS object to be used for saving data
#' @param data_path An optional character string specifying the data path. Defaults to 'file.path(getwd(), "secure_data", PROJECT_DICT$data_date)'
#' @param issues_path An optional character string specifying the issues path. Defaults to 'file.path(getwd(), "secure_data", PROJECT_DICT$data_date, 'metadata', 'issues')'
#'
#' @return An invisible copy of the saved data frame
#' @export
#'
#' @examples
#' # Save issue data to the default paths
#' save_issue(data_df, rds)
#'
#' # Save issue data to custom paths
#' save_issue(data_df, rds, "custom_data_path", "custom_issues_path")
save_issue <- function(data_df,
                       rds,
                       data_path = file.path(getwd(), "secure_data", PROJECT_DICT$data_date),
                       issues_path =
                         file.path(getwd(), "secure_data", PROJECT_DICT$data_date, 'metadata', 'issues')) {

  saveRDS(data_df,
          glue::glue("{data_path}/{issues_path}/{rds}.rds"))

  cat("\nIssue saved:\n\n")

  print(data_df,
        row.names = FALSE)

  return(invisible(data_df))
}
