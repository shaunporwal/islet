#' Title
#'
#' @param data_df Dataframe to be checked for duplicates
#' @param group_by_vars A string or vector of strings of column name(s) to group by in asserting uniqueness
#' @param print_dups A boolean argument, if TRUE, will allow function to display duplicate rows
#'
#' @return Duplicate rows, both in general and by variable/column groupings
#'
#' @examples
#' # Load the dplyr package
#' library(dplyr)
#'
#' # Create a sample data frame
#' data_df <- data.frame(
#'   ID = c(1, 1, 2, 3, 4),
#'   Age = c(25, 25, 30, 40, 50),
#'   Income = c(50000, 50000, 60000, 70000, 80000)
#' )
#'
#' # Call the function to check for duplicates in the entire data frame
#' check_duplicates(data_df)
#'
#' # Call the function to check for duplicates within the "ID" column
#' check_duplicates(data_df, group_by_vars = "ID")
#'
#' # Call the function to check for duplicates within the "ID" and "Age" columns
#' check_duplicates(data_df, group_by_vars = c("ID", "Age"))
#'
#' @export
check_duplicates <- function(data_df,
                             group_by_vars = NULL,
                             print_dups = TRUE) {

  # Capture exact duplicates
  dup_df <- data_df[duplicated(data_df), ]

  # Capture multiple rows per grouped variables
  mult_df <- data.frame()
  if (!is.null(group_by_vars)) {
    mult_df <- data_df %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      dplyr::group_by(across(all_of(group_by_vars))) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::arrange(across(all_of(group_by_vars)))
  }

  union_df <- dplyr::bind_rows(dup_df, mult_df)

  if (print_dups == TRUE){
    if (nrow(union_df) > 0) {
      print(paste0("Duplicates Exist:"))
      print(union_df)
    }
  }

  return(invisible(union_df))
}


