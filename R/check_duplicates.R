#' Title
#'
#' @param data_df Dataframe to be checked for duplicates
#' @param group_by_vars A string or vector of strings of column name(s) to group by in asserting uniqueness
#'
#' @return Duplicate rows, both in general and by variable/column groupings
#' @export
#'
#' @examples Coming Soon
#'
#'
check_duplicates <- function(data_df,
                             group_by_vars = NULL,
                             print_dups = TRUE) {

  # Capture exact duplicates
  dup_df <- data_df[duplicated(data_df), ]

  # Capture multiple rows per grouped variables
  mult_df <- data_df %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::group_by(across(all_of(group_by_vars))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::arrange(across(all_of(group_by_vars)))

  union_df <- dplyr::bind_rows(dup_df, mult_df)

  if (print_dups == TRUE){
    if (nrow(union_df) > 0) {
      print(paste0("Duplicates Exist:"))
      print(mult_df)
    }
  }

  return(invisible(union_df))
}
