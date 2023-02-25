#' Title
#'
#' @param data_df
#' @param group_by_vars
#'
#' @return
#' @export
#'
#' @examples
#'
#'
assert_distinct <- function(data_df,
                            group_by_vars = NULL) {
  dup_df <- data_df[duplicated(data_df), ]
  if (nrow(dup_df) > 0) {
    dup_df$errorType <- "Duplicate Rows"
  }

  mult_df <- data_df %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::group_by(across(all_of(group_by_vars))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::arrange(across(all_of(group_by_vars))) %>%
    dplyr::mutate(errorType = "Multiple Rows Per Identifier")

  union_df <- dplyr::bind_rows(dup_df, mult_df) %>%
    dplyr::arrange(.data$errorType)

  if (nrow(union_df) > 0) {
    error_print <- paste0("Assert distinct failed. nrow(union_df) > 0")
    print(mult_df)
  }

  return(invisible(union_df))
}
