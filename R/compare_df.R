#' Compare Dataframes
#'
#' Compares two datasets, summarizing numeric, factor, character, binary, date,
#' and group-specific fields. Handles comparisons with or without a new dataset.
#'
#' @param old_data Dataframe. The old dataset to compare.
#' @param new_data Dataframe. The new dataset to compare. If NULL, only the old dataset is analyzed.
#' @param suffix_term Character. Suffix to append to parsed column names (default: "").
#' @param ind_outcomes Character vector. Individual outcomes to summarize (default: "").
#' @param group_col Character. Column name representing the grouping variable (required).
#' @param add_years Logical. Whether to include year-based summaries (default: FALSE).
#'
#' @return A list of dataframes containing the comparison results:
#' \describe{
#'   \item{numeric_join}{Merged numeric fields.}
#'   \item{factor_join}{Merged factor fields.}
#'   \item{char_join}{Merged character fields.}
#'   \item{bin_join}{Merged binary fields.}
#'   \item{date_join}{Merged date fields.}
#'   \item{group_join}{Merged group-specific summaries.}
#' }
#' @export
compare_df <- function(old_data, new_data = NULL, suffix_term = "", ind_outcomes = c(""), group_col, add_years = FALSE) {
  # Ensure group_col is provided
  if (missing(group_col) || is.null(group_col)) {
    stop("The 'group_col' parameter is required and must be specified.")
  }

  # Helper function to clean dummy rows
  clean_dummy_rows <- function(df) {
    dummy_fields <- c("dummy_date", "dummy_posi", "dummy_char", "dummy_num", "dummy_factor")
    if (!is.atomic(df) && "field" %in% colnames(df)) {
      df <- df %>%
        filter(!.data$field %in% dummy_fields) %>%
        ungroup()
    }
    return(df)
  }

  # Parse old dataset
  old_parsed <- parse_function(
    parse_df = old_data,
    suffix_term = "old",
    ind_outcomes = ind_outcomes,
    group_col = group_col,
    add_years = add_years
  )
  old_parsed <- lapply(old_parsed, clean_dummy_rows)

  # If no new dataset, include all expected components as NULL where applicable
  if (is.null(new_data)) {
    complete_result <- list(
      numeric_join = old_parsed$summary_numeric %||% NULL,
      factor_join = old_parsed$factor_df %||% NULL,
      char_join = old_parsed$char_df %||% NULL,
      bin_join = old_parsed$binary_df %||% NULL,
      date_join = old_parsed$date_df %||% NULL,
      group_join = old_parsed$group_df %||% NULL
    )
    return(complete_result)
  }

  # Parse new dataset
  new_parsed <- parse_function(
    parse_df = new_data,
    suffix_term = "new",
    ind_outcomes = ind_outcomes,
    group_col = group_col,
    add_years = add_years
  )
  new_parsed <- lapply(new_parsed, clean_dummy_rows)

  # Generalized merging function
  merge_parsed_data <- function(old_df, new_df, by_col = "field") {
    if (is.null(old_df) || is.null(new_df)) return(NULL)
    suppressWarnings(full_join(old_df, new_df, by = by_col))
  }

  # Merge each type of data, ensuring all components are included
  comparison_list <- list(
    numeric_join = merge_parsed_data(old_parsed$summary_numeric, new_parsed$summary_numeric),
    factor_join = merge_parsed_data(old_parsed$factor_df, new_parsed$factor_df),
    char_join = merge_parsed_data(old_parsed$char_df, new_parsed$char_df),
    bin_join = merge_parsed_data(old_parsed$binary_df, new_parsed$binary_df),
    date_join = merge_parsed_data(old_parsed$date_df, new_parsed$date_df),
    group_join = merge_parsed_data(old_parsed$group_df, new_parsed$group_df, by_col = group_col)
  )

  # Ensure the output order is consistent and logical, even if some components are NULL
  ordered_comparison_list <- comparison_list[c(
    "numeric_join",
    "factor_join",
    "char_join",
    "bin_join",
    "date_join",
    "group_join"
  )]

  # Guarantee all elements are present as NULL if not available
  ordered_comparison_list <- lapply(ordered_comparison_list, function(x) if (is.null(x)) NULL else x)

  return(ordered_comparison_list)
}
