#' Compare Dataframes
#'
#' Compares two datasets, summarizing numeric, factor, character, binary, date,
#' and group-specific fields. Handles single dataset analysis or comparison between two datasets.
#'
#' @param old_data Dataframe containing the base dataset for comparison
#' @param new_data Optional dataframe to compare against old_data. If NULL, only old_data is analyzed
#' @param suffix_term Character string to append to parsed column names (default: "")
#' @param ind_outcomes Character vector of individual outcomes to summarize (default: "")
#' @param group_col Required character string specifying the grouping variable column name
#' @param add_years Logical indicating whether to include year-based summaries (default: FALSE)
#'
#' @return A list containing:
#' \describe{
#'   \item{numeric_join}{Numeric field comparisons}
#'   \item{factor_join}{Factor level comparisons}
#'   \item{char_join}{Character field comparisons}
#'   \item{bin_join}{Binary field comparisons}
#'   \item{date_join}{Date field comparisons}
#'   \item{group_join}{Group-specific comparisons}
#' }
#'
#' Each component may be NULL if that type of data is not present.
#' @importFrom dplyr %>% filter ungroup full_join
#' @export
compare_df <- function(old_data, new_data = NULL, suffix_term = "", ind_outcomes = c(""), group_col, add_years = FALSE) {
  if (missing(group_col) || is.null(group_col)) {
    stop("The 'group_col' parameter is required.")
  }
  if (is.null(old_data)) {
    stop("old_data requires a dataframe value")
  }

  merge_parsed_data <- function(old_df, new_df, by_col = "field", is_group = FALSE) {
    if (is.null(old_df) || is.null(new_df)) {
      return(NULL)
    }

    if (is_group && !is.null(group_col)) {
      # For group data, ensure the column exists and handle appropriately
      if (!(group_col %in% names(old_df)) || !(group_col %in% names(new_df))) {
        warning(paste("Group column", group_col, "not found in data"))
        return(NULL)
      }
      suppressWarnings(full_join(old_df, new_df, by = group_col))
    } else {
      suppressWarnings(full_join(old_df, new_df, by = by_col))
    }
  }

  old_parsed <- parse_function(
    parse_df = old_data,
    suffix_term = "old",
    ind_outcomes = ind_outcomes,
    group_col = group_col,
    add_years = add_years
  )

  if (is.null(new_data)) {
    return(list(
      numeric_join = old_parsed$summary_numeric %||% NULL,
      factor_join = old_parsed$factor_df %||% NULL,
      char_join = old_parsed$char_df %||% NULL,
      bin_join = old_parsed$binary_df %||% NULL,
      date_join = old_parsed$date_df %||% NULL,
      group_join = old_parsed$group_df %||% NULL
    ))
  }

  new_parsed <- parse_function(
    parse_df = new_data,
    suffix_term = "new",
    ind_outcomes = ind_outcomes,
    group_col = group_col,
    add_years = add_years
  )

  comparison_list <- list(
    numeric_join = merge_parsed_data(old_parsed$summary_numeric, new_parsed$summary_numeric),
    factor_join = merge_parsed_data(old_parsed$factor_df, new_parsed$factor_df),
    char_join = merge_parsed_data(old_parsed$char_df, new_parsed$char_df),
    bin_join = merge_parsed_data(old_parsed$binary_df, new_parsed$binary_df),
    date_join = merge_parsed_data(old_parsed$date_df, new_parsed$date_df),
    group_join = merge_parsed_data(old_parsed$group_df, new_parsed$group_df, by_col = group_col, is_group = TRUE)
  )

  ordered_comparison_list <- comparison_list[c(
    "numeric_join",
    "factor_join",
    "char_join",
    "bin_join",
    "date_join",
    "group_join"
  )]

  lapply(ordered_comparison_list, function(x) if (is.null(x)) NULL else x)
}
