#' Compare Dataframes
#'
#' Compares two datasets, summarizing numeric, factor, character, binary, date,
#' and surgeon-specific fields. Handles comparisons with or without a new dataset.
#'
#' @param old_data Dataframe. The old dataset to compare.
#' @param new_data Dataframe. The new dataset to compare. If NULL, only the old dataset is analyzed.
#' @param suffix_term Character. Suffix to append to parsed column names (default: "").
#' @param ind_outcomes Character vector. Individual outcomes to summarize (default: "").
#' @param surg_col Character. Column name representing surgeons (default: "surgeon").
#'
#' @return A list of dataframes containing the comparison results:
#' \describe{
#'   \item{numeric_join}{Merged numeric fields.}
#'   \item{factor_join}{Merged factor fields.}
#'   \item{char_join}{Merged character fields.}
#'   \item{bin_join}{Merged binary fields.}
#'   \item{date_join}{Merged date fields.}
#'   \item{surg_join}{Merged surgeon-specific summaries.}
#' }
#' @export
compare_df <- function(old_data = NULL, new_data = NULL, suffix_term = "", ind_outcomes = c(""), surg_col = "surgeon") {
  # Analyze the old dataset
  if (is.null(new_data)) {
    old_data <- parse_function(old_data, "old", ind_outcomes, surg_col, add_years = TRUE)

    # Clean up dummy rows and return analysis of old dataset only
    old_data <- lapply(old_data, function(df) {
      if (!is.null(df$field)) {
        df <- df %>%
          filter(!.data$field %in% c("dummy_date", "dummy_posi", "dummy_char", "dummy_num", "dummy_factor")) %>%
          ungroup()
      }
      return(df)
    })
    return(old_data)
  }

  # Analyze the old and new datasets
  old_data <- parse_function(old_data, "old", ind_outcomes, surg_col)
  new_data <- parse_function(new_data, "new", ind_outcomes, surg_col)

  # Merge numeric fields
  numeric_join <- suppressWarnings(full_join(old_data[[1]], new_data[[1]], by = "field"))
  numeric_join <- numeric_join[, c(1, 2, 9, 3, 10, 4, 11, 5, 12, 6, 13, 7, 14, 8, 15)]

  # Merge factor fields
  factor_join <- suppressWarnings(full_join(old_data[[2]], new_data[[2]], by = "field"))
  factor_join <- factor_join[, c(1, 2, 5, 3, 6, 4, 7)]

  # Merge character fields
  char_join <- suppressWarnings(full_join(old_data[[3]], new_data[[3]], by = "field"))
  char_join <- char_join[, c(1, 2, 5, 3, 6, 4, 7)]

  # Merge binary fields
  bin_join <- suppressWarnings(full_join(old_data[[4]], new_data[[4]], by = "field"))
  bin_join <- bin_join[, c(1, 2, 4, 3, 5)]

  # Merge date fields
  date_join <- suppressWarnings(full_join(old_data[[5]], new_data[[5]], by = "field"))
  date_join <- date_join[, c(1, 2, 5, 3, 6, 4, 7)]

  # Merge surgeon-specific summaries
  surg_join <- suppressWarnings(full_join(old_data[[6]], new_data[[6]], by = surg_col))
  surg_join <- surg_join[, order(names(surg_join))]
  surg_join <- surg_join[c(surg_col, setdiff(names(surg_join), surg_col))]

  # Combine all joined dataframes into a list
  final_df_list <- list(
    numeric_join = numeric_join,
    factor_join = factor_join,
    char_join = char_join,
    bin_join = bin_join,
    date_join = date_join,
    surg_join = surg_join
  )

  # Clean up dummy rows in the joined dataframes
  final_df_list <- lapply(final_df_list, function(df) {
    if (!is.atomic(df) && !is.null(df$field)) {
      df <- df %>%
        filter(!.data$field %in% c("dummy_date", "dummy_posi", "dummy_char", "dummy_num", "dummy_factor")) %>%
        ungroup()
    }
    return(df)
  })

  return(final_df_list)
}
