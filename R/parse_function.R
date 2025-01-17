#' Parse a Dataframe for Analysis
#'
#' Processes a dataframe to generate summaries for numeric, factor, character,
#' binary, date, and other data types. Supports year-based summaries and group-specific outcomes.
#'
#' @param parse_df Dataframe. Input dataframe to parse and analyze.
#' @param suffix_term Character. Suffix to append to parsed column names (default: "").
#' @param ind_outcomes Character vector. Individual outcomes for parsing and group-specific summaries (default: "").
#' @param group_col Character. Column name representing the grouping variable (default: NULL).
#' @param add_years Logical. Include year-based summaries in the output (default: FALSE).
#'
#' @return A list of dataframes summarizing the input dataset:
#' \describe{
#'   \item{date_df}{Summaries for date columns (e.g., min, max, NA percentages).}
#'   \item{binary_df}{Summaries for binary columns (e.g., ratio of TRUE, NA percentages).}
#'   \item{char_df}{Summaries for character columns (e.g., unique values, NA percentages).}
#'   \item{factor_df}{Summaries for factor columns (e.g., unique levels, NA percentages).}
#'   \item{summary_numeric}{Summaries for numeric columns (e.g., mean, quartiles, NA percentages).}
#'   \item{group_df}{Summaries by group for specified outcomes (if applicable).}
#' }
#' @export
parse_function <- function(parse_df, suffix_term = "", ind_outcomes = c(""), group_col = NULL, add_years = FALSE) {
  # Ensure the dataframe is ungrouped
  parse_df <- parse_df %>% ungroup()

  # Add dummy values to ensure functions work with empty datasets
  parse_df$dummy_date <- as.Date("1000-01-01")
  parse_df$dummy_char <- ""
  parse_df$dummy_num <- -1
  parse_df$dummy_factor <- as.factor(c(0))

  # [Previous date, binary, char, factor, and numeric summaries remain the same]
  # ... 

  # Validate and summarize group-specific outcomes
  if (ind_outcomes[[1]] != "" && !is.null(group_col)) {
    if (!all(ind_outcomes %in% colnames(parse_df))) {
      stop("Some specified outcomes are missing in the dataset.")
    }
    if (!(group_col %in% colnames(parse_df))) {
      stop("The specified grouping column is missing in the dataset.")
    }
    group_df <- parse_df %>%
      select(all_of(c(group_col, ind_outcomes))) %>%
      group_by(!!sym(group_col)) %>%
      summarise(across(all_of(ind_outcomes), mean, na.rm = TRUE)) %>%
      rename(group = !!sym(group_col))
  } else {
    group_df <- data.frame(group = character(0))
  }

  # Compile results into a list
  results <- list(
    date_df = date_df,
    binary_df = binary_df,
    char_df = char_df,
    factor_df = factor_df,
    summary_numeric = summary_numeric,
    group_df = group_df
  )

  return(results)
}