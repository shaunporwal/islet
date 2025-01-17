#' Parse a Dataframe for Analysis
#'
#' Processes a dataframe to generate summaries for numeric, factor, character,
#' binary, date, and other data types. Supports year-based summaries and group-specific outcomes.
#' Will handle cases where certain column types are not present in the dataset.
#'
#' @param parse_df Dataframe. Input dataframe to parse and analyze.
#' @param suffix_term Character. Suffix to append to parsed column names (default: "").
#' @param ind_outcomes Character vector. Individual outcomes for parsing and group-specific summaries (default: "").
#' @param group_col Character. Column name representing the grouping variable (default: NULL).
#' @param add_years Logical. Include year-based summaries in the output (default: FALSE).
#'
#' @return A list of dataframes summarizing the input dataset. Only includes summaries for column types
#' that are present in the input data:
#' \describe{
#'   \item{date_df}{Summaries for date columns (if present).}
#'   \item{binary_df}{Summaries for binary columns (if present).}
#'   \item{char_df}{Summaries for character columns (if present).}
#'   \item{factor_df}{Summaries for factor columns (if present).}
#'   \item{summary_numeric}{Summaries for numeric columns (if present).}
#'   \item{group_df}{Summaries by group for specified outcomes (if applicable).}
#' }
#' @export
parse_function <- function(parse_df, suffix_term = "", ind_outcomes = c(""), group_col = NULL, add_years = FALSE) {
  # Ensure the dataframe is ungrouped
  parse_df <- parse_df %>% ungroup()
  
  results <- list()
  
  # Check for and summarize date columns
  date_cols <- names(parse_df)[sapply(parse_df, inherits, "Date")]
  if (length(date_cols) > 0) {
    min_date <- parse_df %>%
      summarise(across(all_of(date_cols), ~ min(., na.rm = TRUE))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "min_date")
    
    max_date <- parse_df %>%
      summarise(across(all_of(date_cols), ~ max(., na.rm = TRUE))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "max_date")
    
    perc_na_date <- parse_df %>%
      summarise(across(all_of(date_cols), ~ mean(is.na(.), na.rm = TRUE))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_date")
    
    results$date_df <- full_join(full_join(min_date, max_date, by = "field"), perc_na_date, by = "field")
  }
  
  # Check for and summarize binary columns
  binary_cols <- names(parse_df)[sapply(parse_df, function(x) 
    is.logical(x) || (is.numeric(x) && length(unique(na.omit(x))) <= 2))]
  if (length(binary_cols) > 0) {
    ratio_binary <- parse_df %>%
      summarise(across(all_of(binary_cols), ~ mean(., na.rm = TRUE))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "ratio_binary")
    
    perc_na_binary <- parse_df %>%
      summarise(across(all_of(binary_cols), ~ mean(is.na(.), na.rm = TRUE))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_binary")
    
    results$binary_df <- full_join(ratio_binary, perc_na_binary, by = "field")
  }
  
  # Check for and summarize character columns
  char_cols <- names(parse_df)[sapply(parse_df, is.character)]
  if (length(char_cols) > 0) {
    values_char <- parse_df %>%
      summarise(across(all_of(char_cols), ~ paste(unique(.), collapse = ", "))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "values_char")
    
    distinct_char <- parse_df %>%
      summarise(across(all_of(char_cols), ~ n_distinct(.))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "distinct_char")
    
    perc_na_char <- parse_df %>%
      summarise(across(all_of(char_cols), ~ mean(is.na(.), na.rm = TRUE))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_char")
    
    results$char_df <- full_join(full_join(values_char, distinct_char, by = "field"), 
                                perc_na_char, by = "field")
  }
  
  # Check for and summarize factor columns
  factor_cols <- names(parse_df)[sapply(parse_df, is.factor)]
  if (length(factor_cols) > 0) {
    levels_factor <- parse_df %>%
      summarise(across(all_of(factor_cols), ~ paste(levels(.), collapse = ", "))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "levels_factor")
    
    distinct_factor <- parse_df %>%
      summarise(across(all_of(factor_cols), ~ n_distinct(.))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "distinct_factor")
    
    perc_na_factor <- parse_df %>%
      summarise(across(all_of(factor_cols), ~ mean(is.na(.), na.rm = TRUE))) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_factor")
    
    results$factor_df <- full_join(full_join(levels_factor, distinct_factor, by = "field"), 
                                  perc_na_factor, by = "field")
  }
  
  # Check for and summarize numeric columns - FIXED THIS SECTION
  numeric_cols <- names(parse_df)[sapply(parse_df, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, binary_cols)  # Exclude binary columns
  
  if (length(numeric_cols) > 0) {
    summary_stats <- parse_df %>%
      summarise(across(all_of(numeric_cols), list(
        mean = ~mean(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        q25 = ~quantile(., 0.25, na.rm = TRUE),
        q75 = ~quantile(., 0.75, na.rm = TRUE),
        na_perc = ~mean(is.na(.))
      )))
    
    # Restructure the output to match expected format
    results$summary_numeric <- data.frame(
      field = rep(numeric_cols, each = 7),
      statistic = rep(c("mean", "min", "max", "median", "q25", "q75", "na_perc"), times = length(numeric_cols)),
      value = unlist(summary_stats)
    )
  }
  
  # Handle group-specific outcomes
  if (ind_outcomes[[1]] != "" && !is.null(group_col)) {
    if (!all(ind_outcomes %in% colnames(parse_df))) {
      stop("Some specified outcomes are missing in the dataset.")
    }
    if (!(group_col %in% colnames(parse_df))) {
      stop("The specified grouping column is missing in the dataset.")
    }
    results$group_df <- parse_df %>%
      select(all_of(c(group_col, ind_outcomes))) %>%
      group_by(!!sym(group_col)) %>%
      summarise(across(all_of(ind_outcomes), \(x) mean(x, na.rm = TRUE))) %>%
      rename(group = !!sym(group_col))
  } else {
    results$group_df <- data.frame(group = character(0))
  }
  
  return(results)
}