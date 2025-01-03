#' Parse a Dataframe for Analysis
#'
#' Processes a dataframe to generate summaries for numeric, factor, character,
#' binary, date, and other data types. Supports year-based summaries and surgeon-specific outcomes.
#'
#' @param parse_df Dataframe. Input dataframe to parse and analyze.
#' @param suffix_term Character. Suffix to append to parsed column names (default: "").
#' @param ind_outcomes Character vector. Individual outcomes for parsing and surgeon-specific summaries (default: "").
#' @param surg_col Character. Column name representing surgeons (default: "surgeon").
#' @param add_years Logical. Include year-based summaries in the output (default: FALSE).
#'
#' @return A list of dataframes summarizing the input dataset:
#' \describe{
#'   \item{date_df}{Summaries for date columns (e.g., min, max, NA percentages).}
#'   \item{binary_df}{Summaries for binary columns (e.g., ratio of TRUE, NA percentages).}
#'   \item{char_df}{Summaries for character columns (e.g., unique values, NA percentages).}
#'   \item{factor_df}{Summaries for factor columns (e.g., unique levels, NA percentages).}
#'   \item{summary_numeric}{Summaries for numeric columns (e.g., mean, quartiles, NA percentages).}
#'   \item{surg_df}{Summaries by surgeon for specified outcomes (if applicable).}
#' }
#' @export
parse_function <- function(parse_df, suffix_term = "", ind_outcomes = c(""), surg_col = "surgeon", add_years = FALSE) {
  # Ensure the dataframe is ungrouped
  parse_df <- parse_df %>% ungroup()

  # Add dummy values to ensure functions work with empty datasets
  parse_df$dummy_date <- as.Date("1000-01-01")
  parse_df$dummy_char <- ""
  parse_df$dummy_num <- -1
  parse_df$dummy_factor <- as.factor(c(0))

  # Summarize date columns
  min_date <- parse_df %>%
    summarise(across(where(~ inherits(., "Date")), ~ min(., na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "min_date")

  max_date <- parse_df %>%
    summarise(across(where(~ inherits(., "Date")), ~ max(., na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "max_date")

  perc_na_date <- parse_df %>%
    summarise(across(where(~ inherits(., "Date")), ~ mean(is.na(.), na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_date")

  date_df <- full_join(full_join(min_date, max_date, by = "field"), perc_na_date, by = "field")

  # Summarize binary columns
  ratio_binary <- parse_df %>%
    summarise(across(where(~ is.logical(.) || (is.numeric(.) && n_distinct(., na.rm = TRUE) <= 2)),
                     ~ mean(., na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "ratio_binary")

  perc_na_binary <- parse_df %>%
    summarise(across(where(~ is.logical(.) || (is.numeric(.) && n_distinct(., na.rm = TRUE) <= 2)),
                     ~ mean(is.na(.), na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_binary")

  binary_df <- full_join(ratio_binary, perc_na_binary, by = "field")

  # Summarize character columns
  values_char <- parse_df %>%
    summarise(across(where(is.character), ~ paste(unique(.), collapse = ", "))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "values_char")

  distinct_char <- parse_df %>%
    summarise(across(where(is.character), ~ n_distinct(.))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "distinct_char")

  perc_na_char <- parse_df %>%
    summarise(across(where(is.character), ~ mean(is.na(.), na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_char")

  char_df <- full_join(full_join(values_char, distinct_char, by = "field"), perc_na_char, by = "field")

  # Summarize factor columns
  levels_factor <- parse_df %>%
    summarise(across(where(is.factor), ~ paste(levels(.), collapse = ", "))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "levels_factor")

  distinct_factor <- parse_df %>%
    summarise(across(where(is.factor), ~ n_distinct(.))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "distinct_factor")

  perc_na_factor <- parse_df %>%
    summarise(across(where(is.factor), ~ mean(is.na(.), na.rm = TRUE))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_factor")

  factor_df <- full_join(full_join(levels_factor, distinct_factor, by = "field"), perc_na_factor, by = "field")

  # Summarize numeric columns
  summary_numeric <- parse_df %>%
    summarise(across(where(is.numeric), list(
      mean = ~ mean(., na.rm = TRUE),
      min = ~ min(., na.rm = TRUE),
      max = ~ max(., na.rm = TRUE),
      median = ~ median(., na.rm = TRUE),
      q25 = ~ quantile(., 0.25, na.rm = TRUE),
      q75 = ~ quantile(., 0.75, na.rm = TRUE),
      na_perc = ~ mean(is.na(.), na.rm = TRUE)
    ))) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "field", values_to = "summary_numeric")

  # Validate and summarize surgeon-specific outcomes
  if (ind_outcomes[[1]] != "") {
    if (!all(ind_outcomes %in% colnames(parse_df))) {
      stop("Some specified outcomes are missing in the dataset.")
    }
    if (!(surg_col %in% colnames(parse_df))) {
      stop("The specified surgeon column is missing in the dataset.")
    }
    surg_df <- parse_df %>%
      select(all_of(c(surg_col, ind_outcomes))) %>%
      group_by(!!sym(surg_col)) %>%
      summarise(across(all_of(ind_outcomes), mean, na.rm = TRUE)) %>%
      rename(surgeon = !!sym(surg_col))
  } else {
    surg_df <- data.frame(surgeon = character(0))
  }

  # Compile results into a list
  results <- list(
    date_df = date_df,
    binary_df = binary_df,
    char_df = char_df,
    factor_df = factor_df,
    summary_numeric = summary_numeric,
    surg_df = surg_df
  )

  return(results)
}
