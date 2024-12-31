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
#'
#' @examples
#' # Load example data
#' my_data <- mtcars
#'
#' # Add a simulated surgeon column and a date column
#' set.seed(123) # Ensure reproducibility
#' my_data$surgeon <- sample(letters[1:5], nrow(my_data), replace = TRUE)
#' my_data$date <- as.Date("2024-01-01") + sample(0:100, nrow(my_data), replace = TRUE)
#' my_data$outcome1 <- sample(0:1, nrow(my_data), replace = TRUE)
#'
#' # Parse the dataframe without year summaries
#' results <- parse_function(parse_df = my_data)
#'
#' # Parse the dataframe with year-based summaries
#' results_with_years <- parse_function(parse_df = my_data, add_years = TRUE)
#'
#' # Parse the dataframe with specific outcomes
#' results_with_outcomes <- parse_function(parse_df = my_data, ind_outcomes = c("outcome1"))

parse_function <- function(parse_df, suffix_term = "", ind_outcomes = c(""), surg_col = "surgeon", add_years = FALSE) {
  library(dplyr)
  library(tidyr)

  # Ensure ungrouped dataframe
  parse_df <- parse_df %>% ungroup()

  # Add dummy values to ensure functions work with empty datasets
  parse_df$dummy_date <- as.Date("1000-01-01")
  parse_df$dummy_char <- ""
  parse_df$dummy_num <- -1
  parse_df$dummy_factor <- as.factor(c(0))

  # Summarize date columns
  min_date <- parse_df %>%
    summarise(across(where(~ inherits(., "Date")), ~ min(., na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "field", values_to = "min_date")

  max_date <- parse_df %>%
    summarise(across(where(~ inherits(., "Date")), ~ max(., na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "field", values_to = "max_date")

  perc_na_date <- parse_df %>%
    summarise(across(where(~ inherits(., "Date")), ~ mean(is.na(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_date")

  date_df <- reduce(list(min_date, max_date, perc_na_date), full_join, by = "field")

  # Summarize binary columns
  ratio_binary <- parse_df %>%
    summarise(across(where(~ is.logical(.) || (is.numeric(.) && n_distinct(., na.rm = TRUE) <= 2)),
                     ~ mean(., na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "field", values_to = "ratio_binary")

  perc_na_binary <- parse_df %>%
    summarise(across(where(~ is.logical(.) || (is.numeric(.) && n_distinct(., na.rm = TRUE) <= 2)),
                     ~ mean(is.na(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_binary")

  binary_df <- full_join(ratio_binary, perc_na_binary, by = "field")

  # Summarize character columns
  values_char <- parse_df %>%
    summarise(across(where(is.character), ~ paste(unique(.), collapse = ", "))) %>%
    pivot_longer(cols = everything(), names_to = "field", values_to = "values_char")

  distinct_char <- parse_df %>%
    summarise(across(where(is.character), ~ n_distinct(.))) %>%
    pivot_longer(cols = everything(), names_to = "field", values_to = "distinct_char")

  perc_na_char <- parse_df %>%
    summarise(across(where(is.character), ~ mean(is.na(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_char")

  char_df <- reduce(list(values_char, distinct_char, perc_na_char), full_join, by = "field")

  # Summarize factor columns
  levels_factor <- parse_df %>%
    summarise(across(where(is.factor), ~ paste(levels(.), collapse = ", "))) %>%
    pivot_longer(cols = everything(), names_to = "field", values_to = "levels_factor")

  distinct_factor <- parse_df %>%
    summarise(across(where(is.factor), ~ n_distinct(.))) %>%
    pivot_longer(cols = everything(), names_to = "field", values_to = "distinct_factor")

  perc_na_factor <- parse_df %>%
    summarise(across(where(is.factor), ~ mean(is.na(.), na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "field", values_to = "perc_na_factor")

  factor_df <- reduce(list(levels_factor, distinct_factor, perc_na_factor), full_join, by = "field")

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
    pivot_longer(cols = everything(), names_to = "field", values_to = "summary_numeric")

  # Handle outcomes for surgeons
  if (length(ind_outcomes) > 0) {
    if (!all(ind_outcomes %in% names(parse_df))) {
      stop("Specified outcomes are missing in the dataset.")
    }
    surg_df <- parse_df %>%
      group_by(!!sym(surg_col)) %>%
      summarise(across(all_of(ind_outcomes), mean, na.rm = TRUE)) %>%
      rename(surgeon = !!sym(surg_col))
  } else {
    surg_df <- NULL
  }

  # Return a list of summaries
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
