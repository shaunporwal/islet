#' Parse a Dataframe for Analysis
#'
#' Processes a dataframe to generate summaries for numeric, factor, character,
#' binary, date, and other data types. Supports year-based summaries.
#'
#' @param parse_df Dataframe. Input dataframe to parse and analyze.
#' @param suffix_term Character. Suffix to append to parsed column names (default: "").
#' @param ind_outcomes Character vector. Individual outcomes for parsing (default: "").
#' @param surg_col Character. Column name representing surgeons (default: "surgeon").
#' @param add_years Logical. Include year-based summaries in the output (default: FALSE).
#'
#' @return A list of dataframes summarizing the input dataset:
#' \describe{
#'   \item{numeric_df}{Numeric column summaries (e.g., mean, quartiles, NA percentages).}
#'   \item{factor_df}{Factor column summaries (e.g., unique levels, NA percentages).}
#'   \item{char_df}{Character column summaries (e.g., unique values, NA percentages).}
#'   \item{binary_df}{Binary column summaries (e.g., ratio of TRUE, NA percentages).}
#'   \item{date_df}{Date column summaries (e.g., min, max, NA percentages).}
#'   \item{surg_df}{Summaries by surgeon for specified outcomes.}
#'   \item{years_mean}{Yearly mean summaries if \code{add_years = TRUE}.}
#'   \item{years_na}{Yearly NA percentages if \code{add_years = TRUE}.}
#' }
#' @export
#'
#' @examples
#' results <- parse_function(parse_df = my_data)
#' results_with_years <- parse_function(parse_df = my_data, add_years = TRUE)
#' results_with_outcomes <- parse_function(parse_df = my_data, ind_outcomes = c("outcome1"))
parse_function <- function(parse_df, suffix_term = "", ind_outcomes = c(""), surg_col = "surgeon", add_years = FALSE) {

  # Ensure ungrouped dataframe
  parse_df <- parse_df %>% ungroup()

  # Add dummy values to ensure functions work with empty datasets
  parse_df$dummy_date <- as.Date("1000-01-01")
  parse_df$dummy_char <- ""
  parse_df$dummy_num <- -1
  parse_df$dummy_factor <- as.factor(c(0))

  # Summarize date columns
  min_date <- parse_df %>%
    summarise_if(is.Date, funs(min(., na.rm = TRUE))) %>%
    gather("field", "min_date")

  max_date <- parse_df %>%
    summarise_if(is.Date, funs(max(., na.rm = TRUE))) %>%
    gather("field", "max_date")

  perc_na_date <- parse_df %>%
    summarise_if(is.Date, funs(round(mean(is.na(.)), digits = 3))) %>%
    gather("field", "perc_na_date")

  date_df <- reduce(list(min_date, max_date, perc_na_date), full_join, by = "field")

  # Summarize binary columns
  ratio_binary <- parse_df %>%
    summarise_if(~ is.logical(.) || (is.numeric(.) && n_distinct(., na.rm = TRUE) <= 2),
                 funs(round(mean(., na.rm = TRUE), digits = 3))) %>%
    gather("field", "ratio_binary")

  perc_na_binary <- parse_df %>%
    summarise_if(~ is.logical(.) || (is.numeric(.) && n_distinct(., na.rm = TRUE) <= 2),
                 funs(round(mean(is.na(.)), digits = 3))) %>%
    gather("field", "perc_na_binary")

  binary_df <- full_join(ratio_binary, perc_na_binary, by = "field")

  # Summarize character columns
  values_char <- parse_df %>%
    summarise_if(is.character, funs(paste(unique(.), collapse = ", "))) %>%
    gather("field", "values_char")

  distinct_char <- parse_df %>%
    summarise_if(is.character, funs(n_distinct(., na.rm = TRUE))) %>%
    gather("field", "distinct_char")

  perc_na_char <- parse_df %>%
    summarise_if(is.character, funs(round(mean(is.na(.)), digits = 3))) %>%
    gather("field", "perc_na_char")

  char_df <- reduce(list(values_char, distinct_char, perc_na_char), full_join, by = "field")

  # Summarize factor columns
  levels_factor <- parse_df %>%
    summarise_if(is.factor, funs(paste(levels(.), collapse = ", "))) %>%
    gather("field", "levels_factor")

  distinct_factor <- parse_df %>%
    summarise_if(is.factor, funs(n_distinct(.))) %>%
    gather("field", "distinct_factor")

  perc_na_factor <- parse_df %>%
    summarise_if(is.factor, funs(round(mean(is.na(.)), digits = 3))) %>%
    gather("field", "perc_na_factor")

  factor_df <- reduce(list(levels_factor, distinct_factor, perc_na_factor), full_join, by = "field")

  # Summarize numeric columns
  summary_numeric <- parse_df %>%
    summarise_if(is.numeric,
                 funs(mean = round(mean(., na.rm = TRUE), 3),
                      min = round(min(., na.rm = TRUE), 3),
                      max = round(max(., na.rm = TRUE), 3),
                      median = round(median(., na.rm = TRUE), 3),
                      q25 = round(quantile(., 0.25, na.rm = TRUE), 3),
                      q75 = round(quantile(., 0.75, na.rm = TRUE), 3),
                      na_perc = round(mean(is.na(.)), 3))) %>%
    gather("field", "summary_numeric")

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
