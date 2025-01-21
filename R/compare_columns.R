#' Compare and Analyze Two Data Frames
#'
#' @param df1 A data frame or tibble
#' @param df2 A data frame or tibble
#' @param df1_name Character string naming the first data frame (default: "df1")
#' @param df2_name Character string naming the second data frame (default: "df2")
#' @param group_by_col Character string specifying column name for grouping (default: NULL)
#'
#' @return A list containing comparison results and filtered dataframes
#' @importFrom gt gt tab_header cols_label fmt_number tab_style cell_borders px cells_body
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
compare_columns <- function(df1, df2, df1_name = "df1", df2_name = "df2", group_by_col = NULL) {
  # Input validation
  if (!is.data.frame(df1) || !is.data.frame(df2)) {
    stop("Both inputs must be data frames or tibbles")
  }
  
  if (!is.character(df1_name) || !is.character(df2_name)) {
    stop("Data frame names must be character strings")
  }
  
  # Calculate comparisons
  cols_unique_to_df1 <- setdiff(names(df1), names(df2))
  cols_unique_to_df2 <- setdiff(names(df2), names(df1))
  mutual_cols <- intersect(names(df1), names(df2))
  
  # Create filtered dataframes
  df1_filtered <- df1[, cols_unique_to_df1, drop = FALSE]
  df2_filtered <- df2[, cols_unique_to_df2, drop = FALSE]
  
  # Create summary dataframe for gt
  summary_df <- data.frame(
    Metric = c("Total Columns", "Unique Columns", "Total Rows", "Unique Rows", "Mutual Columns"),
    df1_value = c(
      ncol(df1),
      length(cols_unique_to_df1),
      nrow(df1),
      nrow(unique(df1)),
      length(mutual_cols)
    ),
    df2_value = c(
      ncol(df2),
      length(cols_unique_to_df2),
      nrow(df2),
      nrow(unique(df2)),
      NA  # NA for mutual columns in df2
    )
  )
  
  # Create columns dataframe for gt
  cols_df <- data.frame(
    Category = c(
      paste("Unique to", df1_name),
      paste("Unique to", df2_name),
      "Mutual Columns"
    ),
    Columns = c(
      paste(cols_unique_to_df1, collapse = ", "),
      paste(cols_unique_to_df2, collapse = ", "),
      paste(mutual_cols, collapse = ", ")
    )
  )
  
  # Create and print summary table
  summary_table <- summary_df %>%
    gt() %>%
    tab_header(
      title = "DataFrame Comparison Summary"
    ) %>%
    cols_label(
      Metric = "Metric",
      df1_value = df1_name,
      df2_value = df2_name
    ) %>%
    fmt_number(
      columns = c(.data$df1_value, .data$df2_value),
      decimals = 0
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "gray85",
        weight = px(1)
      ),
      locations = cells_body()
    )
  
  # Create and print columns table
  cols_table <- cols_df %>%
    gt() %>%
    tab_header(
      title = "Column Details"
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "gray85",
        weight = px(1)
      ),
      locations = cells_body()
    )
  
  # Print tables
  print(summary_table)
  print(cols_table)
  
  # Store results
  summary_data <- list(
    unique_cols = list(
      df1 = cols_unique_to_df1,
      df2 = cols_unique_to_df2
    ),
    total_cols = list(
      df1 = ncol(df1),
      df2 = ncol(df2)
    ),
    mutual_cols = mutual_cols,
    row_counts = list(
      df1 = nrow(df1),
      df2 = nrow(df2)
    ),
    unique_rows = list(
      df1 = nrow(unique(df1)),
      df2 = nrow(unique(df2))
    )
  )
  
  # Return list with all data
  results <- list(
    filtered_dfs = list(
      df1 = df1_filtered,
      df2 = df2_filtered
    ),
    summary_data = summary_data,
    tables = list(
      summary_table = summary_table,
      cols_table = cols_table
    )
  )
  
  # Add names for easier access
  names(results$filtered_dfs) <- c(df1_name, df2_name)
  names(results$summary_data$unique_cols) <- c(df1_name, df2_name)
  names(results$summary_data$total_cols) <- c(df1_name, df2_name)
  names(results$summary_data$row_counts) <- c(df1_name, df2_name)
  names(results$summary_data$unique_rows) <- c(df1_name, df2_name)
  
  return(invisible(results))
}