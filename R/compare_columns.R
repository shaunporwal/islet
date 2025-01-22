#' Compare Column Names Between Two Objects
#'
#' @description
#' Compares column names between two objects (data frames or vectors of column names)
#' and provides a detailed summary of unique and shared columns
#'
#' @param obj1 A data frame, tibble, or vector of column names
#' @param obj2 A data frame, tibble, or vector of column names
#' @param obj1_name Character string naming the first object (default: "obj1")
#' @param obj2_name Character string naming the second object (default: "obj2")
#' @param group_by_col Character string specifying column name for grouping (default: NULL)
#'
#' @return A list containing:
#' \itemize{
#'   \item filtered_objects: List of objects containing unique columns
#'   \item summary_data: List of comparison statistics
#'   \item tables: GT tables showing comparison results
#' }
#' @importFrom gt gt tab_header cols_label fmt_number tab_style cell_borders px cells_body
#' @importFrom rlang .data
#' @importFrom rlang %||%
#' @export
#'
#' @examples
#' # Compare column names of two data frames
#' compare_columns(mtcars, iris)
#'
#' # Compare vectors of column names
#' compare_columns(names(mtcars), names(iris))
compare_columns <- function(obj1, obj2, obj1_name = "obj1", obj2_name = "obj2", group_by_col = NULL) {
  # Validate inputs first
  # Update validation at start of function
  if (!is.data.frame(obj1) && (!is.vector(obj1) || is.list(obj1))) {
    stop("obj1 must be a data frame, tibble, or vector of column names")
  }
  if (!is.data.frame(obj2) && (!is.vector(obj2) || is.list(obj2))) {
    stop("obj2 must be a data frame, tibble, or vector of column names")
  }

  # Special handling for vectors of names
  if (is.vector(obj1) && !is.data.frame(obj1)) {
    obj1_names <- obj1
    obj1 <- data.frame(matrix(ncol = length(obj1)))
    names(obj1) <- obj1_names
  }
  if (is.vector(obj2) && !is.data.frame(obj2)) {
    obj2_names <- obj2
    obj2 <- data.frame(matrix(ncol = length(obj2)))
    names(obj2) <- obj2_names
  }

  if (!is.character(obj1_name) || !is.character(obj2_name)) {
    stop("Object names must be character strings")
  }

  # Calculate comparisons
  cols_unique_to_obj1 <- setdiff(names(obj1), names(obj2))
  cols_unique_to_obj2 <- setdiff(names(obj2), names(obj1))
  mutual_cols <- intersect(names(obj1), names(obj2))

  # Create filtered objects
  obj1_filtered <- obj1[, cols_unique_to_obj1, drop = FALSE]
  obj2_filtered <- obj2[, cols_unique_to_obj2, drop = FALSE]

  # Create summary dataframe for gt
  summary_df <- data.frame(
    Metric = c("Total Columns", "Unique Columns", "Total Rows", "Unique Rows", "Mutual Columns"),
    obj1_value = c(
      ncol(obj1),
      length(cols_unique_to_obj1),
      nrow(obj1),
      nrow(unique(obj1)),
      length(mutual_cols)
    ),
    obj2_value = c(
      ncol(obj2),
      length(cols_unique_to_obj2),
      nrow(obj2),
      nrow(unique(obj2)),
      NA # NA for mutual columns in obj2
    )
  )

  # Create columns dataframe for gt
  cols_df <- data.frame(
    Category = c(
      paste("Unique to", obj1_name),
      paste("Unique to", obj2_name),
      "Mutual Columns"
    ),
    Columns = c(
      paste(cols_unique_to_obj1, collapse = ", "),
      paste(cols_unique_to_obj2, collapse = ", "),
      paste(mutual_cols, collapse = ", ")
    )
  )

  # Create and print summary table
  summary_table <- summary_df |>
    gt() |>
    tab_header(
      title = "Object Comparison Summary"
    ) |>
    cols_label(
      Metric = "Metric",
      obj1_value = obj1_name,
      obj2_value = obj2_name
    ) |>
    fmt_number(
      columns = c(.data$obj1_value, .data$obj2_value),
      decimals = 0
    ) |>
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "gray85",
        weight = px(1)
      ),
      locations = cells_body()
    )

  # Create and print columns table
  cols_table <- cols_df |>
    gt() |>
    tab_header(
      title = "Column Details"
    ) |>
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
      obj1 = cols_unique_to_obj1,
      obj2 = cols_unique_to_obj2
    ),
    total_cols = list(
      obj1 = ncol(obj1),
      obj2 = ncol(obj2)
    ),
    mutual_cols = mutual_cols,
    row_counts = list(
      obj1 = nrow(obj1),
      obj2 = nrow(obj2)
    ),
    unique_rows = list(
      obj1 = nrow(unique(obj1)),
      obj2 = nrow(unique(obj2))
    )
  )

  # Return list with all data
  results <- list(
    filtered_objects = list(
      obj1 = obj1_filtered,
      obj2 = obj2_filtered
    ),
    summary_data = summary_data,
    tables = list(
      summary_table = summary_table,
      cols_table = cols_table
    )
  )

  # Add names for easier access
  names(results$filtered_objects) <- c(obj1_name, obj2_name)
  names(results$summary_data$unique_cols) <- c(obj1_name, obj2_name)
  names(results$summary_data$total_cols) <- c(obj1_name, obj2_name)
  names(results$summary_data$row_counts) <- c(obj1_name, obj2_name)
  names(results$summary_data$unique_rows) <- c(obj1_name, obj2_name)

  return(invisible(results))
}
