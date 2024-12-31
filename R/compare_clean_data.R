#' Compare and Analyze Clean Data
#'
#' This function compares two clean datasets, filters them based on a specified date range,
#' and generates outputs for numeric, factor, character, binary, date, and other data types.
#' The results are saved to an output directory, and optional views of the data can be displayed or saved.
#'
#' @param df_old_path Character. Path to the old dataset file (e.g., a CSV file).
#' @param df_new_path Character. Path to the new dataset file (e.g., a CSV file).
#' @param output_dir Character. Path to the directory where the output files will be saved.
#' @param final_vars_set Character vector. List of variable names to include in the comparison.
#' @param date_col Character. Name of the column in the datasets representing the date.
#' The function uses this column to filter rows based on the date range.
#' @param limit_to_same_date Logical. Whether to filter the new dataset to match the date range of the old dataset (default: TRUE).
#' @param show_views Logical. Whether to display the data views in the RStudio Viewer (default: FALSE).
#' @param save_views Logical. Whether to save views of the dataframes to disk (default: FALSE).
#'
#' @return A list of dataframes containing the comparison results. Dataframes include numeric, factor, character, binary, date,
#' and other derived datasets. Entries with missing data are omitted.
#'
#' The function also saves CSV files of the comparison results to the specified output directory.
#' If `save_views` is TRUE, the views of the dataframes are saved as additional files in the output directory.
#'
#' @export
compare_clean_data <- function(
    df_old_path,
    df_new_path,
    output_dir,
    final_vars_set,
    date_col, # New parameter for the date column name
    limit_to_same_date = TRUE,
    show_views = FALSE,
    save_views = FALSE
) {
  # Read and select specific columns
  old_data <- suppressWarnings(
    readr::read_csv(file = df_old_path, show_col_types = FALSE) |> select(all_of(final_vars_set))
  )
  new_data <- suppressWarnings(
    readr::read_csv(file = df_new_path, show_col_types = FALSE) |> select(all_of(final_vars_set))
  )

  # Ensure the date column exists in the datasets
  if (!date_col %in% names(old_data) | !date_col %in% names(new_data)) {
    stop(paste("Date column", date_col, "not found in one or both datasets."))
  }

  # Code block to limit compare to similar years
  max_filter_date <- max(old_data[[date_col]], na.rm = TRUE)
  min_filter_date <- min(old_data[[date_col]], na.rm = TRUE)

  if (limit_to_same_date) {
    new_data <- suppressWarnings(
      new_data |>
        filter(.data[[date_col]] <= max_filter_date & .data[[date_col]] >= min_filter_date)
    )
  } else {
    new_data <- suppressWarnings(
      new_data |>
        filter(.data[[date_col]] >= min_filter_date)
    )
  }

  # Run Compare -------------------------------------------------------------
  final_list <- suppressWarnings(
    tryCatch(compare_df(old_data, new_data), error = function(e) NULL)
  )

  # Gracefully handle if final_list is NULL
  if (is.null(final_list)) {
    warning("Comparison failed. Returning empty object.")
    return(list())
  }

  # Extracting dates from file paths
  extract_date <- function(path) {
    suppressWarnings(
      sub(".*secure_data/([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1", path)
    )
  }

  date1 <- extract_date(df_old_path) # Old data date
  date2 <- extract_date(df_new_path) # New data date

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Prepare object_list with error handling for missing indices
  object_list <- list(
    numeric = tryCatch(suppressWarnings(final_list[[1]] %>% select(-(matches("q|median")))), error = function(e) NULL),
    factor = tryCatch(suppressWarnings(final_list[[2]]), error = function(e) NULL),
    char = tryCatch(suppressWarnings(final_list[[3]]), error = function(e) NULL),
    bin = tryCatch(suppressWarnings(final_list[[4]]), error = function(e) NULL),
    date = tryCatch(suppressWarnings(final_list[[5]]), error = function(e) NULL),
    surg = tryCatch(suppressWarnings(final_list[[6]]), error = function(e) NULL),
    by_year_mean = tryCatch(suppressWarnings(final_list[[7]]), error = function(e) NULL),
    by_year_na = tryCatch(suppressWarnings(final_list[[8]]), error = function(e) NULL)
  )

  purrr::iwalk(object_list, function(data, name) {
    if (is.null(data)) {
      message(paste("Skipping:", name, "due to missing data."))
    } else {
      tryCatch(
        {
          output_file <- file.path(output_dir, paste0("results_", name, "_", date1, "_", date2, ".csv"))
          suppressWarnings(utils::write.csv(data, file = output_file, row.names = FALSE))
          message(paste("Saved:", output_file))
        },
        error = function(e) {
          warning(paste("Skipping results for", name, "due to error:", e$message))
        }
      )
    }
  })

  # Save views if requested
  if (save_views) {
    purrr::iwalk(object_list, function(data, name) {
      if (!is.null(data)) {
        view_file <- file.path(output_dir, paste0("view_", name, "_", date1, "_", date2, ".csv"))
        suppressWarnings(utils::write.csv(data, file = view_file, row.names = FALSE))
        message(paste("View saved:", view_file))
      }
    })
  }

  message("Processing complete. Files saved to:", output_dir, " - Date range: ", date1, " to ", date2)

  # Display views if requested
  if (show_views) {
    purrr::iwalk(object_list, function(data, name) {
      if (!is.null(data)) {
        suppressWarnings(utils::View(data, title = paste0("View - ", name)))
      }
    })
  } else {
    message("No views displayed as show_views = FALSE")
  }

  # Return only non-null dataframes
  return(purrr::compact(object_list))
}
