#' Build paths to data directories based on a data date file
#'
#' Creates file paths to versioned data directories based on the contents of a data_date.txt file.
#' Supports both here-style paths (`here::here()`) and explicit paths (`fs::path()`).
#'
#' @param ... Additional path components to append
#' @param style Path style: "here" or "path"
#' @param path Root path (required when style="path")
#' @param data_folder_name Data folder name (default: "secure_data")
#' @param path_to_data_date Path to data_date.txt file or its directory
#' @param return_date_only If TRUE, only returns the date string
#'
#' @return Path string or date string (if return_date_only=TRUE)
#' @export
here_data <- function(...,
                      style = c("here", "path"),
                      path = getOption("path_data"),
                      data_folder_name = "secure_data",
                      path_to_data_date = here::here(),
                      return_date_only = FALSE) {
  # Match the style argument
  style <- match.arg(style)

  # Get the data date
  # First looking for data_date file
  if (fs::is_dir(path_to_data_date)) {
    potential_filenames <- c("data_date.txt", "data_date", "dataDate.txt", "dataDate")
    data_date_file_idx <- which(purrr::map_lgl(
      potential_filenames,
      ~ fs::is_file(file.path(path_to_data_date, .x))
    ))

    # Error if no file found
    if (length(data_date_file_idx) == 0) {
      stop(
        paste(
          "No text file containing the data date could be found. Expecting one of",
          paste(shQuote(potential_filenames, type = "csh"), collapse = ", "),
          "in the project's root directory."
        ) %>%
          stringr::str_wrap(),
        call. = FALSE
      )
    }

    data_date_file <- potential_filenames[data_date_file_idx[1]]
    path_to_data_date <- file.path(path_to_data_date, data_date_file)
  }

  # Importing the data date
  data_date_lines <- tryCatch(
    readr::read_lines(path_to_data_date, skip_empty_rows = TRUE),
    error = function(e) {
      cli::cli_alert_danger("There was an error importing data date from file {.path {path_to_data_date}}")
      stop(as.character(e))
    }
  )

  if (rlang::is_empty(data_date_lines)) {
    stop("The data date file is empty.",
      call. = FALSE
    )
  }

  # Take only the first line if multiple lines are present
  data_date <- data_date_lines[1]

  # Basic validation that the date is in a reasonable format
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d{8}$", data_date)) {
    warning("The data date doesn't appear to be in a standard date format (YYYY-MM-DD or YYYYMMDD)")
  }

  # If user only wants the date, return it now
  if (return_date_only) {
    return(data_date)
  }

  # Build and return the path based on style
  if (style == "here") {
    return(here::here(data_folder_name, data_date, ...))
  } else {
    if (is.null(path)) {
      stop("path argument is NULL when style='path'", call. = FALSE)
    }
    return(fs::path(path, data_folder_name, data_date, ...))
  }
}

#' @rdname here_data
#' @export
get_data_date <- function(path_to_data_date = here::here()) {
  # Define . to avoid NOTE about undefined global variable with magrittr pipe
  . <- NULL

  # first looking for data_date file -------------------------------------------
  if (fs::is_dir(path_to_data_date)) {
    potential_filenames <- c("data_date.txt", "data_date", "dataDate.txt", "dataDate")
    data_date_file <-
      purrr::map_lgl(potential_filenames, ~ fs::is_file(file.path(path_to_data_date, .x))) %>% # Use purrr::map_lgl
      which() %>%
      {
        purrr::pluck(potential_filenames, .[1])
      } # Use purrr::pluck

    # error if no file found ---------------------------------------------------
    if (is.null(data_date_file)) {
      paste(
        "No text file containing the data date could be found. Expecting one of",
        paste(shQuote(potential_filenames, type = "csh"), collapse = ", "),
        "in the project's root directory."
      ) %>%
        stringr::str_wrap() %>% # Use stringr::str_wrap
        stop(call. = FALSE)
    }

    path_to_data_date <- file.path(path_to_data_date, data_date_file)
  }

  # importing the data date ----------------------------------------------------
  data_date_lines <- # Renamed from data_date to avoid clash before line selection
    tryCatch(
      readr::read_lines(path_to_data_date, skip_empty_rows = TRUE), # Use readr::read_lines
      error = function(e) {
        cli::cli_alert_danger("There was an error importing data date from file {.path {path_to_data_date}}") # Use cli::cli_alert_danger
        stop(as.character(e))
      }
    )

  if (rlang::is_empty(data_date_lines)) { # Check the lines read
    stop("The data date file is empty.", # Updated error message
      call. = FALSE
    )
  }

  data_date <- data_date_lines[1] # Take only the first line

  # Basic validation that the date is in a reasonable format (optional, but good practice)
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d{8}$", data_date)) {
    warning("The data date doesn't appear to be in a standard date format (YYYY-MM-DD or YYYYMMDD)")
  }

  data_date
}
