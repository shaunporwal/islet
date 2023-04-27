#' A function to check to values from their from data sources
#'
#' @description This function checks and inspects the mapping and values of specified columns
#' in a dataframe, comparing 'from' and 'to' columns if provided.
#'
#' @param data_df A dataframe that includes the from and to variables.
#' @param from A string or vector of strings that contain from variable names.
#' @param to A string or vector of strings containing the name of to variables (default is NULL).
#' @param type Either "map" or "cont" to specify the type of checking to be performed.
#' @param eval_func A function call for the type of check that should be performed on "cont" to variables (default is as.numeric).
#'
#' @return An invisible dataframe with the results of the check.
#' @export
#'
#' @examples
#' # Assuming you have a DataFrame called 'my_data' with 'from_col' and 'to_col' columns
#' check_values(
#'   data_df = my_data,
#'   from = "from_col",
#'   to = "to_col",
#'   type = "map"
#' )
check_values <- function(data_df,
                         from,
                         to = NULL,
                         type,
                         eval_func = as.numeric) {

  if (type == "map") {

    if (!is.null(to)) {

      map_df <- data_df %>%
        dplyr::group_by_at(c(to, from)) %>%
        dplyr::count()

      cat("\nInspect mapping:\n\n")

      print("map_df: ")
      print(map_df %>%
              as.data.frame(),
            row.names = FALSE)

      cat("\n\n")

      return(invisible(map_df))

    } else {

      map_df <- data_df %>%
        dplyr::group_by_at(from) %>%
        dplyr::count()

      cat("\nInspect from values:\n\n")

      print(map_df %>%
              as.data.frame(),
            row.names = FALSE)

      cat("\n\n")

      return(invisible(map_df))
    }
  }

  else if (type == "cont") {

    if (is.null(to)) {

      num_df <- data_df %>%
        dplyr::ungroup() %>%
        dplyr::select(.data[[from]]) %>%
        dplyr::mutate(naturally_to = suppressWarnings(eval_func(.data[[from]]))) %>%
        dplyr::filter(is.na(naturally_to)) %>%
        dplyr::count(.data[[from]]) %>%
        dplyr::mutate(percent = scales::percent(n/nrow(data_df)))

      if (nrow(num_df) > 0) {

        cat("\nNaturally non-parsing from values:\n\n")

        print(num_df %>%
                as.data.frame(),
              row.names = FALSE)
      }

      to_summary <- data_df %>%
        dplyr::ungroup() %>%
        dplyr::select(.data[[from]]) %>%
        dplyr::mutate(naturally_to = suppressWarnings(eval_func(.data[[from]]))) %>%
        dplyr::pull(naturally_to)

      if (length(to_summary) > 0) {

        cat("\n\nSummary of from values:\n\n")

        print(summary(to_summary))

      }

    } else {

      num_df <- data_df %>%
        dplyr::ungroup() %>%
        dplyr::select(.data[[to]],
                      .data[[from]]) %>%
        dplyr::mutate(naturally_to = suppressWarnings(eval_func(.data[[from]]))) %>%
        dplyr::filter(is.na(naturally_to)) %>%
        dplyr::count(.data[[to]],
                     .data[[from]])  %>%
        dplyr::mutate(percent = scales::percent(n/nrow(data_df)))

      if (nrow(num_df) > 0) {

        cat("\nNaturally non-parsing values:\n\n")
        print(num_df %>%
                as.data.frame(),
              row.names = FALSE)
      }

      to_summary <- data_df %>%
        dplyr::ungroup() %>%
        dplyr::pull(to)

      if (length(to_summary) > 0) {

        cat("\n\nSummary of currently to values:\n\n")
        print(summary(to_summary))
      }
    }
    return(invisible(num_df))
  }
}
