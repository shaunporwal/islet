#' A function to check to values from their from data sources
#'
#' @param data_df A dataframe that includes the from and to variables
#' @param from A string or vector of strings that contain from variable names
#' @param to A string or vector of strings containing the name of to variables
#' @param type Either "map" or "cont" to specify the type of checking to be performed
#' @param eval_func A function call for the type of check that should be performed on "cont" to variables.
#'
#' @export
#'

check_values <- function(data_df,
                         from,
                         to = NULL,
                         type,
                         eval_func = as.numeric) {

  if (type == "map") {

    if (!is.null(to)) {

      map_df <- data_df %>%
        # dplyr::group_by(.data[[to]], # TODO: does this work if there are multiple values?
        #                 .data[[from]]) %>%
        dplyr::group_by_at(c(to, from)) %>%
        dplyr::count()

      # 1. Test for 'cat' message in consolve
      cat("\nInspect mapping:\n\n")

      # 2. Test for print message in console
      print("map_df: ")
      print(map_df %>%
              as.data.frame(),
            row.names = FALSE)

      cat("\n\n")

      # 5. Test for returned map_df
      return(invisible(map_df))

    } else {

      map_df <- data_df %>%
        dplyr::group_by_at(from) %>%
        dplyr::count()

      # 6. Test for cat message in console

      cat("\nInspect from values:\n\n")

      # 7. Test for printed df in console
      print(map_df %>%
              as.data.frame(),
            row.names = FALSE)

      cat("\n\n")
      # 8. Test for returned map_df
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

        # 9. Test for cat message
        cat("\nNaturally non-parsing from values:\n\n")

        # 10. Test for df in console
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

        # 11. Test for cat message in console
        cat("\n\nSummary of from values:\n\n")

        # 12. Test for summary printed in console
        print(summary(to_summary))

      }

    } else {

      num_df <- data_df %>%
        dplyr::ungroup() %>%
        dplyr::select(.data[[to]],
                      .data[[from]]) %>%
        dplyr::mutate(naturally_to = suppressWarnings(eval_func(.data[[from]]))) %>%
        dplyr::filter(is.na(naturally_to)) %>%
        dplyr::count(.data[[to]], # TODO: does this work if there are multiple values?
                     .data[[from]])  %>%
        dplyr::mutate(percent = scales::percent(n/nrow(data_df)))


      if (nrow(num_df) > 0) {

        # 15. Test for cat message in console
        cat("\nNaturally non-parsing values:\n\n")
        # 16. Test for printed df in console
        print(num_df %>%
                as.data.frame(),
              row.names = FALSE)
      }

      to_summary <- data_df %>%
        dplyr::ungroup() %>%
        dplyr::pull(to)

      if (length(to_summary) > 0) {

        # 17. Test for cat message in console
        cat("\n\nSummary of currently to values:\n\n")
        # 18. Test for printed summary in console
        print(summary(to_summary))
      }
    }
    return(invisible(num_df))
  }
}
