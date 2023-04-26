#' A function to check parsed values from their raw data sources
#'
#' @param data_df A dataframe that includes the raw and parsed variables
#' @param raw A string or vector of strings that contain raw variable names
#' @param parsed A string or vector of strings containing the name of parsed variables
#' @param type Either "map" or "cont" to specify the type of checking to be performed
#' @param eval_func A function call for the type of check that should be performed on "cont" parsed variables.
#'
#' @export
#'

check_values <- function(data_df,
                         raw,
                         parsed,
                         type,
                         eval_func = as.numeric) {

  if (type == "map") {

    if (!is.null(parsed)) {

      map_df <- data_df %>%
        # dplyr::group_by(.data[[parsed]], # TODO: does this work if there are multiple values?
        #                 .data[[raw]]) %>%
        dplyr::group_by_at(c(parsed, raw)) %>%
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
        dplyr::group_by_at(raw) %>%
        dplyr::count()

      # 6. Test for cat message in console

      cat("\nInspect raw values:\n\n")

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

    if (is.null(parsed)) {

      num_df <- data_df %>%
        dplyr::ungroup() %>%
        dplyr::select(.data[[raw]]) %>%
        dplyr::mutate(naturally_parsed = suppressWarnings(eval_func(.data[[raw]]))) %>%
        dplyr::filter(is.na(naturally_parsed)) %>%
        dplyr::count(.data[[raw]]) %>%
        dplyr::mutate(percent = scales::percent(n/nrow(data_df)))

      if (nrow(num_df) > 0) {

        # 9. Test for cat message
        cat("\nNaturally non-parsing values:\n\n")

        # 10. Test for df in console
        print(num_df %>%
                as.data.frame(),
              row.names = FALSE)
      }

      to_summary <- data_df %>%
        dplyr::ungroup() %>%
        dplyr::select(.data[[raw]]) %>%
        dplyr::mutate(naturally_parsed = suppressWarnings(eval_func(.data[[raw]]))) %>%
        dplyr::pull(naturally_parsed)

      if (length(to_summary) > 0) {

        # 11. Test for cat message in console
        cat("\n\nSummary of naturally parsed values:\n\n")

        # 12. Test for summary printed in console
        print(summary(to_summary))

      }

    } else {

      num_df <- data_df %>%
        dplyr::ungroup() %>%
        dplyr::select(.data[[parsed]],
                      .data[[raw]]) %>%
        dplyr::mutate(naturally_parsed = suppressWarnings(eval_func(.data[[raw]]))) %>%
        dplyr::filter(is.na(naturally_parsed)) %>%
        dplyr::count(.data[[parsed]], # TODO: does this work if there are multiple values?
                     .data[[raw]])  %>%
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
        dplyr::pull(parsed)

      if (length(to_summary) > 0) {

        # 17. Test for cat message in console
        cat("\n\nSummary of currently parsed values:\n\n")
        # 18. Test for printed summary in console
        print(summary(to_summary))
      }
    }
    return(invisible(num_df))
  }
}
