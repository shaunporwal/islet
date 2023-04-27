#' Log Information About Comments and TODOs
#'
#' This function logs information about comments and TODOs added in a file, storing metadata
#' in a specified data path.
#'
#' @param type A character string representing the type of annotation, either "COMMENT" or "TODO".
#' @param item A character string or object containing the comment or TODO line(s).
#' @param current_file A character string representing the name of the current file being edited. Default is the current file.
#' @param data_path A character string representing the path to the directory where log files should be saved. Default is a "secure_data" subfolder in the working directory.
#'
#' @return NULL. This function is called for its side effect of saving log files.
#' @export
#'
#' @examples
#' # Log a comment
#' line_log("COMMENT", "This is a comment.")
#'
#' # Log a TODO with a custom current_file and data_path
#' line_log("TODO", "This is a TODO.", "my_file.R", "custom_data_path")
line_log <- function(type,
                     item,
                     current_file = get_current_file(),
                     data_path = file.path(getwd(), "secure_data", PROJECT_DICT$data_date)) {

  type <- stringr::str_to_upper(type)

  acceptable_types = c('COMMENT', 'TODO')

  if (!(type %in% acceptable_types)){
    stop('Unacceptable type, has to be either COMMENT or TODO', call. = TRUE)
  }

  time <- trunc(as.numeric(format(Sys.time(), "%H%M%OS3")))

  saveRDS(list(log = item,
               file = current_file,
               time = time),
          glue::glue("{data_path}/Metadata/Line Logs/{type}_{time}.rds"))

  saveRDS(list(log = item,
               file = current_file,
               time = time),
          glue::glue("{data_path}/Metadata/Line Logs/{type}_{time}.rds"))
}
