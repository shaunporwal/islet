

line_log <- function(type,
                     item,
                     editor_context = get_current_file(),
                     data_path = file.path(getwd(), "secure_data", PROJECT_DICT$data_date)) {

  type <- stringr::str_to_upper(type)

  time <- trunc(as.numeric(format(Sys.time(), "%H%M%OS3")))

  if (type == "COMMENT") {

    saveRDS(list(log = item,
                 file = editor_context,
                 time = time),
            glue::glue("{data_path}/Metadata/Line Logs/Comments/{type}_{time}.rds"))

  } else if (type == "TODO") {

    saveRDS(list(log = item,
                 file = editor_context,
                 time = time),
            glue::glue("{data_path}/Metadata/Line Logs/Todos/{type}_{time}.rds"))

  }

}
