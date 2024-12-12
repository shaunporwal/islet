#' Read and Process an SQL File
#'
#' Reads an SQL file line by line, replaces tabs with spaces, and converts
#' single-line SQL comments (`--`) into C-style comments (`/* ... */`).
#'
#' @param filepath A string representing the path to the SQL file.
#' @return A single string containing the processed SQL code.
#' @examples
#' # Example usage:
#' # sql_code <- get_sql("path/to/sql_file.sql")
#' @export
get_sql <- function(filepath) {
  con <- file(filepath, "r")
  sql.string <- ""

  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)

    if (length(line) == 0)
      break

    line <- gsub("\\t", " ", line)

    if (grepl("--", line))
      line <- paste(sub("--", "/*", line), "*/")

    sql.string <- paste(sql.string, line)
  }

  close(con)
  return(sql.string)
}
