#' Format text from SQL file into proper SQL query
#'
#' @param filepath Path to SQL file
#'
#' @return SQL query string
#' @export
#'
#' @examples
#'
#' # Path to the SQL file
#' filepath <- system.file("extdata", "my_sql_file.sql", package = "my_package")
#'
#' # Call the function to read the SQL file and format it into a query
#' to_sql_query(filepath)
#'
#' @export
to_sql_query <- function(filepath){
  con = file(filepath, "r")
  sql_string <- ""

  while (TRUE){
    line <- readLines(con, n = 1)

    if ( length(line) == 0 ) {
      break
    }

    if(grepl('"', line)) {
      close(con)
      stop('All double-quotes should be changed to single-quotes')
    }

    line <- gsub("\\t", " ", line)

    if(grepl("--",line) == TRUE)
      line <- paste(sub("--","/*",line),"*/")

    sql_string <- paste(sql_string, line)
  }

  close(con)
  return(sql_string)
}
