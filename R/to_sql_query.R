#' Format text from SQL file into proper SQL query
#'
#' @param filepath Path to SQL file
#'
#' @return SQL query string
#' @export
#'
#' @examples Coming Soon!
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
