#' Create a Banner for Code Separation/Organization
#'
#' @param str_to_banner String to Convert to Banner
#' @param banner_chr Banner Boundary Character
#' @param output Boolean, TRUE to output, FALSE to Not Output
#'
#' @return Invisible String
#'
#' @examples
#' make_banner(str_to_banner = 'asdf', banner_chr = '&', output = TRUE)
#'
#' @export

make_banner <- function(str_to_banner,
                        banner_chr = "&",
                        output = TRUE) {
  top_line <- ""
  for (i in 1:(nchar(str_to_banner) + 6)) {
    top_line <- paste0(top_line, banner_chr)
  }
  top_line <- paste0("#", banner_chr, top_line, banner_chr)
  text_to_rtn <-
    paste0(
      top_line,
      "\n#",
      banner_chr,
      banner_chr,
      "  ",
      str_to_banner,
      "  ",
      banner_chr,
      banner_chr,
      "\n",
      top_line
    )

  # Write to clipboard, work for Windows and Unix-based
  # Determine the operating system
  if (.Platform$OS.type == "unix") {
    # Unix-based system (e.g., macOS, Linux)
    cat(text_to_rtn, file = pipe("pbcopy"))
  } else {
    # Windows
    cat(text_to_rtn, file = pipe("clip"))
  }

  if (output)
    cat(text_to_rtn)
  return(invisible(text_to_rtn))
}
