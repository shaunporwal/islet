#' Convert Excel File to CSV
#' @param path Directory path containing Excel file
#' @param filename Excel filename with extension
#' @param sheet Sheet name or number. Default NULL reads first sheet
#' @return Invisibly returns CSV output path
#' @importFrom readxl read_xlsx
#' @importFrom glue glue#'
#' @export
excel_to_csv <- function(path, filename, sheet = NULL) {
  # Read Excel file
  df <- readxl::read_xlsx(path = file.path(path, filename), sheet = sheet)

  # Create output filename by replacing the extension
  output_filename <- gsub("\\.xlsx$", ".csv", filename, ignore.case = TRUE)
  output_path <- file.path(path, output_filename)

  # Write to CSV
  utils::write.csv(df, file = output_path, row.names = FALSE)
  print(glue::glue("Written to csv at: {path}", path = output_path))

  # Return the output path invisibly
  invisible(output_path)
}
