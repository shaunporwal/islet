read_raw_data <- function(file){
  df_raw_data <- read.csv(file = file, stringsAsFactors = FALSE) %>%
    janitor::clean_names(case = "all_caps") %>%
    mutate(across(where(is.character), function(col) {
      tryCatch({
        converted <- as.Date(col)
        if (all(is.na(converted))) {
          return(col)
        } else {
          return(converted)
        }
      }, error = function(e) {
        return(col)
      })
    })) %>%
    mutate(across(where(is.character), toupper))

  return(df_raw_data)
}
