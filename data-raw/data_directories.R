## code to prepare `data_directories` dataset goes here

data_directory <- list(

  "raw",
  "cleaned"

)

usethis::use_data(data_directory, overwrite = TRUE)


metadata_directory <- list("metadata",
                                  "metadata/filtered",
                                  "metadata/issues",
                                  "metadata/line_logs")


usethis::use_data(metadata_directory, overwrite = TRUE)
