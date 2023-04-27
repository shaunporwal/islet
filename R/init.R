init <- function() {


 has_r_proj <- isTRUE(any(grepl(x = list.files(path = path), pattern = "\\.Rproj$")))

# create project files i.e. project_variables.yaml
# this will create project_variables directory in working directory

 #TODO: Create interactive steps of initializing an rproj
 if (has_r_proj == TRUE) {
  path = getwd()

  dir.create(file.path(path, "project_variables"))
 } else {
    stop("Create .rproj for this project and rerun init()")
  }

  # creates metadata and data folder

  # if data path doesn't exist... add here
  dir.create(data_path, recursive = TRUE, showWarnings = FALSE)

  for (i in 1:length(data_directory)) {

    dir.create(file.path(data_path,
                         data_directory[[i]]))

}
}
