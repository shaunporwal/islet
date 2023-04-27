init <- function(
    project_name,
    path_to_proj) {


  # local project directory (folder should NOT exist already)

  full_project_path <- file.path(path_to_proj, project_name)

  # data folder on shared drive (folder should exist already)
  #dir.create("O:/Outcomes/Andrew/Amplio/Secure Data/{PROJECT_ALIAS}" %>% glue::glue())
  # secure_data_path <- "O:/Outcomes/Andrew/Amplio/Secure Data/{PROJECT_ALIAS}" %>% glue::glue()

  starter::create_project(path = full_project_path,
                          path_data = file.path(full_project_path, 'data'),
                          template = islet::project_template,
                          symlink = FALSE)

  # create project files i.e. project_variables.yaml
  # this will create project_variables directory in working directory

  #TODO: Create interactive steps of initializing an rproj
  path = getwd()

  # creates metadata and data folder

  # if data path doesn't exist... add here
  # dir.create(, recursive = TRUE, showWarnings = FALSE)

  for (i in 1:length(islet::data_directory)) {

    dir.create(file.path("data",
                         islet::data_directory[[i]]))



  }
}
