## code to prepare `project_template_lists` dataset goes here

project_template <- quote(list(
  readme = list(
    template_filename = system.file("project_templates/readme.md",
                                    package = "islet"),
    filename = "README.md",
    copy = FALSE
  ),
  gitignore = list(
    template_filename = system.file("project_templates/gitignore.txt", package = "islet"),
    filename = ".gitignore",
    copy = TRUE
  ),
  project_dict = list(
    template_filename = system.file("project_templates/project_dict.yaml", package = "islet"),
    filename = "project_variables/project_dict.yaml",
    copy = FALSE
  ),
  main = list(
    template_filename = system.file("project_templates/main.R", package = "islet"),
    filename = "code/main.R",
    copy = FALSE
  ),
  init_proj = list(
    template_filename = system.file("project_templates/init_proj.R", package = "islet"),
    filename = "code/init_proj.R",
    copy = FALSE
  ),
  script_one = list(
    template_filename = system.file("project_templates/script_one.R", package = "islet"),
    filename = glue::glue("code/script_one.R"),
    copy = FALSE
  ),
  script_two = list(
    template_filename = system.file("project_templates/script_two.R", package = "islet"),
    filename = glue::glue("code/script_two.R"),
    copy = FALSE
  )
))

usethis::use_data(project_template, overwrite = TRUE)
