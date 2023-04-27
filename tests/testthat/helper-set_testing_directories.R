# # Workflow:
# #   rtn_init_test_dir <- init_test_dir()
# #
# #   insert_metadata(object,
# #                   dir = file.path(rtn_init_test_dir$temp_directory, "Other Folder"),
# #                   file)
# #
# #   remove_test_dir(dir_to_reset = rtn_init_test_dir$temp_directory,
# #                   original_directory = rtn_init_test_dir$original_directory)
#
#
# init_test_dir <- function(data_date = Sys.Date()) {
#
#   rtn_ctd <- create_test_dir()
#
#   insert_dict_yaml(data_date,
#                    dir = rtn_ctd$temp_directory)
#
#   create_project_directories(directory_list = AmplioTemplates::amplio_data_directory)
#
#   create_project_directories(directory_list = AmplioTemplates::amplio_metadata_directory)
#
#   return(list(original_directory = rtn_ctd$original_directory,
#               temp_directory = rtn_ctd$temp_directory))
#
# }
#
#
#
# create_test_dir <- function() {
#
#   temp_directory <- fs::path(tempdir(), "AmplioHelpers")
#
#   fs::dir_create(temp_directory)
#
#   original_directory <- getwd()
#   setwd(temp_directory)
#
#   dir.create("Project Variables")
#   dir.create("secure_data")
#
#   return(list(original_directory = original_directory,
#               temp_directory     = temp_directory))
#
# }
#
#
# remove_test_dir <- function(dir_to_reset = NULL,
#                             original_directory = NULL) {
#
#   # TODO: check exists (maybe needed, maybe not)
#   # TODO: check the directory includes a "temp" node... just an idea
#
#   if (stringr::str_detect(dir_to_reset, "/Temp/[[:alnum:]]+/AmplioHelpers")) {
#
#     print(paste0("dir_to_reset: ", dir_to_reset))
#     unlink(dir_to_reset, recursive = TRUE)
#
#   }
#
#   # TODO: Need try-catch in case a file is in use?
#   setwd(original_directory)
# }
#
# insert_dict_yaml <- function(data_date = Sys.Date(),
#                              prior_data_date = 1900-01-01,
#                              output_state = TRUE,
#                              autosave_state = TRUE,
#                              log_state = TRUE,
#                              dir = NULL) {
#
#   # TODO: Check not absolute path
#   # TODO: Need folder
#   # TODO: Need item name # TODO; check if already has ".yaml"
#   # TODO: Need list item
#   # TODO: Check directory exists
#   # TODO: Allow for update of just one attribute?
#
#   # Remove if already exists:
#   # TODO: temp.... OR... stop if already exists...?
#
#   # print('')
#   # print(paste0('files in PV: ', (list.files(file.path(dir, "Project Variables/")))))
#   # print(paste0('wd: ', getwd()))
#   # print(paste0('files in wd: ', list.files(file.path(getwd()), recursive = TRUE)))
#
#   stopifnot(!file.exists(file.path(dir, "Project Variables/project_dict.yaml")))
#
#   yaml::write_yaml(list(data_date = data_date,
#                         prior_data_date = prior_data_date,
#                         output_state = output_state,
#                         autosave_state = autosave_state,
#                         log_state = log_state),
#                    file.path(dir, "Project Variables/project_dict.yaml"))
#
# }
#
# insert_metadata <- function(object = NULL,
#                             dir = NULL,
#                             file = NULL) {
#
#   # TODO: Check not absolute path
#   # TODO: Need folder
#   # TODO: Need item name # TODO; check if already has ".rds"
#   # TODO: Need list item
#
#   saveRDS(object,
#           file.path(dir, file, ".rds"))
#
# }
