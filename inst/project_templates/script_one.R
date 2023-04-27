# title: "{{folder_name}}"
# subtitle: "read_data"
# description: This file parses the raw data for the "primary" data set for the project


fn_read_data <- function(df_clin_data,
                               df_pt_list,
                               rtns) {

  # }; list2env(args_read_data, globalenv())

  options(CURRENT_FILE = "script_one")


# READ RAW DATA FILES -----------------------------------------------------

  # Read the files
  df_clin_data <- readr::read_csv(file = file.path(get_data_path(),
                                                   clinical_data_file),
                                  col_types = cols(.default = "c")) %>%
    janitor::clean_names(case = "all_caps")



# CHECK COLUMN NAMES ------------------------------------------------------

  #&&&&&&&&&&&&&&&&&&&&&
  #&&  clinical_data  &&
  #&&&&&&&&&&&&&&&&&&&&&

  clin_col <- gdata::read.xls(here("Documents/project_variables_radpros.xlsx"),
                              sheet = "Parsed Variables",
                              stringsAsFactors = FALSE) %>%
    filter(Data.Source.Name == "clinical_data") %>%
    pull(Raw.Variable.Dependency)


  assert_vals(all.equal(sort(clin_col),
                        sort(names(df_clin_data))))





# SAVE OUT RAW DATAFRAMES FOR PARSING --------------------------------------

  return(imap(rtns, ~ get(.x)))

}







