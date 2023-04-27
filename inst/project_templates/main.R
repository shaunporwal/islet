# title: "{{folder_name}}"
# subtitle: "Main"
# description:


# TODO: Create ------------------------------------------------------------

  # Run sym link for local iterations of this code (if doesn't already exist - will be added for initial creation)
  #
  #
  # {{symbolic_link}}
  #
  #
  # Create data directory as needed (will not overwrite files) - then delete this code
  # Only needs to be run once per repository
  #
  #


# INITIALIZE PROJECT ------------------------------------------------------

  source(here::here("Code/init_proj.R"))


# CLEAR METADATA CALLS ----------------------------------------------------

  # AmplioHelpers::reset_metadata("Per Run Values")
  # AmplioHelpers::reset_metadata("Alerts")
  # AmplioHelpers::reset_metadata("Issues")
  # AmplioHelpers::reset_metadata("Line Logs")
  # AmplioHelpers::reset_metadata("Allowed Values")
  # AmplioHelpers::reset_metadata("All")



# DATA CLEANING FILES -----------------------------------------------------

  #&&&&&&&&&&&&&&&&&
  #&&  script_one &&
  #&&&&&&&&&&&&&&&&&

  args_script_one <- list(

    df1 = PROJECT_FILES$initial_raw$df_example_1,
    df2 = PROJECT_FILES$initial_raw$df_example_2,

    rtns = list(
      df1 = "df1",
      df2 = "df2",
    )
  )

  rtn_script_one <- run_file(file = script_one,
                            args = args_script_one)


  #&&&&&&&&&&&&&&&&&&
  #&&  script_two  &&
  #&&&&&&&&&&&&&&&&&&

  args_script_two <- list(

    df_pt_list = rtn_script_one$df_pt_list,

    rtns = list(
      df1 = "df1",
      df2 = "df2"
    )
  )

  rtn_script_two <- run_file(file = script_two,
                             args = args_script_two)




# SAVE OUT "FINAL" CLEANED DATA -------------------------------------------

  saveRDS(object = rtn_script_two$df_clin_data,
          file = file.path(get_data_path(), "Cleaned/df_clin_data.rds"))

