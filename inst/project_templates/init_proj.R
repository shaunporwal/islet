# title: "{{folder_name}}"
# subtitle: "Initial Project"
# description: Initialize project variables and utility functions required for this project


# LOAD LIBRARIES ----------------------------------------------------------

  # TODO: set up dependencies
  library(yaml)
  library(glue)
  library(lubridate)
  library(rmarkdown)
  library(knitr)
  library(mice)
  library(survival)
  library(RODBC)
  library(fuzzyjoin)
  library(tidyverse)
  library(mskR)
  library(here)
  library(AmplioHelpers)



# LOAD LOCAL FUNCTIONS ----------------------------------------------------

  source("O:/Outcomes/Andrew/Amplio/Functions/Production/ImportAnalyticFunctions.R")
  source("O:/Outcomes/Andrew/Amplio/Functions/Production/ImportDatabaseFunctions.R")
  source("O:/Outcomes/Andrew/Amplio/Functions/ryanw/SourceUpdatedFunctions.R")



# SET PATH VARIABLES ------------------------------------------------------

  PROJECT_DICT <- yaml.load_file(here("Project Variables/project_dict.yaml"))



# Incoming files ----------------------------------------------------------

  PROJECT_FILES <- PROJECT_DICT$files



# SET IDB FIELDS ----------------------------------------------------------

  ANALYST_LAST_NAME <- get_analyst_schema()



# DATABASE FIELDS ---------------------------------------------------------

  AMPLIO_PROC_VARIABLE <- ""



# SETUP FUNCTION SCRIPTS --------------------------------------------------

  # source(here("Code/fn_read_data.R"))
  #
  # source(here("Code/fn_query_data.R"))
  #
  # source(here("Code/fn_parse_essential_data.R"))
  #
  # source(here("Code/fn_clean_clin_data.R"))
  #
  # source(here("Code/fn_clean_comp_data.R"))
  #
  # source(here("Code/fn_align_data.R"))
  #
  # source(here("Code/fn_derive_data.R"))
  #
  # source(here("Code/fn_derive_risk_groups_data.R"))
