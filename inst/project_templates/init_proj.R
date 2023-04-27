# title: "{{folder_name}}"
# subtitle: "Initial Project"
# description: Initialize project variables and utility functions required for this project


# LOAD LIBRARIES ----------------------------------------------------------

  # TODO: set up dependencies
  library(tidyverse)
  library(yaml)
  library(here)
  # library(glue)
  # library(lubridate)
  # library(rmarkdown)
  # library(knitr)
  # library(mice)
  # library(survival)
  # library(RODBC)
  # library(fuzzyjoin)


# SET PATH VARIABLES ------------------------------------------------------

  PROJECT_DICT <- yaml.load_file(here("Project Variables/project_dict.yaml"))



# Incoming files ----------------------------------------------------------

  PROJECT_FILES <- PROJECT_DICT$files



# SETUP FUNCTION SCRIPTS --------------------------------------------------

  # source(here("Code/script_one.R"))
  #
  # source(here("Code/script_two.R"))

