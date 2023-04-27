# {{folder_name}}

# Symbolic Link to Secure Data
{{symbolic_link}}

# Data Location

1. Save data in a secure folder on a network drive with a subfolder indicating date data was received.

- Create a symlink as needed to the shared drive data location (not needed on the first creation of directory from template)
- Use the create directories function to establish data structure:
  # AmplioHelpers::create_project_directories(AmplioTemplates::amplio_data_directory)

2. The PROJECT_DICT$data_date objects refers to the date folder in the "secure_data" folder.

3. If updated data is received, add a second date folder indicating the date the data was received, and update PROJECT_DICT$data_date and PROJECT_DICT$prior_data_date.

4. This system ensures the history of results is preserved and previously produced results can always be re-created.


# Scripts as functions

1. Setups scripts are structured as functions to:

  - Allow for clearly defined inputs and outputs
  - Running scripts in a "thread-safe" way where different analysts are not potentially using resources in a different state than assumed.

2. To preserve the "scipt"-like ability to jump into projects at any given point,  the "main" file can be run up to right before the function call, and then within the script the following type of line can be toggled as a comment to allow "running/re-running from the top" of that script, as needed: 

  - }; list2env(list_raw_files_1, globalenv()) 


3. This structure involves "source()" each function witin the init_proj.R file, so it exists in the environment for use.  



# Reminders

1. Do not use setwd()-type calls within project code files.

2. Do not use rm()-type calls within project code files.

3. At some point, we will use the renv to preserve the state of a project's packages, but at this point until we work out the versionizing questions in GitHub for these renv caches, we'll use the old system.
   
   
# Other notes

1. To save energy/time with the intermediary github updates, the following files have been dealt with in ways that mirror the prior iterations.  These will be largely updated for the 2.0 approach.  As opposed to the setup files, these files each act discretely and the overall order of how they should be run is consistent between sites already.

  - create_table_one.R
  - surgeon_rates.R
  - clean_data_compare.R
  - surgeon_rates_compare.R
   
   
# Project Log

**{{Sys.Date()}}**

  Created project folder
