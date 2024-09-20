#EXTRACT VARIABLES FILE

#This file contains functions for extracting variables from datasets in global.R
extract_variables <-
  function(x) {
    db_variables %>%
      filter(
        family_name == x
      ) %>%
      pull(var_name)
  }
#=====
extract_variables_benchmarked <-
  function(x) {
    db_variables %>%
      filter(
        family_name == x, benchmarked_ctf=='Yes'
      ) %>%
      pull(var_name)
  }