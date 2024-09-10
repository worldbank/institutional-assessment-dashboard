#THIS FILE CONTAINS 2 FUNCTIONS THAT ARE BUILT TO PREPARE DATA FILES FOR THE .dta and .rds formats when downloading datasets
#They are only called in the server.R file


dta_prep <- function(data) {
  
  prepared_data <- data %>%
    # Rename columns to ones in db_variables
    setnames(
      old = as.character(db_variables$variable),
      new = substr(as.character(db_variables$var_name), 1, 32),
      skip_absent = TRUE
    )
  
  column_names <- names(prepared_data)
  
  # Truncate column names to 32 characters 
  #We are shortening to 30 characters because of the added characters in uniqueness code below
  truncated_names <- substr(column_names, 1, 30)
  
  # Make sure names are unique (This function uses illegal "." characters)
  unique_names <- make.unique(truncated_names)
  
  # Replace invalid characters for .dta files
  cleaned_names <- str_replace_all(unique_names, "[^A-Za-z0-9_]", "_")
  
  
  # Rename columns in the final data
  setnames(prepared_data, old = column_names, new = cleaned_names)
  
  # Return the prepared data
  return(prepared_data)
}
#===========
rds_prep <- function(data) {
  
  # Prepare the data
  prepared_data <- data %>%
    # Rename columns based on db_variables
    setnames(
      old = as.character(db_variables$variable),
      new = as.character(db_variables$var_name),
      skip_absent = TRUE
    )
  
  # Set Column Names for cleaning
  column_names <- names(prepared_data)
  
  # Make sure names are unique
  unique_names <- make.unique(column_names)
  
  # Replace invalid characters
  cleaned_names <- str_replace_all(unique_names, "[^A-Za-z0-9_]", "_")
  
  
  # Rename columns in the data
  setnames(prepared_data, old = column_names, new = cleaned_names)
  
  # Return the prepared data
  return(prepared_data)
}