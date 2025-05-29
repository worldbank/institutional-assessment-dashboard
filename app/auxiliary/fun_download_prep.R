#THIS FILE CONTAINS 3 FUNCTIONS THAT ARE BUILT TO PREPARE DATA FILES FOR THE .dta and .rds formats when downloading datasets
#They are only called in the server.R file
#This is a helper function that makes a dataset have starter unique columns
make_colnames_unique <- function(data) {
  #unique columns on original data
  CN<- make.unique(names(data))
  
  #Dimensionality Check
  if (length(CN) == ncol(data)) {
    names(data) <- CN
  } else {
    stop("ERROR: Dimensionality Mismatch in initial column uniqueness check. Check Original Dataset.")
  }
  
  return(data)
}
#=================Function that preps dta
dta_prep <- function(data, des_names=FALSE) {
#Handles descriptive data names switch
if (des_names==TRUE){
  db_data <- data %>%
    # Rename columns to ones in db_variables
    setnames(
      old = as.character(db_variables$variable),
      new = substr(as.character(db_variables$var_name), 1, 32),
      skip_absent = TRUE
    )}
else{db_data<-data}
  
#Duplication in original data check
 if (any(duplicated(names(db_data)))){
   prepared_dta_data<- make_colnames_unique(db_data)
 }
  else{prepared_dta_data <- db_data}
  
  #Old data names before preparation
  column_names <- names(prepared_dta_data)

  #We are shortening to 30 characters because of the added characters in uniqueness code below
  truncated_names <- substr(column_names, 1, 30)
  
  # Make sure names are unique (This function uses illegal "." characters)
  unique_names <- make.unique(truncated_names)
  
  # Replace invalid characters for .dta files
  cleaned_dta_names <- str_replace_all(unique_names, "[^A-Za-z0-9_]", "_")
  
  
  # Rename columns in the final data
  setnames(prepared_dta_data, old = column_names, new = cleaned_dta_names)

  return(prepared_dta_data)
}


#=========================== RDS PREP
rds_prep <- function(data, des_names) {
  #Handles descriptive data names switch
  if (des_names==TRUE){
    Rdb_data <- data %>%
      # Rename columns to ones in db_variables
      setnames(
        old = as.character(db_variables$variable),
        new = substr(as.character(db_variables$var_name), 1, 32),
        skip_absent = TRUE
      )}
  else{Rdb_data<-data}
  
  #Duplication in original data check
  if (any(duplicated(names(Rdb_data)))){
    prepared_rds_data<- make_colnames_unique(Rdb_data)
  }
  else{prepared_rds_data <- Rdb_data}
  
  # Set Column Names for cleaning
  column_names <- names(prepared_rds_data)
  
  # Make sure names are unique
  unique_names <- make.unique(column_names)
  
  # Replace invalid characters
  cleaned_rds_names <- str_replace_all(unique_names, "[^A-Za-z0-9_]", "_")
  
  
  # Rename columns in the data
  setnames(prepared_rds_data, old = column_names, new = cleaned_rds_names)
  
  # Return the prepared data
  
  return(prepared_rds_data)
}
#================ CSV Prep
csv_prep<- function(data,des_names){
  if (des_names==TRUE){
    XL_data <- data %>%
    # Rename columns to ones in db_variables
    setnames(
      old = as.character(db_variables$variable),
      new = as.character(db_variables$var_name),
      skip_absent = TRUE)}
  else{
    XL_data<-data}
    # Set Column Names for cleaning
  column_names <- names(XL_data)
    
    # Make sure names are unique
  unique_names <- make.unique(column_names)
    
    # Replace invalid characters
  cleaned_XL_names <- str_replace_all(unique_names, "[^A-Za-z0-9_]", "_")
    
    
    # Rename columns in the data
  setnames(XL_data, old = column_names, new = cleaned_XL_names)
    
    # Return the prepared data
    
  return(XL_data)
  }
  