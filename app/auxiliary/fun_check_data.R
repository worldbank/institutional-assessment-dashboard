#This file contains all data check functions used in the server file:

#================================================
#This function checks data for base countries in time trend, bar plot, and scatterplot

check_data <-function(data,country,indicator_1, indicator_2=NULL){

  #One variable scenario (used in bar and time trend)
  if (is.null(indicator_2)){
    #Establishes the variable (column) searched for
    var <-
      db_variables %>%
      filter(var_name == indicator_1) %>%
      pull(variable)

    #Establishes the base country (row) that is in use
    indicator_val <-
      data %>%
      filter(country_name == country) %>%
      pull(var)

    #Returns a boolean on whether or not the column is null for the selected indicator1
    return(is.na(indicator_val))}

  #Two indicator scenario used in bivariate correlation
  else{

    # Extracts the columns for the given indicators
    vars <- db_variables %>%
      filter(var_name %in% c(indicator_1, indicator_2)) %>%
      pull(variable)

    # Checks that both indicators are in data
    if (all(vars %in% names(data))) {
      has_na <- data %>%
        filter(country_name == country) %>%
        select(all_of(vars)) %>%
        summarise(across(everything(), ~ any(is.na(.)), .names = "has_na_{col}")) %>%
        summarise(across(starts_with("has_na_"), any)) %>%
        unlist() %>%
        any()

      return(has_na)
    } else {
      # If any of the variables do not exist in dv_variables, this indicates missing data
      return(TRUE)
  }
}}
#============================== Raw Data Check data function (used in time trends plot)
trends_check_data <- function(start_year, end_year, country, var) {

  # Filter the data down to start and end years and current comparison country
    comp_data <- raw_data %>%
    select(country_name, Year, !!sym(var)) %>%  # Select only necessary columns
    filter(Year >= start_year & Year <= end_year, country_name == country)
    
  # Check for nulls in the input variable column
  any_nulls <- comp_data %>%
    summarise(has_nulls = any(is.na(!!sym(var)))) %>%
    pull(has_nulls)
  
  return(any_nulls)  # Return TRUE if any nulls are found, FALSE otherwise
}

#====================
#This Function checks data for the map section

check_spatial_data <-function(data,indicator){

  var <-
    db_variables %>%
    filter(var_name == indicator) %>%
    pull(variable)

  indicator_val <-
    data %>%
    pull(paste0("value_",var))

  return(all(is.na(indicator_val)))
}