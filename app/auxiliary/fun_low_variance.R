
# Functions: low_variance and low_variance_dyn
# ---------------------------------------------------------
# These functions are designed to calculate quantiles for a selected base country
# and a comparison group of countries. The quantiles are calculated for a set of 
# variables, and the functions categorize the variables into "Weak", "Emerging", 
# or "Strong" based on their percentile rank. Missing data is handled dynamically 
# to ensure that only relevant variables are considered in the quantile calculation.
# 
# Parameters:
# - data: A dataset containing country-specific data for analysis.
# - base_country: The country of interest for which quantiles are calculated.
# - country_list: A dataset with country details, used to filter comparison countries.
# - comparison_countries: A vector of country names to compare against the base country.
# - vars: A vector of variable names to evaluate.
# - variable_names: A dataframe mapping variable codes to human-readable names for better interpretability.
# 
# Outputs:
# - low_variance: Returns a list of variables from the base country that have 
#                 identical 25th and 75th percentiles, indicating low variance.
# - low_variance_dyn: Similar to low_variance but handles missing variables 
#                     dynamically and ensures variables with complete data are selected.
# ---------------------------------------------------------


low_variance <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {

  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)

  # List all variables that are missing for the base country -- these will be removed from the data
  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:5)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names 

  # List final relevant variables: those selected, minus those missing
  variables <-
    setdiff(vars, na_indicators)

  variables <-
    intersect(variables, names(data))

  # This is the relevant data to be used
  quantiles <-
    data %>%
    ungroup() %>%

    # Keep only the base and comparison countries
    filter(
      (country_name %in% comparison_list$country_name) | (country_name == base_country)
    ) %>%

    # Keep only selected, non-missing indicators
    select(
      country_name,
      all_of(variables)
    )

  # Merge with variable dictionary
  quantiles <-
    quantiles %>%

    # Make long per indicator
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable"
    ) %>%

    # Add variables definition and family
    left_join(
      variable_names,
      by = "variable"
    )

  # Calculate quantiles
  quantiles <-
    quantiles %>%

    # Remove missing values
    filter(!is.na(value)) %>%

    # Calculate relevant indicators
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(value),
      q25 = quantile(value, c(0.25)),
      q75 = quantile(value, c(0.75)),
      status = case_when(
        dtt <= .25 ~ "Weak\n(bottom 25%)",
        dtt > .25 & dtt <= .50 ~ "Emerging\n(25% - 50%)",
        dtt > .50 ~ "Strong\n(top 50%)"
      )
    ) %>%
    ungroup %>%
    rename(dtf = value) %>%
    filter(country_name == base_country & q25==q75) %>%
    select(variable) %>%
    unlist

  return(quantiles)

}

low_variance_dyn <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {
  
  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)
  
  
  # List all variables that are missing for the base country -- these will be removed from the data
  # na_indicators <-
  #   data %>%
  #   ungroup() %>%
  #   filter(country_name == base_country) %>%
  #   select(where(is.na)) %>%
  #   names
  
  na_indicators_df <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country) 
  
  missing_vars <- sapply(na_indicators_df, function(x) sum(is.na(x)) / length(x))
  na_indicators <- names(missing_vars[missing_vars == 1])
  
  
  # List final relevant variables: those selected, minus those missing
  # variables <-
  #   setdiff(vars, na_indicators)
  # 
  # variables <-
  #   intersect(variables, names(data))
  
  if(length(na_indicators) != 0){
    variables <-
      setdiff(vars, na_indicators)
    variables <-
      intersect(variables, names(data))
  }else{
    variables <- vars
  }
  
  # This is the relevant data to be used
  quantiles <-
    data %>%
    ungroup() %>%
    
    # Keep only the base and comparison countries
    filter(
      (country_name %in% comparison_list$country_name) | (country_name == base_country)
    ) %>%
    
    # Keep only selected, non-missing indicators
    select(
      country_name,
      all_of(variables)
    )
  
  # Merge with variable dictionary
  quantiles <-
    quantiles %>%
    
    # Make long per indicator
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable"
    ) %>%
    
    # Add variables definition and family
    left_join(
      variable_names,
      by = "variable"
    )
  
  # Calculate quantiles
  quantiles <-
    quantiles %>%
    
    # Remove missing values
    filter(!is.na(value)) %>%
    
    # Calculate relevant indicators
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(value),
      q25 = quantile(value, c(0.25)),
      q75 = quantile(value, c(0.75)),
      status = case_when(
        dtt <= .25 ~ "Weak\n(bottom 25%)",
        dtt > .25 & dtt <= .50 ~ "Emerging\n(25% - 50%)",
        dtt > .50 ~ "Strong\n(top 50%)"
      )
    ) %>%
    ungroup %>%
    rename(dtf = value) %>%
    filter(country_name == base_country & q25==q75) %>%
    distinct(variable) %>%
    unlist
  
  return(quantiles)
  
}

