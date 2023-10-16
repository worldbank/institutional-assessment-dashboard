family_data <- function(data, base_country, variable_names) {

  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:3)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names 

  data <-
    data %>%
    select(-c(na_indicators, country_code)) %>%
    ungroup %>%
    select(country_name, everything())


  dtf_family_level <-
    data %>%
    pivot_longer(cols = 2:ncol(.),
                 names_to = "variable") %>%
    left_join(variable_names,
              by = "variable") %>%
    filter(!variable %in% grep("_avg", variable, value = T)) %>% 
    group_by(country_name, family_var) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(
      value = ifelse(is.nan(value),NA,value)
    ) %>%
    pivot_wider(names_from = family_var)

}


family_data_dyn <- function(data, base_country, variable_names) {
  
  na_indicators_df <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country) 
  
  missing_vars <- sapply(na_indicators_df, function(x) sum(is.na(x)) / length(x))
  na_indicators <- names(missing_vars[missing_vars == 1])

  data <-
    data %>%
    select(-c(na_indicators, country_code)) %>%
    ungroup %>%
    select(country_name, everything())
  
  dtf_family_level <-
    data %>%
    pivot_longer(cols = 4:ncol(.),
      names_to = "variable") %>%
    left_join(variable_names,
      by = c("variable")) %>%
    filter(!variable %in% grep("_avg", variable, value = T)) %>% 
    group_by(country_name, family_var, year) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(
      value = ifelse(is.nan(value),NA,value)
    ) %>%
    pivot_wider(names_from = family_var)
  
 return(dtf_family_level)
 
}
