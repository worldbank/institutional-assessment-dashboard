check_quantiles <- function(column) {
  q25 <- quantile(column, 0.25, na.rm = TRUE)
  q75 <- quantile(column, 0.75, na.rm = TRUE)
  if (!is.na(q25) & !is.na(q75) & q25 == q75) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}  

family_data <- function(data, base_country, variable_names,comparison_countries) {

  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:5)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names

  lv_data<-data%>% 
    filter(country_name %in% c(base_country,comparison_countries))%>%
    select(-(1:5))
  
  result <- sapply(lv_data, check_quantiles)
  
  lv_indicators <- names(result[result == TRUE])
  
  data <-
    data %>%
    select(-c(union(na_indicators, lv_indicators),country_code)) %>%
    ungroup %>%
    select(country_name, everything())

  
  dtf_family_level <-
    data %>%
    pivot_longer(cols = 4:ncol(.),
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
  
  return(dtf_family_level)
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
    pivot_longer(cols = 5:ncol(.),
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

compute_family_average <- function(cliar_data, vars, type = "static", db_variables,base_country,comparison_countries){
  # this function generates family averages
  # taking a simple average by grouping
  # default is static
  
  na_indicators <-
    cliar_data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:5)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names
  
  lv_data<-cliar_data%>% 
    filter(country_name %in% c(base_country,comparison_countries))%>%
    select(-(1:5))
  
  result <- sapply(lv_data, check_quantiles)
  
  lv_indicators <- names(result[result == TRUE])
  
  cliar_data <-
    cliar_data %>%
    select(-c(union(na_indicators, lv_indicators)))%>%
    ungroup %>%
    select(country_name, everything())

  vars <-setdiff(vars, c(union(na_indicators, lv_indicators)))
  
  cliar_data_long <-
    cliar_data %>%
    pivot_longer(
      cols = 6:ncol(.),
      names_to = "variable"
    ) %>%
    select(-contains("gdp")) %>%
    filter(!variable %in% grep("_avg", variable, value = T))%>%
    left_join(
      variable_names,
      by = "variable"
    )
  
  # only calculate family averages for relevant institutional clusters
  if(type == "static"){
    grouping <- c("country_code", "family_var")
    id_cols <- c("country_code")
  } else{
    grouping = c("country_code", "year", "family_var")
    id_cols <- c("country_code", "year")
  }
  
  cliar_family_level_long <- cliar_data_long |>
    group_by(
      across(all_of(grouping))
    ) |>
    summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop",
      
    )
  
  cliar_family_level <- cliar_family_level_long |>
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = family_var,
      names_glue = "{family_var}_avg",
      values_from = value
    )
  
  return(cliar_family_level)
}
