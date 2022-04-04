# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION TO CREATE FAMILY LEVEL DATA BASED ON COUNTRIES -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

family_data <- function(data, base_country, variable_names) {

  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country) %>%
    select(where(is.na)) %>%
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
    group_by(country_name, family_var) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(
      value = ifelse(is.nan(value),NA,value)
    ) %>%
    pivot_wider(names_from = family_var)


}
