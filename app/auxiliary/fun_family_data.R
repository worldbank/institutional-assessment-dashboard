# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION TO CREATE FAMILY LEVEL DATA BASED ON COUNTRIES -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

family_data <- function(data, base_country) {


  miss_indicator_base <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country) %>%
    select(where(is.na)) %>%
    names

  data <-
    data %>%
    select(-c(miss_indicator_base, country_code)) %>%
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
    pivot_wider(names_from = family_var)


}
