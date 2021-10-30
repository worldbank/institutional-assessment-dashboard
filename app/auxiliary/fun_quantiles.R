# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION THAT DEFINES THE QUANTILES BASED ON SELECTED COUNTRY AND COMPARISON GROUP -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

def_quantiles <- function(datam, base_country, country_list, selected_groups, vars, variable_names) {

  #base_country <- "Uruguay"

  #data <- global_data
  #vars <- vars_mkt

  #vars <- family_names
  #data <- dtf_family_level

  comparison_list <-
    country_list %>%
    #filter(group %in% c("OECD members"))
    filter(group %in% selected_groups)

  na_indicators <- data %>%
    ungroup() %>%
    #select(-c(lac,lac6,oecd)) %>%
    filter(country_name == base_country) %>%
    select(where(is.na)) %>%
    pivot_longer(cols = 1:ncol(.), names_to = "missing_var")

  quantiles <- data %>%
    ungroup() %>%
    #select(-c(lac,lac6,oecd)) %>%
    filter(
      country_name %in% comparison_list$country_name | country_name == base_country
    ) %>%
    select(
      country_name, all_of(vars)
    )

  quantiles_group <- comparison_list %>%
    select(country_name,group) %>%
    right_join(quantiles, by = c("country_name")) %>%
    pivot_longer(cols = all_of(vars), names_to = "variable") %>%
    filter(! variable %in% na_indicators$missing_var) %>%
    left_join(variable_names,
              by = "variable") %>%
    filter(!is.na(value)) %>%
    group_by(
      country_name, variable, var_name, group
    ) %>%
    summarise(dtf = mean(value)) %>%
    group_by(variable,var_name) %>%
    mutate(
      n = n(),
      dtt = percent_rank(dtf),
      q25 = quantile(dtf, c(0.25)),
      q50 = quantile(dtf, c(0.5)),
      r25 = floor(n * .25) / n,
      r50 = floor(n * .5) / n,
      status_dtt = case_when(
        dtt <= .25 ~ "Weak",
        dtt > .25 & dtt <= .50 ~ "Emerging",
        dtt > .50 ~ "Advanced"
      ),
      status_dtf = case_when(
        dtf <= q25 ~ "Weak",
        dtf > q25 & dtf <= q50 ~ "Emerging",
        dtf > q50 ~ "Advanced"
      )
    )

  return(quantiles_group)

}
