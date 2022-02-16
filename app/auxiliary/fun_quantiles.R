# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION THAT DEFINES THE QUANTILES BASED ON SELECTED COUNTRY AND COMPARISON GROUP -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

def_quantiles <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {


  print(comparison_countries)
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)

  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country) %>%
    select(where(is.na))

  na_indicators <-
    if(length(na_indicators)==0) {
      NULL
    } else {
      na_indicators <- na_indicators %>% pivot_longer(cols = 1:ncol(.), names_to = "missing_var")
    }

  quantiles <-
    data %>%
    ungroup() %>%
    filter(
      country_name %in% comparison_list$country_name | country_name == base_country
    ) %>%
    select(
      country_name, all_of(vars)
    )

  quantiles_group <-
    comparison_list %>%
    select(country_name) %>%
    right_join(quantiles, by = c("country_name")) %>%
    pivot_longer(cols = all_of(vars), names_to = "variable") %>%
    filter(! variable %in% na_indicators$missing_var) %>%
    left_join(variable_names,
              by = "variable") %>%
    filter(!is.na(value)) %>%
    group_by(
      country_name, variable, var_name
    ) %>%
    summarise(dtf = mean(value)) %>%
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(dtf),
      q25 = quantile(dtf, c(0.25)),
      q50 = quantile(dtf, c(0.5)),
      status = case_when(
        dtt <= .25 ~ "Weak\n(bottom 25%)",
        dtt > .25 & dtt <= .50 ~ "Emerging\n(25% - 50%)",
        dtt > .50 ~ "Strong\n(top 50%)"
      )
    ) %>%
    ungroup

  base_low_var <-
    quantiles_group %>%
    filter(country_name == base_country & q25==q50) %>%
    .$variable

  quantiles_group <-
    quantiles_group %>%
    filter(! variable %in% base_low_var)

  return(quantiles_group)

}
