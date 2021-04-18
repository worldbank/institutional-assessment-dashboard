# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION THAT DEFINES THE QUANTILES BASED ON SELECTED COUNTRY AND COMPARISON GROUP -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

def_quantiles <- function(data, base_country, comparison_countries, vars) {

    quantiles <-
      data %>%
      filter(
        country_name == base_country | country_name %in% comparison_countries
      ) %>%
      select(country_name,
             all_of(vars)) %>%
      summarise_at(vars(vars),
                   ~ quantile(., c(0.25, 0.5), na.rm = TRUE)) %>%
    t %>%
    as.data.frame() %>%
    mutate(variable = row.names(.)) %>%
    rename(q25 = V1,
           q50 = V2)

  quantiles_group <- as.data.frame(apply(data[all_of(vars)], 2, quantile, probs = c(0.25,0.5) , na.rm = T)) %>%
    rownames_to_column(var="quantile_break") %>%
    pivot_longer(all_of(vars)) %>%
    rename(variable=name,dtf_q=value) %>%
    mutate(
      quantile_break = case_when(
        quantile_break == "25%" ~ "q25_group",
        quantile_break == "50%" ~ "q50_group"
      )
    )

  data <-
    data %>%
    filter(country_name == base_country) %>%
    select(all_of(vars)) %>%
    pivot_longer(all_of(vars)) %>%
    rename(variable = name,
           dtf = value) %>%
    left_join(quantiles) %>%
    mutate(
      classification = case_when(
        dtf >= q50 ~ "Advanced",
        dtf <= q25 ~ "Weak",
        dtf > q25 & dtf < q50 ~ "Emerging"
      )
    ) %>%
    filter(!is.na(dtf))

  return(data)

}
