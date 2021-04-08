# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION THAT DEFINES THE QUANTILES BASED ON SELECTED COUNTRY AND COMPARISON GROUP -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

def_quantiles <- function(data, selected_country, selected, vars) {

  data <- data %>%
    filter(
      country_name == selected_country | country_name %in% selected
    ) %>%
    ungroup() %>%
    select(country_name,all_of(vars))

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

  data <- data %>%
    filter(country_name==selected_country) %>%
    pivot_longer(all_of(vars)) %>%
    rename(variable=name,dtf=value) %>%
    left_join(quantiles_group,by="variable") %>%
    pivot_wider(
      names_from = quantile_break,
      values_from = dtf_q
    ) %>%
    mutate(
      classification = case_when(
        dtf <= q25_group ~ "Weak",
        dtf > q25_group & dtf <= q50_group ~ "Emerging",
        dtf > q50_group ~ "Advanced"
      )
    ) %>%
    filter(!is.na(dtf))

  return(data)

}
