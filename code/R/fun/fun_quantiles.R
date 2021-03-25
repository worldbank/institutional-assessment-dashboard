# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION THAT DEFINES THE QUANTILES BASED ON SELECTED COUNTRY AND COMPARISON GROUP -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

def_quantiles <- function(data, selected_country, comparison_group, vars) {

  data <- data %>%
    filter(
      country == selected_country | country %in% comparison_group
    ) %>%
    ungroup() %>%
    select(country,all_of(vars))

  quantiles_group <- as.data.frame(apply(data[all_of(vars)], 2, quantile, probs = c(0.25,0.5) , na.rm = T)) %>%
    rownames_to_column(var="quantile_break") %>%
    pivot_longer(all_of(vars)) %>%
    rename(variable=name,dtf_q=value) %>%
    arrange(variable,quantile_break) %>%
    mutate(
      q = case_when(
        quantile_break == "25%" ~ "q25_group",
        quantile_break == "50%" ~ "q50_group"
      )
    ) %>%
    select(variable,q,dtf_q)

  data <- data %>%
    filter(country==selected_country) %>%
    pivot_longer(all_of(vars)) %>%
    rename(variable=name,dtf=value) %>%
    left_join(quantiles_group,by="variable") %>%
    pivot_wider(
      names_from = q,
      values_from = dtf_q
    ) %>%
    mutate(
      classification = case_when(
        dtf <= q25_group ~ "Weak",
        dtf > q25_group & dtf <= q50_group ~ "Emerging",
        dtf > q50_group ~ "Advanced"
      )
    )

  return(data)

}
