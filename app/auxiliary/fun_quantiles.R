# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION THAT DEFINES THE QUANTILES BASED ON SELECTED COUNTRY AND COMPARISON GROUP -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

def_quantiles <- function(data, base_country, comparison_countries, vars, variable_names) {

  #base_country <- "Uruguay"

  #data <- global_data
  #vars <- vars_lab

  #vars <- family_names
  #data <- dtf_family_level

  quantiles <-
    data %>%
    ungroup() %>%
    mutate(oecd = ifelse(country_code == "COL", 1, oecd)) %>%
    filter(
      lac6 == 1 | oecd == 1 | country_name == base_country
    ) %>%
    select(country_name, all_of(vars), lac6, oecd, structural) %>%
    pivot_longer(cols = vars,
                 names_to = "variable") %>%
    left_join(variable_names,
              by = "variable") %>%
    group_by(country_name, var_name, lac6, oecd, structural) %>%
    summarise(dtf = mean(value, na.rm = TRUE))  %>%
    group_by(var_name) %>%
    mutate(
      rank = dense_rank(dtf),
      rank_max = max(rank, na.rm = TRUE),
      r25 = quantile(1:rank_max, c(.25)),
      r50 = quantile(1:rank_max, c(.5)),
      dtt = (rank/rank_max),
      status_dtt = case_when(
        rank <= r25 ~ "Weak",
        rank > r25 & rank <= r50 ~ "Emerging",
        rank > r50 ~ "Advanced"
      ),
      q25 = quantile(dtf, c(0.25), na.rm = TRUE),
      q50 = quantile(dtf, c(0.5), na.rm = TRUE),
      avg = mean(dtf, na.rm = TRUE),
      status_dtf = case_when(
        dtf <= q25 ~ "Weak",
        dtf > q25 & dtf <= q50 ~ "Emerging",
        dtf > q50 ~ "Advanced"
      ),
      var_name = var_name %>% str_to_sentence() %>% str_replace_all("Soe", "SOE")
    )

}
