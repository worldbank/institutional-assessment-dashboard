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
    filter(!is.na(value)) %>%
    group_by(country_name, var_name, lac6, oecd, structural) %>%
    summarise(dtf = mean(value)) %>%
    group_by(var_name) %>%
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

}
