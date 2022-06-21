
# Packages -----------------------------------------------------------

library(tidyverse)
library(haven)
library(here)

# Countries ----------------------------------------------------------

base_country <- "Jordan"

comparison_group <- c(
  "Morocco", "Egypt", "Tunisia", "Pakistan",
  "Turkey", "Philippines", "Indonesia", "Georgia",
  "Finland", "Ireland", "Croatia", "Bulgaria", "Uruguay"
)

# Get data -----------------------------------------------------------
source("auxiliary/vars-by-family.R")

gbid_1720 <- read_rds(
  here(
    "data",
    "closeness_to_frontier_long_2017_2020.rds"
  )
) %>% 
  filter(country_name %in% c(base_country, comparison_group)) %>% 
  rename(dtf_2017_2020 = value)

gbid_1416 <- read_rds(
  here(
    "data",
    "closeness_to_frontier_long_2014_2016.rds"
  )
) %>% 
  filter(country_name %in% c(base_country, comparison_group)) %>% 
  rename(dtf_2014_2016 = value)

gbid_overview <- gbid_1416 %>% 
  right_join(gbid_1720) %>% 
  filter(
    variable %in% c("vars_transp", "vars_pol", "vars_leg", "vars_fin", "vars_mkt", "vars_publ", "vars_social")
  ) %>% 
  mutate(
    diff = case_when(
      dtf_2017_2020 >  dtf_2014_2016 ~ "Improved", 
      dtf_2017_2020 == dtf_2014_2016 ~ "No change",
      dtf_2017_2020 <  dtf_2014_2016 ~ "Worsened"
    ),
    diff = fct_relevel(diff, "Improved", "Worsened") 
  ) %>% 
  group_by(var_name) %>% 
  slice(1) %>% 
  ungroup()

gbid <- gbid_1416 %>% 
  right_join(gbid_1720) %>% 
  filter(variable %in% c(vars_transp, vars_pol, vars_leg, vars_fin, vars_mkt, vars_publ, vars_social)) %>% 
  mutate(
    diff = case_when(
      dtf_2017_2020 >  dtf_2014_2016 ~ "Improved", 
      dtf_2017_2020 == dtf_2014_2016 ~ "No change",
      dtf_2017_2020 <  dtf_2014_2016 ~ "Worsened"
    ),
    diff = fct_relevel(diff, "Improved", "Worsened", "No change") 
  ) %>% 
  group_by(var_name, country_name) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    family_name = ifelse(variable == "favoritism", "Public sector performance", family_name),
    family_name = ifelse(variable == "v2clrspct", "Anti-Corruption, Transparency and Accountability", family_name),
    family_short = case_when(
      family_name == "Anti-Corruption, Transparency and Accountability" ~ "anticorruption",
      family_name == "Business environment and trade" ~ "business",
      family_name == "Financial market" ~ "financial",
      family_name == "Justice" ~ "justice",
      family_name == "Labor market" ~ "labor",
      family_name == "Political" ~ "political",
      family_name == "Public sector performance" ~ "public",
      family_name == "Social" ~ "social",
    )
  )

gbid_overview <- gbid_1416 %>% 
  right_join(gbid_1720) %>% 
  filter(
    variable %in% c("vars_transp", "vars_pol", "vars_leg", "vars_fin", "vars_mkt", "vars_publ", "vars_social")
  ) %>% 
  mutate(
    diff = case_when(
      dtf_2017_2020 >  dtf_2014_2016 ~ "Improved", 
      dtf_2017_2020 == dtf_2014_2016 ~ "No change",
      dtf_2017_2020 <  dtf_2014_2016 ~ "Worsened"
    ),
    diff = fct_relevel(diff, "Improved", "Worsened") 
  ) %>% 
  group_by(country_name, var_name) %>% 
  slice(1) %>% 
  ungroup()

# To export ----------------------------------------------------------

all_df <- gbid %>%
  bind_rows(gbid_overview)
  
toexport <- all_df %>%
  mutate(
    point_diff = dtf_2017_2020 - dtf_2014_2016
  ) %>% 
  select(country_name, country_code, group, family_name, family_var, variable, var_name, dtf_2017_2020, dtf_2014_2016, point_diff, diff) %>% 
  arrange(family_name, family_var, var_name)

writexl::write_xlsx(toexport, "data/indicators_differences_by_country_and_family.xlsx")
