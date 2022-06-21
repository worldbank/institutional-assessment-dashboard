
# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(progress)

# Variables ----------------------------------------------------------
source("global.R")
source("auxiliary/static_plot2.R")
source("auxiliary/vars-by-family.R")

# Read data ----------------------------------------------------------

global_data_2014_2016 <- read_rds(
  here(
    "data",
    "closeness_to_frontier_2014_2016.rds"
  )
) %>%
  ungroup()

global_data_2017_2020 <- read_rds(
  here(
    "data",
    "closeness_to_frontier_2017_2020.rds"
  )
) %>%
  ungroup()


# Family vars -------------------------------------------------------------

families <- variable_names %>% 
  distinct(family_name) %>% 
  .$`family_name`

fam_vars <- list(
  "Anti-Corruption, Transparency and Accountability" = vars_transp,
  "Political" = vars_pol,
  "Justice" = vars_leg,
  "Financial market" = vars_fin,
  "Business environment and trade" = vars_mkt,
  "Public sector performance" = vars_publ,
  "Social" = vars_social
)

# Variables ----------------------------------------------------------

dtf1 <- global_data_2014_2016 %>% 
  filter(
    country_name %in% c(comparison_group, base_country)
  ) %>% 
  pivot_longer(
    c(-country_name, -country_code)
  ) %>% 
  rename(dtf1 = value)

dtf2 <- global_data_2017_2020 %>% 
  filter(
    country_name %in% c(comparison_group, base_country)
  ) %>% 
  pivot_longer(
    c(-country_name, -country_code)
  ) %>% 
  rename(dtf2 = value)

dtf <- dtf1 %>% 
  left_join(dtf2, by = c("country_name", "country_code", "name"))

toExcelFam <- dtf %>% 
  rename(variable = name) %>% 
  left_join(
    variable_names %>% 
      select(variable, family_name, var_name)
    , by = "variable"
  ) %>% 
  arrange(country_name, family_name)  %>% 
  mutate(
    diff = dtf2 - dtf1,
    jordan = if_else(country_name == "Jordan", TRUE, FALSE),
    case   = case_when(
      diff > 0  ~ "Improved",
      diff == 0 ~ "No change",
      diff < 0  ~ "Worsened"
    ),
    positive = if_else(diff > 0, TRUE, FALSE),
    negative = if_else(diff < 0, TRUE, FALSE)
  ) %>% 
  group_by(variable) %>% 
  mutate(
    total = sum(n()),
    count_ps = sum(positive == TRUE, na.rm = TRUE),
    count_ng = sum(negative == TRUE, na.rm = TRUE),
    pct_positive = (count_ps / total) * 100,
    pct_negative = (count_ng / total) * 100
  ) %>% 
  ungroup() %>% 
  select(country_name, variable, `2017_2020` = dtf2, `2014_2016` = dtf1, Difference = diff, family_name, var_name, pct_positive, pct_negative) 

Jordan <- toExcelFam %>% 
  filter(country_name == "Jordan") %>% 
  select(jordan_comp = Difference, variable)

toExcelJordan <- toExcelFam %>% 
  left_join(Jordan, by = "variable") %>%
  group_by(variable) %>% 
  mutate(
    comp_pos = if_else(Difference > jordan_comp, TRUE, FALSE),
    comp_neg = if_else(Difference < jordan_comp, TRUE, FALSE),
    total = sum(n()) - 1,
    count_ps = sum(comp_pos == TRUE, na.rm = TRUE),
    count_ng = sum(comp_neg == TRUE, na.rm = TRUE),
    pct_bigger  = (count_ps / total) * 100,
    pct_smaller = (count_ng / total) * 100
  ) %>% 
  select(-c(comp_pos, comp_neg, total, count_ps, count_ng, jordan_comp))

toExcelJordan %>% 
  arrange(variable, country_name) %>% 
  openxlsx::write.xlsx("data/excel_writeup_ctf_indicators.xlsx", rowNames=FALSE)
