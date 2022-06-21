
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

# Variables ----------------------------------------------------------

families <- variable_names %>% 
  distinct(family_name)

base_country <- "Jordan"

comparison_group <- c(
  "Morocco", "Egypt", "Tunisia", "Pakistan",
  "Turkey", "Philippines", "Indonesia", "Georgia",
  "Finland", "Ireland", "Croatia", "Bulgaria", "Uruguay"
)

# Overview Plot
data_family_2014_2016 <- family_data(
  global_data_2014_2016,
  base_country,
  variable_names
) %>%
  def_quantiles(
    base_country,
    country_list,
    comparison_group,
    family_names,
    variable_names
  ) %>% 
  select(country_name, variable, dtf1 = dtf, var_name)

data_family_2017_2020 <- family_data(
  global_data_2017_2020,
  base_country,
  variable_names
) %>%
  def_quantiles(
    base_country,
    country_list,
    comparison_group,
    family_names,
    variable_names
  ) %>% 
  select(country_name, variable, dtf2 = dtf, var_name)

data_family_comparison <- data_family_2014_2016 %>% 
  left_join(data_family_2017_2020) %>% 
  mutate(
    diff   = dtf2 - dtf1,
    jordan = if_else(country_name == "Jordan", TRUE, FALSE),
    case   = case_when(diff > 0 ~ "Improved",
                       diff == 0 ~ "No change",
                       diff < 0 ~ "Worsened"),
  ) %>% 
  filter(var_name != "Labor market")

## Clean for excel file
toExcel <- data_family_comparison %>% 
  rename(
    `2014-2016` = dtf1,
    `2017-2020` = dtf2,
    `Difference` = diff,
    `Variable Label` = var_name
  ) %>% 
  select(-jordan, -case) %>% 
  mutate(
    variable = "family_avg",
    family_name = `Variable Label`
  )

toExcel <- toExcel %>% 
  mutate(
    positive = if_else(Difference > 0, TRUE, FALSE),
    negative = if_else(Difference < 0, TRUE, FALSE),
  ) %>% 
  group_by(family_name) %>% 
  mutate(
    total = sum(n()),
    count_ps = sum(positive == TRUE),
    count_ng = sum(negative == TRUE),
    pct_positive = (count_ps / total) * 100,
    pct_negative = (count_ng / total) * 100
  ) %>% 
  ungroup() %>%
  select(country_name, variable, 3, 5, Difference, 4, family_name, pct_positive, pct_negative) 

Jordan <- toExcel %>% 
  filter(country_name == "Jordan") %>% 
  select(jordan_comp = Difference, family_name)

toExcelJordan <- toExcel %>% 
  left_join(Jordan, by = "family_name") %>%
  group_by(family_name) %>% 
  mutate(
    comp_pos = if_else(Difference > jordan_comp, TRUE, FALSE),
    comp_neg = if_else(Difference < jordan_comp, TRUE, FALSE),
    total = sum(n()) - 1,
    count_ps = sum(comp_pos == TRUE),
    count_ng = sum(comp_neg == TRUE),
    pct_bigger  = (count_ps / total) * 100,
    pct_smaller = (count_ng / total) * 100
  ) %>% 
  select(-c(comp_pos, comp_neg, total, count_ps, count_ng, jordan_comp))

toExcelJordan %>% 
  arrange(family_name, country_name) %>% 
  openxlsx::write.xlsx("data/excel_writeup_ctf_family_avg.xlsx", rowNames=FALSE)
