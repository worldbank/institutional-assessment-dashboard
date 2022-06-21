library(tidyverse)
library(ggtext)
library(here)

source("global.R")
source("auxiliary/vars-by-family.R")

comparison_group <- c(
  "Morocco", "Egypt", "Tunisia", "Pakistan",
  "Turkey", "Philippines", "Indonesia", "Georgia",
  "Finland", "Ireland", "Croatia", "Bulgaria", "Uruguay"
)

base_country <- "Jordan"

# By Family ----------------------------------------------------------

# Vars
source("auxiliary/vars-by-family.R")

families <- variable_names %>% 
  distinct(family_name) %>% 
  .$`family_name`

fam_vars <- list(
  "Anti-Corruption, Transparency and Accountability" = vars_transp,
  "Political" = vars_pol,
  "Justice" = vars_leg,
  "Financial market" = vars_fin, 
  # "Labor market" = vars_lab,
  "Business environment and trade" = vars_mkt,
  "Public sector performance" = vars_publ,
  "Social" = vars_social
)

# Function with all the variables to send to excel
make_excel <- function(fam) {
  
    df_all <- read_rds(
      here(
        "data",
        "raw_data.rds"
      )
    ) %>% 
    select(fam, country_name, year) %>% 
    filter(
      country_name %in% c(comparison_group, base_country),
      year > 2013
    ) %>% 
    mutate(
      group = ifelse(year %in% c(2014:2016), "2014_2016", "2017_2020")
    ) %>% 
    group_by(country_name, group) %>% 
    summarise(
      across(
        all_of(fam),
        ~ mean(., na.rm = TRUE)
      )
    ) %>% 
    pivot_longer(
      c(-country_name, -group)
    ) %>% 
    pivot_wider(
      names_from = group,
      values_from = value
    ) %>% 
    rename(
      dtf1 = `2014_2016`,
      dtf2 = `2017_2020`
    ) %>% 
    mutate(
      diff = dtf2 - dtf1,
      jordan = if_else(country_name == "Jordan", TRUE, FALSE),
      case   = case_when(
        diff > 0  ~ "Improved",
        diff == 0 ~ "No change",
        diff < 0  ~ "Worsened"
      )
    )
}
  
toExcelFam <- map_dfr(fam_vars, make_excel) %>% 
  rename(
    variable = name 
  ) %>% 
  left_join(
    variable_names %>% 
      select(variable, family_name)
    , by = "variable"
  ) %>% 
  arrange(country_name, family_name)  %>% 
  mutate(
    positive = if_else(diff > 0, TRUE, FALSE),
    negative = if_else(diff < 0, TRUE, FALSE),
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
  select(country_name, variable, `2017_2020` = dtf2, `2014_2016` = dtf1, Difference = diff, family_name, pct_positive, pct_negative) 

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
  openxlsx::write.xlsx("data/excel_writeup_indicators.xlsx", rowNames=FALSE)
