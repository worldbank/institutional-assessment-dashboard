# Packages ----------------------------------------------------------------

library(tidyverse)
library(DT)
library(plotly)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinybusy)
library(shinyWidgets)
library(bs4Dash)
library(fresh)
library(sf)
library(formattable)
library(here)
library(data.table)
library(hrbrthemes)
library(mailtoR)

## Auxiliary functions -----------------------------------------------------------------

source(
  here(
    "auxiliary",
    "vars-control.R"))

# Function that defines quantiles based on country, comparison and variables
source(here("auxiliary",
                 "fun_quantiles.R"))

source(here("auxiliary",
                 "fun_family_data.R"))

source(here("auxiliary",
                 "fun_missing_var.R"))

source(here("auxiliary",
                 "fun_low_variance.R"))

# Create benchmark graphs
source(here("auxiliary",
                 "plots.R"))

# Data -------------------------------------------------------------


global_data <-
  read_rds(
    here(
      "data",
      "closeness_to_frontier.rds"
    )
  ) %>%
  ungroup

ctf_long <-
  read_rds(
    here(
      "data",
      "closeness_to_frontier_long.rds"
    )
  )

country_groups <-
  read_rds(here("data",
                     "wb_country_groups.rds"))

definitions <-
  read_rds(here("data",
                     "definitions.rds"))
country_list <-
  read_rds(
    here(
      "data",
      "wb_country_list.rds"
    )
  )

wb_country_geom_fact <-
  read_rds(here("data",
                     "wb_country_geom_fact.rds"))

st_crs(wb_country_geom_fact) <- "WGS84"



period_info_available <-
  read_rds(
    here(
      "data",
      "period_info_available.rds")
  )

period_info_by_variable <-
  read_rds(
    here(
      "data",
      "period_info_by_variable.rds"
    )
  )

# Load data control
db_variables <-
  db_variables %>%
  filter(variable %in% vars_all | var_level == "family") %>%
  mutate(
    description = str_replace_all(description, "[[:punct:]]", " ")
  ) %>%
  left_join(
    period_info_by_variable,
    by = "variable"
  )

# Options ---------------------------------------------------------

variable_names <-
  db_variables %>%
  select(
    variable,
    var_level,
    var_name,
    family_var,
    family_name
  )

countries <-
  country_list %>%
  select(country_name) %>%
  unlist %>%
  unname %>%
  unique

variable_list <-
  list(
    `Anti-Corruption, Transparency and Accountability` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_fin") %>% .$var_name),
    `Business environment and trade` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_mkt") %>% .$var_name),
    `Financial market` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_fin") %>% .$var_name),
    `SOE Corporate Governance` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_service_del") %>% .$var_name),
    `Labor market` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_lab") %>% .$var_name),
    `Justice` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_leg") %>% .$var_name),
    `Political` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_pol") %>% .$var_name),
    `Public sector` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_publ") %>% .$var_name),
    `Social` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_social") %>% .$var_name)
  )

group_list <-
  list(
    `Economic` = c(country_groups %>% filter(group_category=="Economic") %>% .$group_name),
    `Region` = c(country_groups %>% filter(group_category=="Region") %>% .$group_name),
    `Income` = c(country_groups %>% filter(group_category=="Income") %>% .$group_name)
  )




# Inputs ################################################################################

plot_height <- 650

# Data sets ---------------------------------------------------------------------------




# Raw data
raw_data <-
  read_rds(here("data",
                     "raw_data.rds")) %>%
  filter(year >= 1990,
         rowSums(!is.na(.)) > 3) %>%
  rename(Year = year)
