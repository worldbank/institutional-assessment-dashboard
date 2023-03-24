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
library(shinyhelper)
library(bs4Dash)
library(fresh)
library(sf)
library(haven)
library(zoo)
library(formattable)
library(here)
library(data.table)
library(hrbrthemes)
library(bsplus)
library(htmltools)

## Auxiliary functions -----------------------------------------------------------------

db_variables <-
  read_rds(
    here(
      "data",
      "db_variables.rds"
    )
  )

source(here("auxiliary", "vars-control.R"))

# Function that defines quantiles based on country, comparison and variables
source(here("auxiliary", "fun_quantiles.R"))
source(here("auxiliary", "fun_family_data.R"))
source(here("auxiliary", "fun_missing_var.R"))
source(here("auxiliary", "fun_low_variance.R"))

# Create benchmark graphs
source(here("auxiliary", "plots.R"))

# Data -------------------------------------------------------------

raw_data <-
  read_rds(
    here(
      "data",
      "compiled_indicators.rds"
    )
  ) %>%
  filter(year >= 1990,
         rowSums(!is.na(.)) > 3) %>%
  rename(Year = year)

raw_data$country_name <- str_replace_all(raw_data$country_name, "Yugoslavia", "Serbia")

global_data <-
  read_rds(
    here(
      "data",
      "closeness_to_frontier.rds"
    )
  ) %>%
  ungroup

global_data$country_name <- str_replace_all(global_data$country_name, "Yugoslavia", "Serbia")

ctf_long <-
  read_rds(
    here(
      "data",
      "closeness_to_frontier_long.rds"
    )
  )

ctf_long$country_name <- str_replace_all(ctf_long$country_name, "Yugoslavia", "Serbia")

country_groups <-
  read_rds(
    here(
      "data",
      "wb_country_groups.rds"
    )
  )

definitions <-
  read_rds(
    here(
      "data",
      "definitions.rds"
    )
  )

country_list <-
  read_rds(
    here(
      "data",
      "wb_country_list.rds"
    )
  )

country_list$country_name <- str_replace_all(country_list$country_name, "Yugoslavia", "Serbia")


spatial_data <-
  read_rds(
    here(
      "data",
      "indicators_map.rds"
    )
  )

spatial_data$country_name <- str_replace_all(spatial_data$country_name, "Yugoslavia", "Serbia")


st_crs(spatial_data) <- "+proj=robin"

# Load data control
db_variables <-
  db_variables %>%
  filter(variable %in% vars_all | var_level == "family")

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
  raw_data %>%
  select(country_name) %>%
  filter(!(country_name %in% country_groups$group_name)) %>%
  unlist %>%
  unname %>%
  unique %>%
  sort

extract_variables <-
  function(x) {
    db_variables %>%
      filter(
        family_name == x
      ) %>%
      pull(var_name)
  }

variable_list <-
  lapply(family_names$var_name, extract_variables)

names(variable_list) <- family_names$var_name


group_list <-
  list(
    `Economic` = country_groups %>% filter(group_category == "Economic") %>% pull(group_name),
    `Region` = country_groups %>% filter(group_category == "Region") %>% pull(group_name),
    `Income` = country_groups %>% filter(group_category == "Income") %>% pull(group_name)
  )


all_groups <- group_list %>% unlist %>% unname

# Inputs ################################################################################

plot_height <- 650

customItem <- 
  function(text, 
           icon = shiny::icon("warning"),
           href = NULL, ...) {
    
    if (is.null(href)) 
    
      href <- "#"
      icon <- tagAppendAttributes(
        icon, 
        class = "nav-icon"
      )
      
      tags$li(
        a(href = href, icon, text, class = "nav-link", target = "_blank"),
        class = "nav-item"
      )
}
