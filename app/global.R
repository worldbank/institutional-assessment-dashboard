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
library(officer)
library(rvg)


options(dplyr.summarise.inform = FALSE)

## Auxiliary functions -----------------------------------------------------------------

db_variables <-
  read_rds(
    here(
      "data",
      "db_variables.rds"
    )
  )


db_variables<-db_variables %>% 
  mutate(across(where(is.character), str_squish))

## add a temporary rank id within family in db_variables
set.seed(1957)
generate_random_sequence <- function(length) {
  return(sample(1:length, length, replace = FALSE))
}

db_variables<-db_variables %>% 
  group_by(family_var) %>% 
  mutate(rank_id = generate_random_sequence(n())) %>% 
  ungroup()

source(here("auxiliary", "vars-control.R"))

# Function that defines quantiles based on country, comparison and variables
source(here("auxiliary", "fun_quantiles.R"))
source(here("auxiliary", "fun_family_data.R"))
source(here("auxiliary", "fun_missing_var.R"))
source(here("auxiliary", "fun_low_variance.R"))

# Create benchmark graphs
source(here("auxiliary", "plots.R"))
source(here("auxiliary", "clean_plotly_legend.R"))
source(here("auxiliary", "fixfacets.R"))

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
  rename(Year = year) %>%
  mutate(Year = as.double(Year))




global_data <-
  read_rds(
    here(
      "data",
      "closeness_to_frontier.rds"
    )
  ) %>%
  ungroup 

global_data_dyn <-
  read_rds(
    here(
      "data",
      "closeness_to_frontier_dyn.rds"
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



ctf_long_dyn <-
  read_rds(
    here(
      "data",
      "closeness_to_frontier_dyn_long.rds"
    )
  )

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



spatial_data <-
  read_rds(
    here(
      "data",
      "indicators_map.rds"
    )
  )

clean_country <-
  read.csv(
    here(
      "data",
      "Country_name_list.csv"
    )
  )  

for(i in 1:nrow(clean_country)){
  if (clean_country[i,'Clean_Names']!=""){
    country_list$country_name[country_list$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
    ctf_long$country_name[ctf_long$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
    ctf_long_dyn$country_name[ctf_long_dyn$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
    raw_data$country_name[raw_data$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
    global_data$country_name[global_data$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
    global_data_dyn$country_name[global_data_dyn$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
    spatial_data$country_name[spatial_data$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
  }
} 


country_list = country_list[order(country_list$country_name, decreasing = FALSE), ]
ctf_long = ctf_long[order(ctf_long$country_name, decreasing = FALSE), ]
ctf_long_dyn = ctf_long_dyn[order(ctf_long_dyn$country_name, decreasing = FALSE), ]
raw_data = raw_data[order(raw_data$country_name, decreasing = FALSE), ]
global_data = global_data[order(global_data$country_name, decreasing = FALSE), ]
global_data_dyn = global_data_dyn[order(global_data_dyn$country_name, decreasing = FALSE), ]
spatial_data = spatial_data[order(spatial_data$country_name, decreasing = FALSE), ]


st_crs(spatial_data) <- "+proj=robin"

# Load data control
db_variables <-
  db_variables %>%
  filter(variable %in% vars_all | var_level == "family")


#Add Label Attirbutes to DTA file
for (i in colnames(global_data)[4:length(colnames(global_data))]){
  name<-subset(db_variables$var_name,db_variables$variable==i)
  if(length(name)>0){
    name<-str_replace_all(name, "[[:punct:]]", "")
    attr(global_data[[i]],'label')<-name
  }}

for (i in colnames(raw_data)[4:length(colnames(raw_data))]){
  attr(raw_data[[i]],'label')<-subset(db_variables$var_name,db_variables$variable==i)
}
for (i in colnames(raw_data)[4:length(colnames(raw_data))]){
  name<-subset(db_variables$var_name,db_variables$variable==i)
  if(length(name)>0){
    name<-str_replace_all(name, "[[:punct:]]", "")
    attr(raw_data[[i]],'label')<-name
  }}

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

# plot_height <- 650
plot_height <- 500

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