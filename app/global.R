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
library(openxlsx)
library(countrycode)
library(cicerone)
library(shinyhelper)
library(colourpicker)

options(dplyr.summarise.inform = FALSE)

## Auxiliary functions -----------------------------------------------------------------

db_variables <-
  read_rds(
    here(
      "data",
      "db_variables.rds"
    )
  )

family_order <- read.csv(here(
  "data",
  "family_order.csv"
))

db_variables <- left_join(db_variables, family_order, by = "family_name")

source(here("auxiliary", "vars-control.R"))

# Function that defines quantiles based on country, comparison and variables
source(here("auxiliary", "fun_quantiles.R"))
source(here("auxiliary", "fun_family_data.R"))
source(here("auxiliary", "fun_missing_var.R"))
source(here("auxiliary", "fun_low_variance.R"))
source(here("auxiliary", "fun_loadInputs.R"))

# Create benchmark graphs
source(here("auxiliary", "plots.R"))
source(here("auxiliary", "clean_plotly_legend.R"))
source(here("auxiliary", "fixfacets.R"))

#Functions that prepare data for download in different file formats
source(here("auxiliary", "fun_download_prep.R"))

#Functions that check for null data by indicator
source(here("auxiliary", "fun_check_data.R"))

#Functions that extracts variables 
source(here("auxiliary", "fun_extract_var.R"))

#Functions that remove aggregate average columns from datasets
source(here("auxiliary", "fun_remove_avg.R"))

# Function that displays publications
source(here("auxiliary", "fun_publications.R"))

# Modules
source(here("modules", "mod_publications.R"))

#Functions that prepare plotting settings
source(here("auxiliary","fun_plot_prep.R" ))

# Guide/help
source(here("auxiliary", "guides.R"))

#Use Bs4Dash Package post deprecation
source(here("auxiliary","useBs4Dash.R"))


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
      "closeness_to_frontier_dynamic.rds"
    )
  ) %>%
  filter(year <2024)%>%
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
      "closeness_to_frontier_dynamic_long.rds"
    )
  )

year_ctf_dynamic <-
  read_rds(
    here(
      "data",
      "year_coverage_ctf_for_analysis.rds"
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

country_list <- country_list %>% 
  rename(group = group_name)

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

#THESE ARE NOT CURRENTLY IN USE. REMOVE LATER?
#==========
# down_clust_ctf_stat_data <- read_rds(
#   here(
#     "data",
#     "closeness_to_frontier_confint_static.rds"
#   )
# )
# 
# down_clust_ctf_dyn_data <- read_rds(
#   here(
#     "data",
#     "closeness_to_frontier_confint_dynamic.rds"
#   )
# )
#==========
for(i in 1:nrow(clean_country)){
  if (clean_country[i,'Clean_Names']!=""){
    country_list$country_name[country_list$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
    ctf_long$country_name[ctf_long$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
    ctf_long_dyn$country_name[ctf_long_dyn$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
    # raw_data$country_name[raw_data$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
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

for (i in colnames(global_data_dyn)[5:length(colnames(global_data_dyn))]){
  name<-subset(db_variables$var_name,db_variables$variable==i)
  if(length(name)>0){
    name<-str_replace_all(name, "[[:punct:]]", "")
    attr(global_data_dyn[[i]],'label')<-name
  }}


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
  )%>%
  filter(
    family_var!="vars_other"
  )

countries <-
  raw_data %>%
  select(country_name) %>%
  filter(!(country_name %in% country_groups$group_name)) %>%
  unlist %>%
  unname %>%
  unique %>%
  sort

# Get flags for each country
country_flags_codes <- countrycode::countrycode(countries, "country.name.en", "ecb")

# Match "West Bank and Gaza" code for flagcdn
country_get_palestine <- c("West Bank and Gaza" = "PS")

# Function to get flags with countries
flags_with_countries <- mapply(function(country, code) {
  flag_html <- tags$img(src = paste0(src = 'https://flagcdn.com/w20/', tolower(code), '.png'), alt = code)
  label_html <- tags$span(country)
  paste(flag_html, label_html, sep = " ")
}, countries, ifelse(countries == "West Bank and Gaza", country_get_palestine["West Bank and Gaza"], countrycode::countrycode(countries, "country.name.en", "ecb")), SIMPLIFY = FALSE)


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

extract_variables_benchmarked <-
  function(x) {
    db_variables %>%
      filter(
        family_name == x, benchmarked_ctf=='Yes'
      ) %>%
      pull(var_name)
  }

variable_list_benchmarked <-
  lapply(family_names$var_name, extract_variables_benchmarked)

names(variable_list_benchmarked) <- family_names$var_name

# Exclude Monetary Stability - #296
variable_list_benchmarked$`Public Finance Institutions` <- variable_list_benchmarked$`Public Finance Institutions` [variable_list_benchmarked$`Public Finance Institutions`  != "Monetary stability"]


remove_average_items <- function(family) {
  family_filtered <- family[!grepl("Average", family)]
  return(family_filtered)
}

# Apply the function to each family in the list
filtered_variable_list <- lapply(variable_list, remove_average_items)


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
     
      tags$li(
        a(href = href, icon, text, class = "nav-link", target = "_blank"),
        class = "nav-item"
      )
  }


# Bivariate correlation ----------------------------------------------------------

## y axis variable choices  

y_scatter_choices <- append(
  "Log GDP per capita, PPP",
  variable_list
)


## x axis variable choices will be everything apart from the y axis variable selected
x_scatter_choices <- function(yvar){
  
extract_xvar_choices <-
  function(x, yvar) {
    db_variables %>%
      dplyr::filter(
        var_name != yvar
      ) %>% 
      dplyr::filter(
        family_name == x 
      ) %>%
      pull(var_name)
  }

xvar_choice_list <- purrr::map2(family_names$var_name, yvar, extract_xvar_choices)
names(xvar_choice_list) <- family_names$var_name

xvar_choice_list <- c("Log GDP per capita, PPP",xvar_choice_list)

return(xvar_choice_list)
}
