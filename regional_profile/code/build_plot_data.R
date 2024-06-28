# load packages -------------------------------------------------------------------------------------
packages <- c("tidyverse","here","sf")
pacman::p_load(packages,character.only = TRUE)

# options
options(scipen=999, dplyr.summarise.inform = FALSE)

# load cliar data -----------------------------------------------------------------------------------

# income classification; source: https://datahelpdesk.worldbank.org/knowledgebase/articles/378834-how-does-the-world-bank-classify-countries
income_class_fy24 <-  read_csv2(
  here(
    "regional_profile/data",
    "class_income_fy24.csv"
  )
)

db_variables <-
  read_rds(
    here(
      "app",
      "data",
      "db_variables.rds"
    )
  )

family_order <- read.csv(
  here(
    "app",
    "data",
    "family_order.csv"
  )
)

db_variables <- left_join(db_variables, family_order, by = "family_name")

source(here("regional_profile/aux", "vars-control.R"))
source(here("regional_profile/aux", "fun_quantiles.R"))
source(here("regional_profile/aux", "fun_family_data.R"))
source(here("regional_profile/aux", "fun_missing_var.R"))
source(here("regional_profile/aux", "fun_low_variance.R"))

global_data <-
  read_rds(
    here(
      "app",
      "data",
      "closeness_to_frontier.rds"
    )
  ) %>%
  ungroup 

raw_data <-
  read_rds(
    here(
      "app",
      "data",
      "compiled_indicators.rds"
    )
  ) %>%
  filter(year >= 1990,
         rowSums(!is.na(.)) > 3) %>%
  rename(Year = year) %>%
  mutate(Year = as.double(Year))

spatial_data <-
  read_rds(
    here(
      "app",
      "data",
      "indicators_map.rds"
    )
  )

country_groups <-
  read_rds(
    here(
      "app",
      "data",
      "wb_country_groups.rds"
    )
  )

country_list <-
  read_rds(
    here(
      "app",
      "data",
      "wb_country_list.rds"
    )
  )

clean_country <-
  read.csv(
    here(
      "app",
      "data",
      "Country_name_list.csv"
    )
  )  

for(i in 1:nrow(clean_country)){
  if (clean_country[i,'Clean_Names']!=""){
    country_list$country_name[country_list$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
    global_data$country_name[global_data$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
    spatial_data$country_name[spatial_data$country_name==clean_country[i,'Country']]=clean_country[i,'Clean_Names']
  }
} 

country_list = country_list[order(country_list$country_name, decreasing = FALSE), ]
global_data = global_data[order(global_data$country_name, decreasing = FALSE), ]
spatial_data = spatial_data[order(spatial_data$country_name, decreasing = FALSE), ]

st_crs(spatial_data) <- "+proj=robin"

# Load data control
db_variables <-
  db_variables %>%
  filter(variable %in% vars_all | var_level == "family")

for (i in colnames(global_data)[4:length(colnames(global_data))]){
  name<-subset(db_variables$var_name,db_variables$variable==i)
  if(length(name)>0){
    name<-str_replace_all(name, "[[:punct:]]", "")
    attr(global_data[[i]],'label')<-name
  }}

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

check_quantiles <- function(column) {
  q25 <- quantile(column, 0.25, na.rm = TRUE)
  q75 <- quantile(column, 0.75, na.rm = TRUE)
  if (!is.na(q25) & !is.na(q75) & q25 == q75) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}  

#write_rds(
#  spatial_data |> select(country_code),
#  here("regional_profile/data/countries_sf.rds")
#)

# Countries x Region ----------------------------------------------------------------------
## OVERVIEW -------------------------------------------------------------------------------

#comparison_group <- "High income"
#base_country <- "Antigua and Barbuda"

family <- "Overview"
threshold <- "Default"

bind_data <- tibble()

for(i in country_groups$group_name){
#for(i in "High income"){
  
  comparison_group <- i
  
  cat("\n",comparison_group,"\n\n")
  
  group_countries <- 
    country_list |>
    filter(group == comparison_group) |>
    pull(country_name)
  
  for(j in group_countries) {
  #for(j in "Antigua and Barbuda") {
    
    base_country <- j
    
    cat(base_country, "\n")
    
    comparison_countries <-
      country_list %>%
      filter(group %in% comparison_group) %>%
      select(country_name) %>%
      unique() %>%
      filter(country_name != base_country) %>%
      pluck(1)
    
    vars <-
      if (family == "Overview") {
        vars_all
      } else {
        variable_names |>
          filter(family_name == family) |>
          pull(variable) |>
          unique()
      }
    
    na_indicators <-
      global_data %>%
      ungroup() %>%
      filter(country_name %in% base_country) %>%
      select(-(1:5)) %>%
      summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
      select(where(is.na)) %>%
      distinct() %>%
      names 
    
    # List final relevant variables: those selected, minus those missing
    variables <-
      setdiff(vars, na_indicators) %>%
      intersect(names(global_data))
    
    # List specific family variables missing
    missing_variables <-
      vars[vars %in% na_indicators] %>%
      data.frame() %>%
      rename("variable"=".") %>%
      left_join(variable_names %>% select(variable,var_name), by = "variable") %>%
      filter(!is.na(var_name)) %>%
      .$var_name
    
    # This is the relevant data to be used
    low_variance_variables <-
      global_data %>%
      ungroup() %>%
      
      # Keep only the base and comparison countries
      filter(
        (country_name %in% comparison_countries) | (country_name == base_country)
      ) %>%
      
      # Keep only selected, non-missing indicators
      select(
        country_name,
        all_of(variables)
      )
      
    if (ncol(low_variance_variables) > 1){
      
      low_variance_variables <-
        low_variance_variables %>%
        # Make long per indicator
        pivot_longer(
          cols = all_of(variables),
          names_to = "variable"
        ) %>%
        
        #remove wdi_nygdppcapppkd variable 
        #filter(value <= 1) %>%
        
        # Add variables definition and family
        left_join(
          variable_names,
          by = "variable"
        ) %>%
        
        # Remove missing values
        filter(!is.na(value)) %>%
        
        # Calculate relevant indicators
        group_by(variable, var_name) %>%
        mutate(
          dtt = percent_rank(value),
          q25 = quantile(value, c(0.25)),
          q75 = quantile(value, c(0.75)),
          status = case_when(
            dtt <= .25 ~ "Weak\n(bottom 25%)",
            dtt > .25 & dtt <= .50 ~ "Emerging\n(25% - 50%)",
            dtt > .50 ~ "Strong\n(top 50%)"
          )
        ) %>%
        ungroup %>%
        rename(dtf = value) %>%
        filter(country_name == base_country & q25==q75) %>%
        select(variable) %>%
        unlist %>%
        data.frame() %>%
        rename("variable" = ".") %>%
        left_join(variable_names %>% select(variable, var_name), by = "variable") %>%
        .$var_name
      
      missing_variables <- c(missing_variables, low_variance_variables)
      
      data <-
        family_data(
          global_data,
          base_country,
          variable_names,
          comparison_countries
        ) %>%
        def_quantiles(
          base_country,
          country_list,
          comparison_countries,
          vars_family,
          family_names,
          threshold
        ) |>
        mutate(
          base_country = paste0(base_country),
          comparison_group = paste0(comparison_group)
        ) |>
        relocate(comparison_group,base_country) 
      
      # bind with other base country comparison
      bind_data <- bind_rows(bind_data, data)
      
    }
    
  }

}

bind_data <- bind_data |>
  filter(dtf <= 1)

## export data ----
#write_csv2(
#  bind_data,
#  here(
#    "regional_profile/data",
#    paste0(
#      "countries_vs_region.csv"
#    )
#  )
#)

#write_rds(
#  bind_data,
#  here(
#    "data/data_report",
#    paste0(
#      "countries_vs_region.rds"
#    )
#  )
#)

## CLUSTERS -------------------------------------------------------------------------------

#family <- "Labor and Social Protection Institutions"
#comparison_group <- "Latin America & Caribbean"
#base_country <- "Brazil"

threshold <- "Default"

if (threshold=="Default"){
  cutoff<-c(25,50)
}else if (threshold=="Terciles")
{
  cutoff<-c(33,66)
}

bind_data <- tibble()

for (k in family_names$var_name){

  family <- k
  
  cat("\n",family,"\n\n")
  
  for(i in country_groups$group_name){
    #for(i in "High income"){
    
    comparison_group <- i
    
    cat("\n",comparison_group,"\n\n")
    
    group_countries <- 
      country_list |>
      filter(group == comparison_group) |>
      pull(country_name)
    
    for(j in group_countries) {
      #for(j in "Antigua and Barbuda") {
      
      base_country <- j
      
      #cat(base_country, "\n")
      
      comparison_countries <-
        country_list %>%
        filter(group %in% comparison_group) %>%
        select(country_name) %>%
        unique() %>%
        filter(country_name != base_country) %>%
        pluck(1)
      
      vars <-
        if (family == "Overview") {
          vars_all
        } else {
          variable_names |>
            filter(family_name == family) |>
            pull(variable) |>
            unique()
        }
      
      na_indicators <-
        global_data %>%
        ungroup() %>%
        filter(country_name %in% base_country) %>%
        select(-(1:5)) %>%
        summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
        select(where(is.na)) %>%
        distinct() %>%
        names 
      
      # List final relevant variables: those selected, minus those missing
      variables <-
        setdiff(vars, na_indicators) %>%
        intersect(names(global_data))
      
      # List specific family variables missing
      missing_variables <-
        vars[vars %in% na_indicators] %>%
        data.frame() %>%
        rename("variable"=".") %>%
        left_join(variable_names %>% select(variable,var_name), by = "variable") %>%
        filter(!is.na(var_name)) %>%
        .$var_name
      
      # This is the relevant data to be used
      low_variance_variables <-
        global_data %>%
        ungroup() %>%
        
        # Keep only the base and comparison countries
        filter(
          (country_name %in% comparison_countries) | (country_name == base_country)
        ) %>%
        
        # Keep only selected, non-missing indicators
        select(
          country_name,
          all_of(variables)
        )
      
      if (ncol(low_variance_variables) > 1){
        
        low_variance_variables <-
          low_variance_variables %>%
          # Make long per indicator
          pivot_longer(
            cols = all_of(variables),
            names_to = "variable"
          ) %>%
          
          #remove wdi_nygdppcapppkd variable 
          #filter(value <= 1) %>%
          
          # Add variables definition and family
          left_join(
            variable_names,
            by = "variable"
          ) %>%
          
          # Remove missing values
          filter(!is.na(value)) %>%
          
          # Calculate relevant indicators
          group_by(variable, var_name) %>%
          mutate(
            dtt = percent_rank(value),
            q25 = quantile(value, c(0.25)),
            q75 = quantile(value, c(0.75)),
            status = case_when(
              dtt <= .25 ~ "Weak\n(bottom 25%)",
              dtt > .25 & dtt <= .50 ~ "Emerging\n(25% - 50%)",
              dtt > .50 ~ "Strong\n(top 50%)"
            )
          ) %>%
          ungroup %>%
          rename(dtf = value) %>%
          filter(country_name == base_country & q25==q75) %>%
          select(variable) %>%
          unlist %>%
          data.frame() %>%
          rename("variable" = ".") %>%
          left_join(variable_names %>% select(variable, var_name), by = "variable") %>%
          .$var_name
        
        missing_variables <- c(missing_variables, low_variance_variables)
        
        static_avg_data <-
          global_data |>
          select(-matches("_avg"))
        
        vars_static_avg_data <- names(static_avg_data)[6:length(static_avg_data)] 
        
        static_avg <- 
          compute_family_average(
            static_avg_data,
            vars_static_avg_data,
            "static",
            db_variables,
            base_country,
            comparison_countries
          ) |>
          select(-matches('NA'))
        
        data <-
          static_avg_data |>
          left_join(static_avg, by='country_code') |>
          ungroup() |>
          filter(
            country_name %in% c(base_country, comparison_countries)
          ) |>
          select(
            country_name,
            any_of(variables)
          ) |>
          # Make long per indicator
          pivot_longer(
            cols = any_of(variables),
            names_to = "variable"
          ) |>
          # Add variables definition and family
          left_join(
            variable_names,
            by = "variable"
          ) |>
          filter(!is.na(value)) |>
          # Calculate relevant indicators
          group_by(variable, var_name) |>
          mutate(
            dtt = percent_rank(value),
            q_lv_25 = quantile(value,c(0.25)),
            q_lv_75 = quantile(value,c(0.75)),
            q_cutoff1 = quantile(value, c(cutoff[1]/100)),
            q_cutoff2 = quantile(value, c(cutoff[2]/100)),
            status = case_when(
              dtt <= cutoff[1]/100 ~ paste0("Weak\n(bottom ", cutoff[1],"%)"),
              dtt > cutoff[1]/100 & dtt <= cutoff[2]/100 ~ paste0("Emerging\n(",cutoff[1],"% - ",cutoff[2],"%)"),
              dtt > cutoff[2]/100 ~ paste0("Strong\n(top ",100-cutoff[2],"%)")
            ),
            nrank = min_rank(-value)
          ) |>
          ungroup() |>
          rename(dtf = value) |>
          filter(variable %in% vars) |>
          mutate(
            base_country = paste0(base_country),
            comparison_group = paste0(comparison_group)
          ) |>
          relocate(comparison_group,base_country) 
        
        # bind with other base country comparison
        bind_data <- bind_rows(bind_data, data)
        
      }
      
    }
    
  }
  
}

bind_data <- bind_data |>
  filter(dtf <= 1)

## export data ----
write_csv2(
  bind_data,
  here(
    "regional_profile/data",
    paste0(
      "countries_vs_region_clusters.csv"
    )
  )
)