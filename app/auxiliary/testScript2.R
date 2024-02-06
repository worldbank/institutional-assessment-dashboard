data = global_data
Category = "Custom"
Grp = c("GRP1", "GRP1","GRP1", "GRP2", "GRP2", "GRP2", "GRP2", "GRP2", "GRP3", "GRP3", "GRP3", "GRP3")
Countries <- c("Denmark", "Russian Federation", "Sweden", "Tajikistan",
  "Thailand", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Uzbekistan", "Venezuela, RB",
  "Vietnam", "Yemen, Rep.")

custom_df <- data.frame(Category, Grp, Countries)

base_country <- "Angola"
comparison_countries = country_list %>%
  filter(group == "Sub-Saharan Africa" & country_name != base_country) %>%
  distinct(country_name) %>%
  pull()
comparison_countries = NULL
var = "Right to information"

groups = c("Sub-Saharan Africa", unique(custom_df$Grp)[1:3])

# data = global_data
# base_country =  input$country_bar
# comparison_countries = input$countries_bar
# groups = input$groups_bar
# var = input$vars_bar
# variable_names = variable_names
# custom_df = custom_df()

# static_bar(
#   global_data,
#   base_country,
#   comparison_countries,
#   groups,
#   var,
#   variable_names,
#   custom_df
# )
# 
# country_list %>%
#   filter(group %in% "Sub-Saharan Africa") %>%
#   select(group, country_code)
# 
# x <- country_list %>%
#   filter(country_name %in% custom_df$Countries) %>%
#   select(group, country_code)

raw_data = raw_data
vars_trends = "Absence of corruption"
var = 
  db_variables %>%
  filter(var_name == vars_trends) %>%
  pull(variable)

country_trends = base_country
countries_trends = NULL
country_list = country_list
group_trends = groups
db_variables = db_variables
custom_df_trend = custom_df


indicator = var
indicator_name = vars_trends
base_country
comparison_countries
country_list
groups = groups
definitions = db_variables
custom_df

trends_plot(
  raw_data,
  var,
  vars_trends,
  country_trends,
  countries_trends,
  country_list,
  group_trends,
  db_variables,
  custom_df_trend
)


x <- custom_df %>% 
      group_by(Grp) %>% 
      mutate(note = paste0(Grp, " (", paste0(Countries, collapse = " , ") , ")")) %>% 
      distinct(note) %>% 
      pull() %>% 
      paste0(., collapse = " ; ")





x <- family_data(
  global_data,
  base_country(),
  variable_names
)

data = x
base_country = base_country()
country_list = country_list
comparison_countries = input$countries
vars = vars_family
variable_names = family_names
threshold = input$threshold

plot_notes_function(y, z, tab_name, miss_var, plot_type, custom_df) 

plot_notes_function(
  base_country(),
  note_compare(),
  input$family,
  missing_variables,
  "static",
  custom_df = custom_df()
  
)
interactive_plot(x, tab_name, buttons,  plot_type) 


interactive_plot(
  base_country(),
  note_compare(),
  input$family,
  plotly_remove_buttons,
  missing_variables,
  "dynamic",
  custom_df = custom_df()
)

data =  global_data_dyn %>%
  def_quantiles_dyn(
    base_country(),
    country_list,
    input$countries,
    vars_all,
    variable_names,
    input$threshold
  )%>%
  filter(variable %in% vars()) 

cutoff<-c(25,50)
base_country = base_country()
tab_name = input$family
rank = input$rank
group_median = input$benchmark_median
custom_df = NULL ## New addition made by Shel in August 2023 to accommodate custom groups
title = TRUE
dots = FALSE
note = NULL
threshold = input$threshold
preset_order = input$preset_order

data =  global_data %>%
  def_quantiles(
    base_country(),
    country_list,
    input$countries,
    vars_all,
    variable_names,
    input$threshold
  )

data = global_data
base_country  = "Angola"
comparison_countries  = high_group()

Category = "Custom"
Grp = c("GRP1", "GRP1","GRP1", "GRP2", "GRP2", "GRP2", "GRP2", "GRP2", "GRP3", "GRP3", "GRP3", "GRP3")
Countries <- c("Denmark", "Russian Federation", "Sweden", "Tajikistan",
  "Thailand", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Uzbekistan", "Venezuela, RB",
  "Vietnam", "Yemen, Rep.")

custom_df <- data.frame(Category, Grp, Countries)

base_country <- "Angola"
comparison_countries = custom_df %>%
  filter(Grp == "GRP1" & Countries != base_country) %>%
  distinct(Countries) %>%
  pull()


  custom_df_data <- custom_df %>%
    filter(Grp %in% "GRP1") %>% 
    select(Grp, Countries) %>% 
    rename(group = Grp, 
      country_name = Countries) %>% 
    left_join(., country_list %>% distinct(country_code, country_name), by = c("country_name" ))
  
  high_group <- custom_df_data


high_group_df <-  country_list %>%
  filter(group %in% input$high_group) %>%
  select(group, country_code)

y_scatter  = "Log GDP per capita, PPP"
x_scatter = 'Firms identifying corruption as a major constraint'
variable_names  = variable_names
country_list = country_list
linear_fit = TRUE

static_scatter(
  global_data,
  input$country_scatter,
  input$countries_scatter,
  high_group(),
  input$y_scatter,
  input$x_scatter,
  variable_names,
  country_list,
  input$linear_fit
) %>%
  interactive_scatter(
    input$y_scatter,
    input$x_scatter,
    db_variables,
    high_group(),
    plotly_remove_buttons
  )
