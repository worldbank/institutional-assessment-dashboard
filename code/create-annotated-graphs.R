plotly_remove_buttons <-
  c("zoomIn2d",
    "zoomOut2d",
    "pan2d",
    "autoScale2d",
    "lasso2d",
    "select2d",
    "toggleSpikelines",
    "hoverClosest3d",
    "hoverClosestCartesian",
    "hoverCompareCartesian")

## Auxiliary functions -----------------------------------------------------------------

source(file.path("auxiliary",
                 "vars-by-family.R"))

# Function that defines quantiles based on country, comparison and variables
source(file.path("auxiliary",
                 "fun_quantiles.R"))

source(file.path("auxiliary",
                 "fun_family_data.R"))

# Create benchmark graphs
source(file.path("auxiliary",
                 "plots.R"))

## Data sets ----------------------------

# Indicator definitions
definitions <-
  read_rds(file.path("data",
                     "indicator_definitions.rds"))

all_indicators <-
  read_rds(file.path("data",
                     "list_of_indicators.rds"))

# Closeness to frontier data
global_data <-
  read_rds(file.path("data",
                     "country_dtf.rds"))

wb_country_geom_fact <-
  read_rds(file.path("data",
                     "wb_country_geom_fact.rds"))

st_crs(wb_country_geom_fact) <- "WGS84"

# Raw data
raw_data <-
  read_rds(file.path("data",
                     "raw_data.rds")) %>%
  filter(year >= 1990,
         rowSums(!is.na(.)) > 3) %>%
  rename(Year = year)

# Metadata
variable_names <-
  read_rds(file.path("data",
                     "variable_names.rds"))

country_list <-
  read_rds(file.path("data",
                     "wb_country_list.rds"))

color_groups <- colorRampPalette(c("#053E5D", "#60C2F7"))

base_country <-
  function(x) {"Uruguay"}

input <-
  data.frame(
    "family" = "Overview",
    "groups" = c("OECD members", "LAC6")
  )

family_data(
        global_data,
        base_country(),
        variable_names
      ) %>%
        def_quantiles(
          base_country(),
          country_list,
          input$groups,
          family_names,
          variable_names
        ) %>%
  static_plot(base_country(),
              input$family) %>%
  interactive_plot(base_country(),
                   input$groups,
                   input$family,
                   plotly_remove_buttons)
