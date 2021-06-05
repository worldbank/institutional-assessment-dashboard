# Load packages
packages <- c("tidyverse",
              "here",
              "skimr",
              "data360r")

pacman::p_load(packages,
               character.only = TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# IMPORT DATA 360 ----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

source(file.path("app/auxiliary",
                 "vars-by-family.R"))

# IDs from selected indicators
selected_indicators <- c(
  671, # Favoritism in decisions of government officials
  667, # Irregular payments and bribes
  663, # Diversion of public funds
  687, # Transparency of government policymaking
  633, # Property rights (WEF)
  519  # Extent of market dominance
)

# Get data360
data_api <- get_data360(
  indicator_id = selected_indicators,
  output_type = 'long')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# BASIC CLEANING ---------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

data_api <-
  data_api %>%
  rename(
    country_code = `Country ISO3`,
    country_name = `Country Name`,
    value = Observation
  ) %>%
  arrange(
    Indicator,country_name
  ) %>%
  separate(
    Period,into = c("Year1","year"),sep="-",remove = F
  ) %>%
  select(
    -c(`Subindicator Type`,Period,Year1)
  ) %>%
  mutate(
    var = case_when(
      Indicator == "Favoritism in decisions of government officials, 1-7 (best)" ~ "favoritism",
      Indicator == "Irregular payments and bribes" ~ "bribes",
      Indicator == "Diversion of public funds" ~ "diversion_pfunds",
      Indicator == "Transparency of government policymaking" ~ "transparency_polmak",
      Indicator == "Property rights (WEF)" ~ "property_rights",
      Indicator == "Extent of market dominance" ~ "mkt_dominance"
    )
  ) %>%
  pivot_wider(
    id_cols = c(country_name,country_code,year),
    names_from = var
  )

# Export cleaned data
write_rds(data_api,
          here("data",
               "data_cleaned",
               "selected_vars.rds"))
