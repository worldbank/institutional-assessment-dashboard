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

#data_original <- read_rds(here("data",
#                               "data_cleaned",
#                               "merged_for_residuals.rds"))

# IDs from selected indicators
selected_indicators <- c(
  290,   # Corruption / Percent of firms identifying the courts system as a major constraint
  472	Registering property: Cost
  477	Enforcing contracts: Cost
  482	Resolving insolvency: Cost
  519,   # Extent of market dominance
  633,   # Property rights (WEF)
  663,   # Diversion of public funds
  665,   # Public trust in politicians
  667,   # Irregular payments and bribes
  671,   # Favoritism in decisions of government officials
  683,   # Wastefulness of government spending
  687,   # Transparency of government policymaking
  691	Efficiency of legal framework in challenging regs
  697	Effectiveness of antimonopoly policy
  719	Prevalence of trade barriers
  723	Burden of customs procedures
  747	Index of economic freedom score
  3311,	 # Price controls
  3285,	 # Foreign Currency Regulations
  3289,	 # Restrictive Labor Regulations
  3305	Administrative burdens on startups
  3307	Explicit barriers to trade and investment
  3308	Other barriers to trade and investment
  3311	Price controls
  3323	Complexity of regulatory procedures
  3326	Governance of state-owned enterprises
  3328	Regulatory protection of incumbents
  3469,  # E-Participation Index, 0-1 (best)
  24840	Paying taxes: Time
  27885	Publicized laws and government data
  27885, # Publicized laws and government data
  30823	Central Bank independence
  31001	Efficiency of the banking supervisory authority
  31003	Efficiency of the financial market supervisory authority
  31088	Financial sector: competition regulation
  31115, # Freedom of entry for foreigners
  40985, # Civil Liberties
  40986, # Political Rights
  41008	Burden of government regulation, 1-7 (best)
  41794, # Absence of corruption (Global States of Democracy)
  41827, # Civil society participation
  41881  # Engaged society
  41932	Fundamental rights
  41951	Judicial accountability
  41953	Judicial independence
  41981	Lower chamber female legislators
  42025	Power distributed by social group
  42026	Power distributed by socio-economic position
  42084	Rigorous and impartial public administration
)

# Get data360
data_api <- get_data360(
  indicator_id = selected_indicators,
  output_type = 'long')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# BASIC CLEANING ---------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

data_cleaned <-
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
  mutate(
    year = ifelse(is.na(year),as.character(Period),year)
  ) %>%
  select(
    -c(`Subindicator Type`,Period,Year1)
  ) %>%
  filter(
    !is.na(value)
  ) %>%
  complete(
    nesting(country_code, country_name),
    nesting(Indicator, year),
    fill = list(value = NA)
  ) %>%
  mutate(
    var = case_when(
      Indicator == "Favoritism in decisions of government officials, 1-7 (best)" ~ "favoritism",
      Indicator == "Irregular payments and bribes" ~ "bribes",
      Indicator == "Diversion of public funds" ~ "diversion_pfunds",
      Indicator == "Transparency of government policymaking" ~ "transparency_polmak",
      Indicator == "Property rights (WEF)" ~ "property_rights",
      Indicator == "Extent of market dominance" ~ "mkt_dominance",
      Indicator == "Civil society participation" ~ "v2x_cspart",
      Indicator == "Engaged society" ~ "v2dlengage",
      Indicator == "Public trust in politicians" ~ "trust_pol",
      Indicator == "Absence of corruption (Global States of Democracy)" ~ "f2_corruption",
      Indicator == "E-Participation Index, 0-1 (best)" ~ "eparticipationindex",
      Indicator == "Publicized laws and government data"  ~ "open_data_barometer",
      Indicator == "Corruption / Percent of firms identifying the courts system as a major constraint" ~ "es_court_constraint",
      Indicator == "Freedom of entry for foreigners" ~ "efw_tourist",
      Indicator == "Restrictive Labor Regulations" ~ "efw_labor_mkt_reg",
      Indicator == "Foreign Currency Regulations" ~ "efw_free_foreign_curr",
      Indicator == "Political Rights" ~ "e_fh_pr",
      Indicator == "Price controls" ~ "price_controls",
      Indicator == "Civil Liberties" ~ "e_fh_cl",
      Indicator == "Wastefulness of government spending" ~ "eff_govspending"
    )
  ) %>%
  pivot_wider(
    id_cols = c(country_name,country_code,year),
    names_from = var
  )

# Export cleaned data
write_rds(data_cleaned,
          file.path("data",
                    "raw_data.rds"))
