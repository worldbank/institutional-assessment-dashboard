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
#
#gov_indicators <- get_metadata360(site="gov", metadata_type = "indicators")
#tc_indicators <- get_metadata360(site="tc", metadata_type = "indicators")
#
#indicators_360 <- bind_rows(gov_indicators,tc_indicators)

# IDs from selected indicators
selected_indicators_1 <- c(
  290,   # Corruption / Percent of firms identifying the courts system as a major constraint
  464,   #  Dealing with construction permits: Procedures
  472,   # Registering property: Cost
  477,	   # Enforcing contracts: Cost
  #482	Resolving insolvency: Cost                    # ONLY ON DEFINITIONS
  519,   # Extent of market dominance
  633,   # Property rights (WEF)
  663,   # Diversion of public funds
  665,   # Public trust in politicians
  667,   # Irregular payments and bribes
  671,   # Favoritism in decisions of government officials
  683,   # Wastefulness of government spending
  685,   # Efficiency of legal framework in settling disputes
  687,   # Transparency of government policymaking
  691,   #	Efficiency of legal framework in challenging regs
  697,   #	Effectiveness of antimonopoly policy
  #719,   #	Prevalence of trade barriers
  723,	 # Burden of customs procedures
  747,   # Index of economic freedom score          # ONLY ON DEFINITIONS
  3210,  #  Consolidated regulatory governance score
  3311,	 # Price controls
  3285,	 # Foreign Currency Regulations               # DIFFERENT VALUES
  3451,  # Government Online Service Index
  3276,  # Corruption
  3289,	 # Restrictive Labor Regulations
  3305,	 # Administrative burdens on startups
  3307,	 # Explicit barriers to trade and investment
  3308,	 # Other barriers to trade and investment
  3310,  #  Use of command and control regulation
  3311,	 # Price controls
  3323,	 # Complexity of regulatory procedures
  3326,	 # Governance of state-owned enterprises
  3328,	 # Regulatory protection of incumbents
  3469  # E-Participation Index, 0-1 (best)
)

# DIVIDED BECAUSE API WASNT ABLE TO GET ALL TOGETHER
selected_indicators_2 <- c(
  24840, # Paying taxes: Time
  27470, # Revised Combined Polity Score
  27885, # Publicized laws and government data
  27919, # People can access and afford civil justice
  28833, #  Resolving insolvency Outcome
  #30823, # Central Bank independence                # NO RECENT DATA
  31001, #  Efficiency of the banking supervisory authority
  31003, #  Efficiency of the financial market supervisory authority
  31088, #  Financial sector: competition regulation
  31115, #  Freedom of entry for foreigners
  40294, #  Ease of starting a business
  40432, #  Ease of protecting minority investors
  40418, #  Ease of getting credit
  #40985 #  Civil Liberties                    # STRANGE VALUES
  #40986, # Political Rights                           # STRANGE VALUES
  41008, #	Burden of government regulation, 1-7 (best)
  41619, #  GCI 4.0: Global Competitiveness Index 4.0
  41714, #  GCI 4.0: Efficiency of the clearance process
  41794, #  Absence of corruption (Global States of Democracy)
  41827, #  Civil society participation
  41881, #  Engaged society
  41883, #  Executive constraints
  41189, #  Procurement
  41932, #  Fundamental rights
  41951 #  Judicial accountability
  #41953 #	Judicial independence       # ONLY ON DEFINITIONS
)

selected_indicators_3 <- c(
  41955, #  Law  and order
  41981, #  Lower chamber female legislators
  42024, #  Power distributed by gender
  42025, #  Power distributed by social group
  42026, #  Power distributed by socio-economic position
  42084, #	Rigorous and impartial public administration
  42602, #  Resolving insolvency: Strength of insolvency framework index
  43050  #  GCI 4.0: Border clearance efficiency
)

# Get data360
data_api_1 <- get_data360(
  indicator_id = selected_indicators_1,
  output_type = 'long')

data_api_2 <- get_data360(
  indicator_id = selected_indicators_2,
  output_type = 'long')

data_api_3 <- get_data360(
  indicator_id = selected_indicators_3,
  output_type = 'long')

data_api <- bind_rows(data_api_1,data_api_2,data_api_3)

rm(data_api_2,selected_indicators_2, data_api_3, selected_indicators_3,data_api_1,selected_indicators_1)

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
      Indicator == "Foreign Currency Regulations" ~ "efw_free_foreign_curr",        # DIFFERENT VALUES
      #Indicator == "Political Rights" ~ "e_fh_pr",                     # STRANGE VALUES
      Indicator == "Price controls" ~ "price_controls",
      #Indicator == "Civil Liberties" ~ "e_fh_cl",                       # STRANGE VALUES
      Indicator == "Wastefulness of government spending" ~ "eff_govspending",
      Indicator == "Registering property: Cost" ~ "register_prop_overall",
      Indicator == "Enforcing contracts: Cost" ~ "enf_contr_overall",
      Indicator == "Effectiveness of antimonopoly policy" ~ "eff_antimonopoly",
      Indicator == "Explicit barriers to trade and investment" ~ "barriers_trade_expl",
      Indicator == "Burden of customs procedures" ~ "customs_burden",
      Indicator == "Index of economic freedom score" ~ "wsj_financialfreedom",
      Indicator == "Administrative burdens on startups" ~ "barriers_startups",
      Indicator == "Other barriers to trade and investment" ~ "barriers_trade_oth",
      Indicator == "Complexity of regulatory procedures" ~ "complexity_procedures",
      Indicator == "Governance of state-owned enterprises" ~ "governance_soe",
      Indicator == "Regulatory protection of incumbents" ~ "protection_incumbents",
      Indicator == "Paying taxes: Time" ~ "pay_taxes_overall",
      Indicator == "Publicized laws and government data" ~ "open_data_barometer",
      #Indicator == "Central Bank independence" ~ "cbi",
      Indicator == "Efficiency of the banking supervisory authority" ~ "efficiency_superv_bank",
      Indicator == "Efficiency of the financial market supervisory authority" ~ "efficiency_superv_fin",
      Indicator == "Financial sector: competition regulation" ~ "competition_rules_fin",
      Indicator == "Burden of government regulation, 1-7 (best)" ~ "govreg_burden",
      Indicator == "Fundamental rights" ~ "f4_rights",
      Indicator == "Judicial accountability" ~ "v2juaccnt",
      Indicator == "Lower chamber female legislators" ~ "v2lgfemleg",
      Indicator == "Power distributed by social group" ~ "v2pepwrsoc",
      Indicator == "Power distributed by socio-economic position" ~ "v2pepwrses",
      Indicator == "Rigorous and impartial public administration" ~ "rigorous_impartial_pa",
      Indicator == "Revised Combined Polity Score" ~ "e_p_polity",
      Indicator == "Executive constraints" ~ "f1_govpowers",
      Indicator == "Power distributed by gender" ~ "v2pepwrgen",
      Indicator == "Law  and order" ~ "f3_security",
      Indicator == "Corruption" ~ "e_ti_cpi",
      Indicator == "Government Online Service Index, 0-1 (best)" ~ "egovernmentindex",
      Indicator == "Procurement" ~ "proc_mean_score",
      Indicator == "Consolidated regulatory governance score" ~ "regulatory_governance",
      Indicator == "People can access and afford civil justice" ~ "f7_civiljustice",
      Indicator == "Efficiency of legal framework in challenging regs" ~ "legaleff_challenging",
      Indicator == "Efficiency of legal framework in settling disputes" ~ "legaleff_disputes",
      Indicator == "Resolving insolvency: Outcome" ~ "resolve_insolv_overall",
      Indicator == "GCI 4.0: Global Competitiveness Index 4.0" ~ "gci_overall",
      Indicator == "GCI 4.0: Efficiency of the clearance process" ~ "lpi_clearance_eff",
      Indicator == "GCI 4.0: Border clearance efficiency" ~ "wef_border_admin",
      Indicator == "Ease of starting a business" ~ "start_bus_overall",
      Indicator == "Dealing with construction permits: Procedures" ~ "constr_perm_overall",
      Indicator == "Ease of protecting minority investors" ~ "protect_minority_ov",
      Indicator == "Ease of getting credit" ~ "access_credit_overall",
      Indicator == "Resolving insolvency: Strength of insolvency framework index" ~ "insolvency_framework",
      Indicator == "Use of command and control regulation" ~ "command_control"
    )
  ) %>%
  pivot_wider(
    id_cols = c(country_name,country_code,year),
    names_from = var
  ) %>%
  # SC: methodological note for PRM indicates that 1998 and 2013 indicators are comparable, but not with 2018 due to change in methodology
  # --> drop if year ==2018
  mutate(
    across(
      c(barriers_startups:protection_incumbents),
      ~ifelse(year==2018, NA, .x)
    )
  ) %>%
  # Fix vars with opposite scale
  # reason: for the CTF methodology, we need for all indicators that "higher values" means "better performance"
  # freedom house: Countries are graded between 1 (most free) and 7 (least free).
  #mutate(
  #  across(
  #    c(e_fh_pr,e_fh_cl),
  #    ~(8 - .x)
  #  )
  #) %>%
  # transform missing
  mutate(
    e_p_polity = ifelse(e_p_polity < -10, NA, e_p_polity)
  ) %>%
  # PRM indicators: Countries are graded between 0 (less control/involvement) and 6 (more control/involvement)
  mutate(
    across(
      c(
        governance_soe,
        price_controls,
        command_control,
        complexity_procedures,
        barriers_startups,
        protection_incumbents,
        barriers_trade_expl,
        barriers_trade_oth
      ),
      ~(6 - .x)
    )
  ) %>%
  filter(
    year >= 1950   # variable e_p_polity has values since 1800, filter to reduce # of rows
  )

# Export cleaned data
write_rds(data_cleaned,
          file.path("data",
                    "raw_data.rds"))
