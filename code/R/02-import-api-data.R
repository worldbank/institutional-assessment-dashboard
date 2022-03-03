# Load packages
packages <- c("tidyverse",
              "here",
              "data360r",
              "skimr")

pacman::p_load(packages,
               character.only = TRUE)

# GET INDICATORS FROM API 360 ----
#gov_indicators <- get_metadata360(site="gov", metadata_type = "indicators")
#tc_indicators <- get_metadata360(site="tc", metadata_type = "indicators")

#indicators_360 <- bind_rows(gov_indicators,tc_indicators)

governance_data <- c(
  42099, # SOE board of directors independence (Mining)
  42100, # SOE board of directors independence (Oil & Gas)
  #42105, # SOE corporate governance practice (Mining)
  #42106, # SOE corporate governance practice (Oil & Gas)
  42161, # SOE-government transfers governance rule (Mining)
  42162, # SOE-government transfers governance rule (Oil & Gas)
  42092, # SOE annual report disclosure (Oil & Gas)
  42091, # SOE annual report disclosure (Mining)
  42107, # SOE financial audit requirement (Mining)
  42108, # SOE financial audit requirement (Oil & Gas)
  42141, # SOE report legislative review requirement (Mining)
  42142  # SOE report legislative review requirement (Oil & Gas)
)

# Get data360
indicator_ids <-
  db_variables %>%
    filter(
      select==1 &
      !is.na(api_id)
    ) %>%
    .$api_id

indicator_ids <- split(indicator_ids,
  ceiling(
    seq_along(indicator_ids)/10
  )
)

datalist = list()

for(i in 1:length(indicator_ids)){
  datalist[[i]] <- data360r::get_data360(
           indicator_id = c(indicator_ids[[i]]),
           output_type = 'long'
          )
}

data_api_raw = do.call(dplyr::bind_rows , datalist)

data_api_governance <- data360r::get_data360(
  indicator_id = governance_data,
  output_type = 'long')

data_api_governance <- data_api_governance %>%
  mutate(
    Indicator = case_when(
      Indicator == "SOE board of directors independence (Mining)" ~ "SOE board of directors independence",
      Indicator == "SOE board of directors independence (Oil & Gas)" ~ "SOE board of directors independence",
      #Indicator == "SOE corporate governance practice (Mining)" ~ "SOE corporate governance practice",
      #Indicator == "SOE corporate governance practice (Oil & Gas)" ~ "SOE corporate governance practice",
      Indicator == "SOE-government transfers governance rule (Mining)" ~ "SOE-government transfers governance rule",
      Indicator == "SOE-government transfers governance rule (Oil & Gas)" ~ "SOE-government transfers governance rule",
      Indicator == "SOE annual report disclosure (Oil & Gas)" ~ "SOE annual report disclosure",
      Indicator == "SOE annual report disclosure (Mining)" ~ "SOE annual report disclosure",
      Indicator == "SOE financial audit requirement (Mining)" ~ "SOE financial audit requirement",
      Indicator == "SOE financial audit requirement (Oil & Gas)" ~ "SOE financial audit requirement",
      Indicator == "SOE report legislative review requirement (Mining)" ~ "SOE report legislative review requirement",
      Indicator == "SOE report legislative review requirement (Oil & Gas)" ~ "SOE report legislative review requirement"
    )
  ) %>%
  group_by(`Country ISO3`,`Country Name`,Indicator,Period) %>%
  summarise(Observation = mean(Observation, na.rm=T))

data_api <-
  bind_rows(
    data_api_raw,
    data_api_governance
  ) %>%
  mutate(
    Indicator = str_trim(Indicator)
  )

# Cleaning API data ----
data_api <- data_api %>%
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
  #left_join(
  #  db_variables %>%
  #    filter(
  #      select==1 &
  #      data_source == "api" &
  #      !is.na(api_id)
  #    ) %>%
  #    select(var_name,variable),
  #  by=c("Indicator"="var_name")
  #) %>%
  mutate(
    var = case_when(
      Indicator == "SOE board of directors independence" ~ "soe_board",
      #Indicator == "SOE corporate governance practice" ~ "soe_corporate",
      Indicator == "SOE-government transfers governance rule" ~ "soe_government",
      Indicator == "SOE annual report disclosure" ~ "soe_annual_report",
      Indicator == "SOE financial audit requirement" ~ "soe_financial",
      Indicator == "SOE report legislative review requirement" ~ "soe_report_legislative",
      #Indicator == "Steering Capability" ~ "steering_capability",
      Indicator == "Hiring and firing practices, 1-7 (best)" ~ "hiring_pract",
      Indicator == "Freedom of opinion and expression is effectively guaranteed" ~ "opinion_freedom",
      Indicator == "Freedom of academic and cultural expression" ~ "v2clacfree",
      Indicator == "Complaint mechanisms" ~ "complaint_mechan",
      Indicator == "Right to information" ~ "right_to_info",
      #Indicator == "GCI 4.0: 1.E Undue influence and corruption" ~ "undue_influ_corrupt",
      Indicator == "CSO entry and exit" ~ "v2cseeorgs",
      Indicator == "3. Undue influence" ~ "undue_incluence",
      Indicator == "Burden of customs procedures, 1-7 (best)" ~ "burden_cust_proc",
      Indicator == "GCI 4.0: 7.A Domestic competition" ~ "domestic_competition",
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
      #Indicator == "E-Participation Index, 0-1 (best)" ~ "eparticipationindex",
      #Indicator == "Publicized laws and government data"  ~ "open_data_barometer",
      Indicator == "Corruption / Percent of firms identifying the courts system as a major constraint" ~ "es_court_constraint",
      #Indicator == "Freedom of entry for foreigners" ~ "efw_tourist",
      Indicator == "Restrictive Labor Regulations" ~ "efw_labor_mkt_reg",
      #Indicator == "Foreign Currency Regulations" ~ "efw_free_foreign_curr",        # DIFFERENT VALUES
      #Indicator == "Political Rights" ~ "e_fh_pr",                     # STRANGE VALUES
      Indicator == "Price controls" ~ "price_controls",
      #Indicator == "Civil Liberties" ~ "e_fh_cl",                       # STRANGE VALUES
      Indicator == "Wastefulness of government spending" ~ "eff_govspending",
      #Indicator == "Registering property: Cost" ~ "register_prop_overall",
      #Indicator == "Enforcing contracts: Cost" ~ "enf_contr_overall",
      Indicator == "Effectiveness of antimonopoly policy" ~ "eff_antimonopoly",
      Indicator == "Explicit barriers to trade and investment" ~ "barriers_trade_expl",
      Indicator == "Burden of customs procedures" ~ "customs_burden",
      #Indicator == "Index of economic freedom score" ~ "wsj_financialfreedom",
      Indicator == "Administrative burdens on startups" ~ "barriers_startups",
      Indicator == "Other barriers to trade and investment" ~ "barriers_trade_oth",
      Indicator == "Complexity of regulatory procedures" ~ "complexity_procedures",
      Indicator == "Governance of state-owned enterprises" ~ "governance_soe",
      Indicator == "Regulatory protection of incumbents" ~ "protection_incumbents",
      #Indicator == "Paying taxes: Time" ~ "pay_taxes_overall",
      Indicator == "Publicized laws and government data" ~ "open_data_barometer",
      #Indicator == "Central Bank independence" ~ "cbi",
      #Indicator == "Efficiency of the banking supervisory authority" ~ "efficiency_superv_bank",
      #Indicator == "Efficiency of the financial market supervisory authority" ~ "efficiency_superv_fin",
      #Indicator == "Financial sector: competition regulation" ~ "competition_rules_fin",
      #Indicator == "Burden of government regulation, 1-7 (best)" ~ "govreg_burden",
      #Indicator == "Fundamental rights" ~ "f4_rights",  # POLITICAL ISSUE
      Indicator == "Judicial accountability" ~ "v2juaccnt",
      Indicator == "Lower chamber female legislators" ~ "v2lgfemleg",
      Indicator == "Power distributed by social group" ~ "v2pepwrsoc",
      Indicator == "Power distributed by socio-economic position" ~ "v2pepwrses",
      #Indicator == "Rigorous and impartial public administration" ~ "rigorous_impartial_pa",
      Indicator == "Revised Combined Polity Score" ~ "e_p_polity",
      #Indicator == "Executive constraints" ~ "f1_govpowers",  # POLITICAL ISSUE
      Indicator == "Power distributed by gender" ~ "v2pepwrgen",
      #Indicator == "Law  and order" ~ "f3_security",  # POLITICAL ISSUE
      Indicator == "Corruption" ~ "e_ti_cpi",
      #Indicator == "Government Online Service Index, 0-1 (best)" ~ "egovernmentindex",
      #Indicator == "Procurement" ~ "proc_mean_score",
      Indicator == "Regulatory governance score" ~ "regulatory_governance",
      Indicator == "People can access and afford civil justice" ~ "f7_civiljustice",
      Indicator == "Efficiency of legal framework in challenging regs" ~ "legaleff_challenging",
      Indicator == "Efficiency of legal framework in settling disputes" ~ "legaleff_disputes",
      #Indicator == "Resolving insolvency: Outcome" ~ "resolve_insolv_overall",
      Indicator == "GCI 4.0: Global Competitiveness Index 4.0" ~ "gci_overall",
      Indicator == "GCI 4.0: Efficiency of the clearance process" ~ "lpi_clearance_eff",
      Indicator == "GCI 4.0: Border clearance efficiency" ~ "wef_border_admin",
      Indicator == "Ease of starting a business" ~ "start_bus_overall",
      #Indicator == "Dealing with construction permits: Procedures" ~ "constr_perm_overall",
      Indicator == "Ease of protecting minority investors" ~ "protect_minority_ov",
      Indicator == "Ease of getting credit" ~ "access_credit_overall",
      #Indicator == "Resolving insolvency: Strength of insolvency framework index" ~ "insolvency_framework",
      Indicator == "Use of command and control regulation" ~ "command_control",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(var)) %>%
  pivot_wider(
    id_cols = c(country_name,country_code,year),
    names_from = var#,
    #values_from = value
  ) %>%
  # SC: methodological note for PRM indicates that 1998 and 2013 indicators are comparable, but not with 2018 due to change in methodology
  # --> drop if year ==2018
  #mutate(
  #  across(
  #    c(barriers_startups:protection_incumbents),
  #    ~ifelse(year==2018, NA, .x)
  #  )
  #) %>%
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
  filter(
    year >= 1950   # variable e_p_polity has values since 1800, filter to reduce # of rows
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
    ),
    country_code=as.character(country_code),
    year=as.character(year)
  ) %>%
  select(
    country_code,
    year,
    all_of(vars_api)
  )

# Drop partial data ----
rm(datalist,data_api_raw,data_api_governance,indicator_ids,i)
