# Load packages

packages <- c("tidyverse",
              "here",
              "data360r",
              "skimr")

pacman::p_load(packages,
               character.only = TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# IMPORT ORIGINAL DATA ----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Load .dta file
# data_original <- read_dta(here("data",
#                                "data_cleaned",
#                                "merged_for_residuals.dta"))
# Saving as R format and csv
# save_rds(data_original,
#          here("data",
#              "data_cleaned",
#              "merged_for_residuals.rds")) # save as rds format / small file size
# Open rds file, faster loading

data_original <- read_rds(here("data",
                               "data_cleaned",
                               "merged_for_residuals.rds"))

source(file.path("app/auxiliary",
                 "vars-by-family.R"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# BASIC CLEANING ---------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

data_cleaned <- data_original %>%
  # Rename variables
  rename(
    country_name = countryname,
    country_code = code,
    telecom_infr = telecommunicationinfrastructurei,
    daigov_2016 = daigovernmentsubindex_2016,
    contract_manag_score = contract_management_score,
    perf_guarantee_score = performance_guarantee_score,
    proc_mean_score = mean_score,
    f6_regulatoryenf = f6_regulatoryenforcement,
    efw_integrity_legalsys = efw_integrityofthelegalsystem,
    efw_contracts_enf = efw_legalenforcementofcontracts,
    efw_businessreg = efw_businessregulations,
    efw_free_foreign_curr = efw_freedom_foreign_curr,
    protect_minority_ov = protect_minority_overall,
    efficiency_superv_bank = efficiancy_supervision_banking,
    efficiency_superv_fin = efficiancy_supervision_finmkt,
    getting_credit = bl,
    insolvency_framework = resolvinginsolvencystrength,
    credit_registry_cov = gettingcreditcreditregistry,
    rigorous_impartial_pa = v2clrspct,
    barriers_trade_expl = barriers_trade_explicit,
    barriers_trade_oth = barriers_trade_other,
    cbi = lvau,
    efw_inv_restr = efw_foreign_invest_restr,
    efw_tourist = efw_freedomofforeignerstovisit
  ) %>%
  # Fix vars with opposite scale
  # reason: for the CTF methodology, we need for all indicators that "higher values" means "better performance"
  # freedom house: Countries are graded between 1 (most free) and 7 (least free).
  mutate(
    across(
      c(e_fh_pr,e_fh_cl),
      ~(8 - .x)
    )
  ) %>%
  # transform missing
  mutate(
    e_p_polity = ifelse(e_p_polity < -10, NA, e_p_polity)
  ) %>%
  # PRM indicators: Countries are graded between 0 (less control/involvement) and 6 (more control/involvement)
  mutate(
    across(
      c(governance_soe,price_controls,command_control,complexity_procedures,barriers_startups,protection_incumbents,barriers_trade_expl,barriers_trade_oth),
      ~(6 - .x)
    )
  ) %>%
  mutate(
    country_name = ifelse(country_name == "ksv", "Kosovo", country_name),
    country_name = ifelse(country_name == "tmp", "Timor-Leste", country_name)
  )

extra_groups <- data_cleaned %>%
  select(country_name,country_code, lac,lac6,structural) %>%
  unique

write_rds(extra_groups,
          here("data",
               "data_cleaned",
               "extra_groups.rds"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CHOOSE INDICATORS OF INTEREST --------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Keep only vars of interest
data_selected <- data_cleaned %>%
  select(
    -c(country,iso3code)
  ) %>%
  # SC: methodological note for PRM indicates that 1998 and 2013 indicators are comparable, but not with 2018 due to change in methodology
  # --> drop if year ==2018
  mutate(
    across(
      c(barriers_startups:protection_incumbents),
      ~ifelse(year==2018, NA, .x)
    )
  )

# Additions with indicators that are not on Gov360 ----
additions <- haven::read_dta(here("data",
                                  "data_raw",
                                  "new_additions_notGov360.dta"))
additions <- additions %>%
  filter(year==2020 | year ==2015) %>%
  rename(country_code = iso3code) %>%
  select(-c(countryname,country,ccode)) %>%
  mutate(
    year=as.character(year)
  ) %>%
  labelled::remove_labels()

data_binded <-
  data_selected %>%
  mutate(
    country_code=as.character(country_code),
    year=as.character(year)
  ) %>%
  left_join(
    additions,
    by = c("country_code","year")
  ) %>%
  mutate(
    v2stfisccap = coalesce(v2stfisccap.x, v2stfisccap.y),
    v2stcritrecadm = coalesce(v2stcritrecadm.x, v2stcritrecadm.y)
  ) %>%
  #select(
  #  country_name, country_code,
  #  year,
  #  all_of(vars_all)
  #) %>%
  mutate(
    proff1 = proff1*(-1)
  )

# KEEP ONLY INDICATORS THAT SHOULD BE FROM ORIGINAL DATA AND ADDITIONS ----
# (THE ONES THAT HAVE NO ISSUE COMING FROM API)

data_original <-
  data_binded %>%
  select(
    c(
      country_code,country_name,year,
      gtmi,
      v2stfisccap,
      v2stcritrecadm,
      scopeofstateownedenterprises,
      governmentinvolvementinnetworkse,
      directcontroloverbusinessenterpr,
      price_controls,
      command_control,
      proff1,
      close2,
      efw_free_foreign_curr,
      efw_labor_mkt_reg,
      govreg_burden,
      lpi_clearance_eff,
      open_data_barometer,
      proc_mean_score,
      regulatory_governance,
      rigorous_impartial_pa,
      v2juaccnt,
      v2lgfemleg,
      v2pepwrgen,
      v2pepwrses,
      v2pepwrsoc,
      # VAR ONLY IN ORIGINAL DATA, NOT FOUND IN API
      f5_opengov,
      nontariff_barriers,
      efw_tourist,
      efw_credit_mkt_reg,
      efw_capitalcontrols,
      efw_inv_restr,
      competition_rules_fin,
      efficiency_superv_bank,
      efficiency_superv_fin,
      cbi,
      insolvency_framework,
      collective_barg,
      empl_protection_perm,
      empl_protection_temp,
      union_density,
      minimum_wage_ratio,
      f8_criminaljustice,
      efw_integrity_legalsys,
      e_fh_pr,
      e_fh_cl,
      v2lgqugen,
      f6_regulatoryenf,
      centregov_mean,
      v2xcs_ccsi
    )
  )

var_original <- data_original %>%
  select(-c(country_code,country_name,year)) %>%
  skim() %>%
  select(skim_variable) %>%
  .$skim_variable

var_api <- setdiff(vars_all,var_original)

# GET INDICATORS FROM API 360 ----

# IDs from selected indicators
selected_indicators_1 <- c(
  290,   # Corruption / Percent of firms identifying the courts system as a major constraint
  #464, #DOING BUSINESS INDICATOR  #  Dealing with construction permits: Procedures
  #472, #DOING BUSINESS INDICATOR  # Registering property: Cost
  #477,	#DOING BUSINESS INDICATOR  # Enforcing contracts: Cost
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
  #719,  #	Prevalence of trade barriers
  723,	 # Burden of customs procedures
  #747,   # Index of economic freedom score          # ONLY ON DEFINITIONS
  3276,  # Corruption
  #3285,	 # Foreign Currency Regulations               # DIFFERENT VALUES
  3289,	 # Restrictive Labor Regulations
  3305,	 # Administrative burdens on startups
  3307,	 # Explicit barriers to trade and investment
  3308,	 # Other barriers to trade and investment
  3310,  #  Use of command and control regulation
  3311,	 # Price controls
  3323	 # Complexity of regulatory procedures
)

# API WASNT ABLE TO GET ALL TOGETHER
selected_indicators_2 <- c(
  3326,	 # Governance of state-owned enterprises
  3328,	 # Regulatory protection of incumbents
  #3451,  # Government Online Service Index
  #3469,  # E-Participation Index, 0-1 (best)
  #24840, #DOING BUSINESS INDICATOR # Paying taxes: Time
  27470, # Revised Combined Polity Score
  #27885, # Publicized laws and government data
  27889, # Right to information
  27894, # Complaint mechanisms
  27900, # Freedom of opinion and expression is effectively guaranteed
  27919, # People can access and afford civil justice
  #28833, #DOING BUSINESS INDICATOR  # Resolving insolvency Outcome
  28782, #  Steering Capability
  #30823, # Central Bank independence                # NO RECENT DATA
  #31001, #  Efficiency of the banking supervisory authority
  #31003, #  Efficiency of the financial market supervisory authority
  #31088, #  Financial sector: competition regulation # ID NOT FOUND 24-09-2021
  #31115, #  Freedom of entry for foreigners # ID NOT FOUND 24-09-2021
  40294, #  Ease of starting a business
  40418, #  Ease of getting credit
  40432 #  Ease of protecting minority investors
  #40822  # 3. Undue influence
  #40985 #  Civil Liberties                    # STRANGE VALUES
  #40986, # Political Rights                           # STRANGE VALUES
)

selected_indicators_3 <- c(
  #41008, #	Burden of government regulation, 1-7 (best)
  41031, #  Regulatory governance score
  #41189, #  Procurement
  41305, #  Burden of customs procedures, 1-7 (best)
  41619, #  GCI 4.0: Global Competitiveness Index 4.0
  41629, #  GCI 4.0: 1.E Undue influence and corruption
  41703, #  GCI 4.0: 7.A Domestic competition
  41714, #  GCI 4.0: Efficiency of the clearance process
  41794, #  Absence of corruption (Global States of Democracy)
  41827, #  Civil society participation
  41881, #  Engaged society
  41883, #  Executive constraints
  41857, #  CSO entry and exit
  41918, #  Freedom of academic and cultural expression
  41932, #  Fundamental rights
  41951, #  Judicial accountability
  #41953 #	Judicial independence       # ONLY ON DEFINITIONS
  41955, #  Law  and order
  41981, #  Lower chamber female legislators
  42024, #  Power distributed by gender
  42025, #  Power distributed by social group
  42026, #  Power distributed by socio-economic position
  #42084, #	Rigorous and impartial public administration
  #42602, #DOING BUSINESS INDICATOR #  Resolving insolvency: Strength of insolvency framework index
  43034, #  Hiring and firing practices, 1-7 (best)
  43050  #  GCI 4.0: Border clearance efficiency
)

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
data_api_1 <- get_data360(
  indicator_id = selected_indicators_1,
  output_type = 'long')

data_api_2 <- get_data360(
  indicator_id = selected_indicators_2,
  output_type = 'long')

data_api_3 <- get_data360(
  indicator_id = selected_indicators_3,
  output_type = 'long')

data_api_governance <- get_data360(
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
    data_api_1,
    data_api_2,
    data_api_3,
    data_api_governance
  )

data_api <-
  data_api %>%
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
    mutate(
      var = case_when(
        Indicator == "SOE board of directors independence" ~ "soe_board",
        #Indicator == "SOE corporate governance practice" ~ "soe_corporate",
        Indicator == "SOE-government transfers governance rule" ~ "soe_government",
        Indicator == "SOE annual report disclosure" ~ "soe_annual_report",
        Indicator == "SOE financial audit requirement" ~ "soe_financial",
        Indicator == "SOE report legislative review requirement" ~ "soe_report_legislative",
        Indicator == "Steering Capability" ~ "steering_capability",
        Indicator == "Hiring and firing practices, 1-7 (best)" ~ "hiring_pract",
        Indicator == "Freedom of opinion and expression is effectively guaranteed" ~ "opinion_freedom",
        Indicator == "Freedom of academic and cultural expression" ~ "v2clacfree",
        Indicator == "Complaint mechanisms" ~ "complaint_mechan",
        Indicator == "Right to information" ~ "right_to_info",
        Indicator == "GCI 4.0: 1.E Undue influence and corruption" ~ "undue_influ_corrupt",
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
        Indicator == "Fundamental rights" ~ "f4_rights",
        Indicator == "Judicial accountability" ~ "v2juaccnt",
        Indicator == "Lower chamber female legislators" ~ "v2lgfemleg",
        Indicator == "Power distributed by social group" ~ "v2pepwrsoc",
        Indicator == "Power distributed by socio-economic position" ~ "v2pepwrses",
        #Indicator == "Rigorous and impartial public administration" ~ "rigorous_impartial_pa",
        Indicator == "Revised Combined Polity Score" ~ "e_p_polity",
        Indicator == "Executive constraints" ~ "f1_govpowers",
        Indicator == "Power distributed by gender" ~ "v2pepwrgen",
        Indicator == "Law  and order" ~ "f3_security",
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
    pivot_wider(
      id_cols = c(country_name,country_code,year),
      names_from = var
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
  ) %>%
  select(
    country_code,year,
    all_of(var_api)
  )

var_matched_api <- data_api %>%
  select(-c(country_code, year)) %>%
  skim() %>%
  select(skim_variable) %>%
  .$skim_variable

missing_var_api <- setdiff(var_api,var_matched_api)

data_mixed <- data_original %>%
  left_join(
    data_api,
    by = c("country_code","year")
  ) %>%
  select(-c(v2stfisccap, close2, proff1, undue_influ_corrupt))

# Explore dataset
#skim(data_selected)

write_rds(data_mixed,
          here("data",
               "data_cleaned",
               "selected_vars.rds"))

write_rds(data_mixed,
          here("app",
               "data",
               "raw_data.rds"))

rm(data_binded,data_selected,data_cleaned,data_api_2, data_api_3,data_api_1)
