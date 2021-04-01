# Load packages

packages <- c("tidyverse",
              "here",
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
    credit_registry_cov = gettingcreditcreditregistry, # Peter McConaghy suggested to include this, but already included in access_credit_overall
    rigorous_impartial_pa = v2clrspct,
    barriers_trade_expl = barriers_trade_explicit,
    barriers_trade_oth = barriers_trade_other,
    cbi = lvau,
    efw_inv_restr = efw_foreign_invest_restr,
    efw_tourist = efw_freedomofforeignerstovisit
  ) %>%
  # Fix vars with opposite scale
  # reason: for the DTF methodology, we need for all indicators that "higher values" means "better performance"
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
  )

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CHOOSE INDICATORS OF INTEREST --------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 1 POLITICAL INSTITUTIONS
vars_pol <- c("e_fh_pr","e_fh_cl","e_p_polity","f1_govpowers","v2pepwrsoc","v2pepwrses","v2pepwrgen","v2lgqugen","f4_rights","f3_security","v2lgfemleg")
# dropped EA: wgi_pol_stability

# 2 SOCIAL INSTITUTIONS
vars_social <- c("v2x_cspart","v2dlengage","v2xcs_ccsi","trust_pol")

# 3 ACCOUTABILITY INSTITUTIONS
vars_transp <- c("e_ti_cpi","f2_corruption","favoritism","bribes","diversion_pfunds","transparency_polmak","egovernmentindex","eparticipationindex",
                 "f5_opengov","rigorous_impartial_pa","open_data_barometer")
#dropped EA: wgi_voice_acc wgi_control_corr

# 4 CENTER OF GOV/PUBLIC SECTOR INSTITUTIONS
vars_publ <- c("f6_regulatoryenf","proc_mean_score","eff_govspending","regulatory_governance","centregov_mean")
# dropped EA: wgi_regulatory wgi_gov_effective
# dropped SC: gov_efficiency (GCI)

# 5 LEGAL INSTITUTIONS
vars_leg <- c("f8_criminaljustice","f7_civiljustice","es_court_constraint","v2juaccnt",
              "efw_integrity_legalsys","legaleff_challenging","legaleff_disputes","enf_contr_overall","resolve_insolv_overall")
# dropped EA: wgi_rulelaw
# dropped SC: fw_contracts_enf efw_impartialcourts
# EA: judicial ind for LJI from LinzerStanton/VDEM?

# 6 BUSINESS ENV. AND TRADE INSTITUTIONS
vars_mkt <- c("govreg_burden", "gci_overall", "mkt_dominance", "eff_antimonopoly", "nontariff_barriers", "property_rights",
              "efw_inv_restr", "efw_capitalcontrols", "efw_tourist", "customs_burden", "lpi_clearance_eff", "wef_border_admin",
              "complexity_procedures", "barriers_startups", "protection_incumbents", "barriers_trade_expl", "barriers_trade_oth",
              "start_bus_overall", "constr_perm_overall", "register_prop_overall", "protect_minority_ov", "pay_taxes_overall", "trade_borders_overall")
# dropped SC: wsj_propertyrights, startbus_days, startbus_procedures, wsj_businessfreedom (already from source, WB DB), efw_property_rights 	efw_reg_trade_barr efw_businessreg (already from source, WEF, GCR: nontariff_barriers property_rights govreg_burden)
# EA unpacked efw_controls_movement ->  efw_inv_restr efw_capitalcontrols efw_tourist

# 7 LABOR MARKET INSTITUTIONS
vars_lab <- c("efw_labor_mkt_reg","collective_barg","empl_protection_perm","empl_protection_temp","union_density","minimum_wage_ratio")

# 8 FINANCIAL INSTITUTITONS
vars_fin <- c("efw_credit_mkt_reg", "efw_free_foreign_curr", "competition_rules_fin", "efficiency_superv_bank",
              "efficiency_superv_fin", "cbi", "access_credit_overall", "insolvency_framework")
# dropped EA: access_credit_overall insolvency_framework --> SC: why?
# dropped SC: getting_credit, credit_registry_cov (subindicators of access_credit_overall)
# dropped SC: wsj_financialfreedom (as suggested by Peter McConaghy), financial_institutions (it is an outcome)
# added EA: cbi --> SC: what is it?

# 9 SOE Governance/SERVICE DELIVERY INSTITUTIONS
vars_service_del <- c("governance_soe","price_controls","command_control")

# Create a list for group all variables
vars_all <- c(vars_pol,
                    vars_social,
                    vars_transp,
                    vars_publ,
                    vars_leg,
                    vars_mkt,
                    vars_lab,
                    vars_fin,
                    vars_service_del)

# Keep only vars of interest
data_selected <- data_cleaned %>%
  select(
    country_name, country_code,
    year,
    lac, lac6, oecd,
    structural,
    all_of(vars_all)
  ) %>%
  # SC: methodological note for PRM indicates that 1998 and 2013 indicators are comparable, but not with 2018 due to change in methodology
  # --> drop if year ==2018
  mutate(
    across(
      c(barriers_startups:protection_incumbents),
      ~ifelse(year==2018, NA, .x)
    )
  )

# Explore dataset
skim(data_selected)

write_rds(data_selected,
          here("data",
               "data_cleaned",
               "selected_vars.rds"))

rm(data_cleaned, data_original)
