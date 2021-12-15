# Load packages ----
packages <- c("tidyverse",
              "here")

pacman::p_load(packages,
               character.only = TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Import original data----------------------
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Basic cleaning ---------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

data_cleaned <- data_original %>%
  ## Rename variables ----
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
    #rigorous_impartial_pa = v2clrspct,
    barriers_trade_expl = barriers_trade_explicit,
    barriers_trade_oth = barriers_trade_other,
    cbi = lvau,
    efw_inv_restr = efw_foreign_invest_restr,
    efw_tourist = efw_freedomofforeignerstovisit
  ) %>%
  ## Change variables ----
  mutate(
    # Fix vars with opposite scale
    # reason: for the CTF methodology, we need for all indicators that "higher values" means "better performance"
    # freedom house: Countries are graded between 1 (most free) and 7 (least free).
    across(
      c(e_fh_pr,e_fh_cl),
      ~(8 - .x)
    ),
    # transform missing
    e_p_polity = ifelse(e_p_polity < -10, NA, e_p_polity),
    # PRM indicators: Countries are graded between 0 (less control/involvement) and 6 (more control/involvement)
    across(
      c(governance_soe,price_controls,command_control,complexity_procedures,
        barriers_startups,protection_incumbents,barriers_trade_expl,barriers_trade_oth),
      ~(6 - .x)
    ),
    # SC: methodological note for PRM indicates that 1998 and 2013 indicators are comparable,
    # but not with 2018 due to change in methodology
    # --> drop if year ==2018
    across(
      c(barriers_startups:protection_incumbents),
      ~ifelse(year==2018, NA, .x)
    ),
    country_name = ifelse(country_name == "ksv", "Kosovo", country_name),
    country_name = ifelse(country_name == "tmp", "Timor-Leste", country_name)
  ) %>%
  ## Keep only indicators from original data ----
  select(
    country_name,
    country_code,
    year,
    all_of(vars_original)
  )

# Drop original data and save selected cleaned ----
rm(data_original)
