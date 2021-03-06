# Process original data

- Input: `data/raw/merged_for_residuals.rds`
- Output: `data/clean/original_data.rds`



```r
packages <-
  c(
    "here",
    "tidyverse",
    "labelled"
  )

pacman::p_load(packages, character.only = TRUE)
```


## Import original data

This data was originally received in Stata format, then saved as an R dataset


```r
data_original <-
  read_rds(
    here(
      "..",
      "data",
      "raw",
      "merged_for_residuals.rds"
    )
  )
```


## Rename variables

Rename variables BECAUSE



```r
original_clean <-
  data_original %>%
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
    barriers_trade_expl = barriers_trade_explicit,
    barriers_trade_oth = barriers_trade_other,
    cbi = lvau,
    efw_inv_restr = efw_foreign_invest_restr,
    efw_tourist = efw_freedomofforeignerstovisit
  )
```

## Rescale

Some variable are measured in opposite scale, where a lower value means a better performance. To apply the closeness to frontier methodology, we need all indicators to point to the same direction, where higher values mean better performances.


```r
original_clean <-
  original_clean %>%
  mutate(
    # Freedom house: Countries are graded between 1 (most free) and 7 (least free).
    across(
      c(
        e_fh_pr,
        e_fh_cl
      ),
      ~ (8 - .x)
    ),
    # PRM indicators: Countries are graded between 0 (less control/involvement) and 6 (more control/involvement)
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
      ~ (6 - .x)
    ),
    # Methodological note for PRM indicates that 1998 and 2013 indicators are comparable,
    # but not with 2018 due to change in methodology, so we remove 2018 data
    across(
      c(barriers_startups:protection_incumbents),
      ~ ifelse(year == 2018, NA, .x)
    ),
    # transform missing
    e_p_polity = ifelse(e_p_polity < -10, NA, e_p_polity),
    # Fix country names
    country_name = ifelse(country_name == "ksv", "Kosovo", country_name),
    country_name = ifelse(country_name == "tmp", "Timor-Leste", country_name),
    country_code = as.character(country_code),
    year = as.character(year)
  )
```

## Subset indicators

We will keep only indicators from original data


```r
original_clean <-
  original_clean %>%
  select(
    country_name,
    country_code,
    year,
    all_of(vars_original)
  ) %>%
  remove_labels()

rm(data_original)
```

## Save data


```r
write_rds(
  original_clean,
  here(
    "..",
    "data",
    "clean",
    "original_data.rds"
  )
)
```
