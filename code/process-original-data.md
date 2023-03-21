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

## Fix country names


```r
original_clean <-
  original_clean %>%
  mutate(
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
