# Process original data

- Input: `data/raw/merged_for_residuals-v2.rds`
- Input: `data/raw/CBIData_Romelli2022.dta`
- Output: `data/clean/original_data.rds`

*Notes*: We have to address the country name bug from the source. We use the `countrycode` package in each data import and cleaning in order to verify the country name and code. The objective is to create a unique, consistent identifier for country identifications for all data imports.


```r
packages <-
  c(
    "here",
    "tidyverse",
    "labelled",
    "countrycode",
    'haven'
  )

pacman::p_load(packages, character.only = TRUE)
```

## Aux. funs


```r
plot_missingness <- function(data){
  data |> 
    select(
    starts_with(var)
    ) |> 
    summarise(
      var_name = var,
      indicator_cor = cor(
        .data[[paste0(var, ".dashboard")]],
        .data[[paste0(var, ".revised")]],
        use = "complete.obs"
      )
    )
}
```

## Import original data

This data was originally received in Stata format, then saved as an R dataset.


```r
data_original <-
  read_rds(
    here(
      "..",
      "data",
      "raw",
      "merged_for_residuals-v2.rds"
    )
  )
```



```r
cbi_data <-
  read_dta(
    here(
      "..",
      "data",
      "raw",
      "CBI",
      "Romelli2022",
      "CBIData_Romelli2022.dta"
    )
  )

db_variables <- read_rds(
  here(
    "..",
    "data",
    "final",
    "db_variables.rds"
  )
)

source(
  here("vars-control.R")
)
```

## Rename variables

Rename variables because these are inherited from original stata files.


```r
original_clean <-
  data_original %>%
  rename(
    country_name = countryname,
    country_code = iso3code,
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
    efw_inv_restr = efw_foreign_invest_restr,
    efw_tourist = efw_freedomofforeignerstovisit
  )
```

## Fix country names


```r
original_clean_test <- original_clean |> 
  mutate(
    country_code_test = countrycode(
      country_name, 
      origin = "country.name", 
      destination = "wb",
      nomatch = NA_character_
    )
  )
```

```
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `country_code_test = countrycode(...)`.
## Caused by warning in `countrycode_convert()`:
## ! Some values were not matched unambiguously: ksv, Micronesia, Timor, tmp, Yugoslavia
```

```r
original_clean_test |> 
  filter(country_code != country_code_test) |> 
  distinct(country_name, .keep_all = TRUE) |> 
  select(starts_with("country"))
```

```
## # A tibble: 3 × 7
##   country         country_name country_code countrycode country_…¹ count…² count…³
##   <chr>           <chr>        <chr>        <chr>       <chr>        <dbl> <chr>  
## 1 Andorra         Andorra      ADO          AND         ""              NA AND    
## 2 Romania         Romania      ROM          ROU         "ROU"          190 ROU    
## 3 WestBankandGaza Palestine    WBG          PSE         ""              NA PSE    
## # … with abbreviated variable names ¹​country_text_id, ²​country_id,
## #   ³​country_code_test
```

```r
original_clean_countrycode_nomatch <- original_clean_test |> 
  filter(
    is.na(country_code_test)
  )

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

Remove CBI from API and replace it with David Romelli data

```r
cbi_data <-
  cbi_data %>%
  select(
    admin,
    wb_a3,
    year,
    LVAU
  ) %>%
  remove_labels()
  
cbi_data <-
  cbi_data %>%
  rename(
    country_name = admin,
    country_code = wb_a3,
    cbi = LVAU
  )

cbi_data$year <- as.character(cbi_data$year)

original_clean <-
  original_clean %>%
  left_join(
    cbi_data,
    by = c("country_name","country_code", "year")
  )
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
