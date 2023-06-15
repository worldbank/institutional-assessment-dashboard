# Import data

- Inputs:
  - `data/clean/original_data.rds`
  - `data/clean/additional_data.rds`
  - `data/clean/api_data.rds`

- Outputs:
  - `data/final/compiled_indicators.rds`


## Load packages


```r
packages <- 
  c(
    "tidyverse",
    "here",
    "testthat",
    "naniar"
  )

pacman::p_load(
  packages, 
  character.only = TRUE
)

theme_set(theme_minimal())
```

## Load data


```r
original_data <-
  read_rds(
    here(
      "..",
      "data",
      "clean",
      "original_data.rds"
    )
  )


original_data <-
  original_data %>%
  mutate(
    country_code = ifelse(country_code == "ROM", "ROU", country_code),
  )
```



```r
additional_data <-
  read_rds(
    here(
      "..",
      "data",
      "clean",
      "additional_data.rds"
    )
  )

api_data <-
  read_rds(
    here(
      "..",
      "data",
      "clean",
      "api_data.rds"
    )
  )

full_data <-
  original_data %>%
  left_join(
    additional_data,
    by = c("country_code", "year")
  ) %>%
  left_join(
    api_data,
    by = c("country_code","year")
  )
```


```r
# count number of missing values
# why are we establishing a manually determined (83) NA count here?
full_data <-
  full_data %>%
  mutate(na_count = apply(is.na(.), 1, sum)) %>%
  filter(na_count < 83) %>%
  select(-na_count)

# Clean country names
full_data <-
  full_data %>%
  mutate(
    country_name =
      country_name %>%
      str_replace_all("Macedonia", "North Macedonia") %>%
      str_replace_all("Swaziland", "Eswatini")
  )
```

## Fix GCI data

GCI indicators are supposed to take values from 1 to 7. However, each indicator has one country with a score above 7. In the code chunk below, we fix this issue.


```r
full_data <-
  full_data %>%
  mutate(
    across(
      c(
        customs_burden,
        legaleff_challenging,
        eff_antimonopoly,
        favoritism,
        wef_judindep,
        trust_pol,
        #wef_renewable,
        legaleff_disputes
      ),
      ~ ifelse(. <= 7, ., NA)
    )
  )
```


##. Fix scale of indicators

Rescale indicators so a higher number is always a better performance


```r
full_data <- 
  full_data %>%
  mutate(
    e_p_polity = ifelse(e_p_polity < -10, NA, e_p_polity),
    # PRM indicators: Countries are graded between 0 (less control/involvement) and 6 (more control/involvement). Methodological note for PRM indicates that 1998 and 2013 indicators are comparable, but not with 2018 due to change in methodology, so we remove 2018 data
    across(
      c(
        soe_governance,
        price_controls,
        command_control,
        complexity_procedures,
        barriers_startups,
        protection_incumbents,
        barriers_trade_expl,
        barriers_trade_oth,
        directcontroloverbusinessenterpr,
        governmentinvolvementinnetworkse,
        scopeofstateownedenterprises
      ),
      ~ ifelse(year == 2018, NA, 6 - .x)
    ),
    # Enterprise Survey: Percent Of Firms Identifying X As A Major Constraint
    across(
      c(
        es_finance_constraint,
        es_permits_constraint,
        es_court_constraint,
        se_customs_constraint,
        es_labor_constraint,
        es_corruption_constraint,
        es_tax_constraint
      ),
      ~ 100 - .
    ),
    # Freedom house: Countries are graded between 1 (most free) and 7 (least free)
    across(
      c(
        e_fh_pr,
        e_fh_cl
      ),
      ~ (8 - .x)
    )
  )
```

```
## Error in `mutate()`:
## ℹ In argument: `across(...)`.
## Caused by error in `across()`:
## ! Can't subset columns that don't exist.
## ✖ Column `soe_governance` doesn't exist.
```

## Save binded datasets ====================================================


```r
write_rds(
  full_data,
  here(
    "..",
    "data",
    "final",
    "compiled_indicators.rds"
  )
)
```
