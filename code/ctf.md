# Calculate distance to frontier

- Inputs:
  - `data/final/definitions.rds`
  - `data/final/compiled_indicators.rds`
  - `data/raw/wb_country_list.rds`
      
- Outputs:
 - `data/final/closeness_to_frontier.rds`
 - `data/final/closeness_to_frontier_long.rds`

## Load packages


```r
packages <- 
  c(
    "tidyverse",
    "here",
    "skimr",
    "labelled"
  )

pacman::p_load(packages,
               character.only = TRUE)
```

## Calculate global closeness to frontier

Closeness to frontier (CTF) is global, meaning that we identify the worst and best performance in the full sample (all countries). For each indicator $i$, we compare the last available value of indicator $i$ with the worst and best
performance for indicator $i$ among all countries and in the last $y$ years (2013 - most recent data).^[In [the doing business report](https://www.doingbusiness.org/content/dam/doingBusiness/media/Annual-Reports/English/DB17-Chapters/DB17-DTF-and-DBRankings.pdf) they consider the last 5 years, but here for some indicators we have shorter time series].


1. Keep only data from after 2013

Ideally, this will use data for the last 7 years in any given year


```r
definitions <-
  read_rds(
    here(
      "..",
      "data",
      "final",
      "definitions.rds"
    )
  )

data <-
  read_rds(
    here(
      "..",
      "data",
      "final",
      "compiled_indicators.rds"
    )
  ) %>%
  filter(
    year >= 2013
  )
```

2. Rescale indicators so a higher number is always a better performance


```r
data_rescaled <- 
  data %>%
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


3. Calculate country-level average for each indicator


```r
country_average <-
  data_rescaled %>%
  group_by(
    country_code,
    country_name
  ) %>%
  summarise(
    across(
      all_of(vars_all),
      ~ mean(., na.rm = TRUE)
    )
  )
```

```
## `summarise()` has grouped output by 'country_code'. You can override using the `.groups`
## argument.
```

4. Identify worst and best performance for each indicator


```r
min_max <-
  data_rescaled %>%
  summarise(
    across(
      all_of(vars_all),
      list(
        min = ~ min(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE)
      ),
      .names="{.col}-{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)-(.*)"
  )
```

```
## Warning: There were 2 warnings in `summarise()`.
## The first warning was:
## ℹ In argument: `across(...)`.
## Caused by warning in `min()`:
## ! no non-missing arguments to min; returning Inf
## ℹ Run ]8;;ide:run:dplyr::last_dplyr_warnings()dplyr::last_dplyr_warnings()]8;; to see the 1 remaining warning.
```

5. Calculate closeness to frontier at indicator level


```r
ctf <-
  country_average %>%
  pivot_longer(
    all_of(vars_all),
    names_to = "variable"
  ) %>%
  left_join(
    min_max,
    by = "variable"
  ) %>%
  mutate(
    ctf = (min - value) / (min - max),
    ctf = ifelse(
      ctf == 0,
      0.01,
      ctf
    )
  ) %>%
  pivot_wider(
    id_cols = c("country_name", "country_code"),
    names_from = "variable",
    values_from = "ctf"
  ) %>%
  select(-starts_with("gdp")) %>%
  left_join(
    country_average %>%
      select(country_name, country_code, gdp_pc_ppp_const)
  ) %>%
  mutate(
    log_gdp = log(gdp_pc_ppp_const)
  )
```

```
## Joining with `by = join_by(country_name, country_code)`
```

## Calculate median per group


```r
country_list <-
  read_rds(
    here(
      "..",
      "data",
      "final",
      "wb_country_list.rds"
    )
  )

group_ctf <-
  country_list %>%
  left_join(
    ctf
  ) %>%
  group_by(
    group_code, group
  ) %>%
  summarise(
    across(
      3:ncol(ctf),
      ~ median(., na.rm = TRUE)
    )     
  ) %>%
  filter(!is.na(group)) %>%
  rename(
    country_name = group,
    country_code = group_code
  ) 
```

```
## Joining with `by = join_by(country_code, country_name)`
## `summarise()` has grouped output by 'group_code'. You can override using the `.groups` argument.
```

```r
ctf <-
  ctf %>%
  bind_rows(group_ctf) %>%
  ungroup %>%
  arrange(country_name)

ctf$diversion_pfunds<-(1-ctf$diversion_pfunds)


write_rds(
  ctf,
  here(
    "..",
    "data",
    "final",
    "closeness_to_frontier.rds"
  )
)



ctf_long <-
  ctf %>%
  pivot_longer(
    all_of(vars_all),
    names_to = "variable"
  ) %>%
  select(-contains("gdp")) %>%
  left_join(
    db_variables %>%
      select(variable, var_name, family_name, family_var)
  ) %>%
  left_join(
    country_list %>%
      select(country_name, group)
  )
```

```
## Joining with `by = join_by(variable)`
## Joining with `by = join_by(country_name)`
```

```
## Warning in left_join(., country_list %>% select(country_name, group)): Each row in `x` is expected to match at most 1 row in `y`.
## ℹ Row 1 of `x` matches multiple rows.
## ℹ If multiple matches are expected, set `multiple = "all"` to silence this warning.
```

```r
ctf_long <-
  ctf_long %>%
  group_by(family_name, family_var, country_name, country_code, group) %>%
  summarise(value = median(value, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(
    variable = family_var,
    var_name = family_name
  ) %>%
  bind_rows(ctf_long)
```

```
## `summarise()` has grouped output by 'family_name', 'family_var', 'country_name', 'country_code'.
## You can override using the `.groups` argument.
```

```r
write_rds(
  ctf_long,
  here(
    "..",
    "data",
    "final",
    "closeness_to_frontier_long.rds"
  )
)
```
