# Calculate distance to frontier

- Inputs:
  - `data/constructed/indicator_definitions.rds`
  - `data/final/compiled_indicators.rds`
  - `data/final/wb_country_list.rds`
      
- Outputs:
 -`data/final/closeness_to_frontier.rds`
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
      "constructed",
      "indicator_definitions.rds"
    )
  )
```

```
## Warning in readRDS(con, refhook = refhook): cannot open
## file 'C:/Users/wb501238/Documents/GitHub/institutional-
## assessment-dashboard/notebook/../data/constructed/
## indicator_definitions.rds': No such file or directory
```

```
## Error in readRDS(con, refhook = refhook): cannot open the connection
```

```r
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


2. Calculate country-level average for each indicator


```r
country_average <-
  data %>%
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
## `summarise()` has grouped output by 'country_code'. You
## can override using the `.groups` argument.
```

3. Identify worst and best performance for each indicator


```r
min_max <-
  data %>%
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

4. Calculate closeness to frontier at indicator level


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
## Joining, by = c("country_name", "country_code")
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
## Joining, by = c("country_code", "country_name")
## `summarise()` has grouped output by 'group_code'. You
## can override using the `.groups` argument.
```

```r
ctf <-
  ctf %>%
  bind_rows(group_ctf) %>%
  ungroup %>%
  arrange(country_name)

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
## Joining, by = "variable"
## Joining, by = "country_name"
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
## `summarise()` has grouped output by 'family_name',
## 'family_var', 'country_name', 'country_code'. You can
## override using the `.groups` argument.
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
