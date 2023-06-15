# Clean list of contries

- Inputs:
  - `data/final/compiled_indicators.rds`
  - `data/raw/CLASS.xlsx`, obtained from https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups on September 1, 2022
  - `data/raw/group_list.csv`, input by the research team to list relevant groups
      
- Outputs:
  - `data/final/wb_country_list.rds`
  - `data/final/wb_country_groups.rds`


```r
  packages <-
    c(
      "tidyverse",
      "here",
      "readxl",
      "assertthat",
      "countrycode"
    )

  pacman::p_load(packages,
                 character.only = TRUE)
```


## Inputs


```r
  indicators <-
    read_rds(
      here(
        "..",
        "data",
        "final",
        "compiled_indicators.rds"
      )
    )

  group_list <-
    read_csv(
      here(
        "..",
        "data",
        "raw",
        "group_list.csv"
      )
    )
```

```
## Rows: 17 Columns: 2
## ── Column specification ──────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): group_name, group_category
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
  country_list <-
    read_xlsx(
      here(
        "..",
        "data",
        "raw",
        "CLASS.xlsx"
      ),
      sheet = "Groups"
    ) %>%
    transmute(
      country_code = CountryCode,
      group = GroupName,
      group_code = GroupCode
    )
```

## Subset country list

The only relevant countries are those we have some data for


```r
country_list <-
  indicators %>%
  select(country_code, country_name) %>%
  unique %>%
  left_join(country_list)
```

```
## Joining with `by = join_by(country_code)`
```

```
## Warning in left_join(., country_list): Each row in `x` is expected to match at most 1 row in `y`.
## ℹ Row 1 of `x` matches multiple rows.
## ℹ If multiple matches are expected, set `multiple = "all"` to silence this
##   warning.
```

## Subset groups


```r
country_list <-
  country_list %>%
  filter(
    group %in% group_list$group_name
  ) %>%
  unique
```

## Save datasets

Dataset with list of countries in our sample


```r
write_rds(
  country_list,
  here(
    "..",
    "data",
    "final",
    "wb_country_list.rds"
  )
)

write_rds(
  group_list,
  here(
    "..",
    "data",
    "final",
    "wb_country_groups.rds"
  )
)
```
