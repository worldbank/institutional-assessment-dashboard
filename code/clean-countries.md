# Clean list of contries

- Inputs:
  - `data/final/compiled_indicators.rds`
  - `data/raw/wb_country_list.rds`
      
      
- Outputs:
  - `data/final/wb_country_list.rds`


```r
  packages <-
    c(
      "tidyverse",
      "here"
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

  country_list <-
    read_csv(
      here(
        "..",
        "data",
        "raw",
        "wb_country_list.csv"
      )
    ) %>%
    rename(country_name = country)
```

```
## Rows: 2058 Columns: 4
## -- Column specification ---------------------------------
## Delimiter: ","
## chr (4): group_code, group, country_code, country
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

List all relevant groups


```r
  group_codes <-
    c(
      "EUU",
      "ARB",
      "CEB",
      "HIC",
      "LIC",
      "LMC",
      "LMY",
      "MIC",
      "OED",
      "UMC",
      "EAS",
      "ECS",
      "LCN",
      "MEA",
      "NAC",
      "SAS",
      "SSF"
    )
```


## Subset country list

The only relevant countries are those we have some data for


```r
country_list <-
  country_list %>%
  filter(
    country_name %in% indicators$country_name,
    group_code %in% group_codes
  )
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
```
