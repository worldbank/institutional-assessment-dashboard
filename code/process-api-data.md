# Process API data

- Input: `data/final/db_variables.rds`
- Output: `data/clean/original_data.rds`
 
 

```r
packages <- 
  c(
    "tidyverse",
    "here",
    "data360r",
    "skimr"
  )

pacman::p_load(packages,
               character.only = TRUE)
```

## List indicators to retrieve

Governance indicators are calculated in a disaggregate manner, so we will import them separately, aggregate them, and combine with the other indicators.


```r
governance_data <- c(
  42099, # SOE board of directors independence (Mining)
  42100, # SOE board of directors independence (Oil & Gas)
  #42105, # SOE corporate governance practice (Mining)
  #42106, # SOE corporate governance practice (Oil & Gas)
  42161, # SOE-government transfers governance rule (Mining)
  42162, # SOE-government transfers governance rule (Oil & Gas)
  42092, # SOE annual report disclosure (Oil & Gas)
  42091, # SOE annual report disclosure (Mining)
  42107, # SOE financial audit requirement (Mining)
  42108, # SOE financial audit requirement (Oil & Gas)
  42141, # SOE report legislative review requirement (Mining)
  42142  # SOE report legislative review requirement (Oil & Gas)
)
```

All other indicators are coming from the list of selected indicators.


```r
# Get data360
indicator_ids <-
  db_variables %>%
  filter(
    !is.na(api_id),
    !(api_id %in% governance_data)
  ) %>%
  pull(api_id) %>%
  split(
    ceiling(
      seq_along(.)/10
    )
  )
```


```r
datalist = list()

for(i in 1:length(indicator_ids)){
  datalist[[i]] <- 
    get_data360(
           indicator_id = c(indicator_ids[[i]]),
           output_type = 'long'
          )
}
```

```
## Warning in download.file(input, tmpFile, method = method,
## mode = "wb", quiet = !showProgress): cannot open URL
## 'http://tcdata360-backend.worldbank.org/api/v1/data.csv?
## &indicators=%2C41794%2C227%2C41718%2C3305%2C41114%2C724%2C41814%2C308%2C40835%2C41825&':
## HTTP status was '404 Not Found'
```

```
## Error in download.file(input, tmpFile, method = method, mode = "wb", quiet = !showProgress): cannot open URL 'http://tcdata360-backend.worldbank.org/api/v1/data.csv?&indicators=%2C41794%2C227%2C41718%2C3305%2C41114%2C724%2C41814%2C308%2C40835%2C41825&'
```

```r
data_api_raw = do.call(dplyr::bind_rows , datalist)
```


## Extract and aggregate governance data

The code below imports the data from the API

```r
data_api_governance <- 
  get_data360(
    indicator_id = governance_data,
    output_type = 'long'
  )
```

```
## Warning in download.file(input, tmpFile, method = method,
## mode = "wb", quiet = !showProgress): cannot open URL
## 'http://tcdata360-backend.worldbank.org/api/v1/data.csv?
## &indicators=%2C42099%2C42100%2C42161%2C42162%2C42092%2C42091%2C42107%2C42108%2C42141%2C42142&':
## HTTP status was '404 Not Found'
```

```
## Error in download.file(input, tmpFile, method = method, mode = "wb", quiet = !showProgress): cannot open URL 'http://tcdata360-backend.worldbank.org/api/v1/data.csv?&indicators=%2C42099%2C42100%2C42161%2C42162%2C42092%2C42091%2C42107%2C42108%2C42141%2C42142&'
```

These indicators are calculated separately for the mining and oil & gas industries. We are interested in a more aggregate measure, so we will take the average of the two by country and period.


```r
data_api_governance <- 
  data_api_governance %>%
  # Harmonize indicator name across industries
  separate(
    Indicator,
    into = c("Indicator"),
    sep = " [(]"
  ) %>%
  # Group observations to be combined
  group_by(
    `Country ISO3`,
    `Country Name`,
    Indicator,
    Period
  ) %>%
  # Take the average
  summarise(
    Observation = mean(Observation, na.rm = T)
  ) %>%
  mutate(
    Indicator = str_replace_all(Indicator, "-", " ")
  )
```

```
## Error in separate(., Indicator, into = c("Indicator"), sep = " [(]"): object 'data_api_governance' not found
```

## Combine all indicators


```r
data_api <-
  bind_rows(
    data_api_raw,
    data_api_governance
  ) %>%
  mutate(
    Indicator = str_trim(Indicator)
  )
```

```
## Error in list2(...): object 'data_api_governance' not found
```

# Cleaning API data ----

```r
data_api_clean <- 
  data_api %>%
  filter(!is.na(Observation)) %>%
  rename(
    country_code = `Country ISO3`,
    country_name = `Country Name`,
    value = Observation
  ) %>%
  arrange(
    Indicator, country_name
  ) %>%
  separate(
    Period,
    into = c("Year1", "year"),
    sep = "-", 
    remove = F
  ) %>%
  mutate(
    year = ifelse(
      is.na(year),
      as.character(Period),
      year
    )
  ) %>%
  group_by(
    country_code, year, Indicator
  ) %>%
  mutate(count = n()) %>%
  ungroup %>%
  filter(
    !((count > 1) & (year != Period))
  ) %>%
  select(
    -c(`Subindicator Type`, Period, Year1, count)
  ) %>%
  complete(
    nesting(country_code, country_name),
    nesting(Indicator, year),
    fill = list(value = NA)
  ) 
```

```
## Error in filter(., !is.na(Observation)): object 'data_api' not found
```

```r
var_names <-
  db_variables %>%
  select(
    var = variable,
    Indicator = var_api
  )

data_api_labeled <-
  data_api_clean %>%
  filter(
    !is.na(value),
    !is.null(value)
  ) %>% 
  left_join(
    var_names,
    by = "Indicator"
  ) 
```

```
## Error in filter(., !is.na(value), !is.null(value)): object 'data_api_clean' not found
```

```r
data_api_labeled <-
  data_api_labeled %>%
  mutate(
    var = case_when(
      Indicator == "SOE board of directors independence" ~ "soe_board",
      Indicator == "SOE government transfers governance rule" ~ "soe_government",
      Indicator == "SOE annual report disclosure" ~ "soe_annual_report",
      Indicator == "SOE financial audit requirement" ~ "soe_financial",
      Indicator == "SOE report legislative review requirement" ~ "soe_report_legislative",
      TRUE ~ var
    ) 
  ) %>%
  pivot_wider(
    id_cols = c(country_name,country_code,year),
    names_from = var
  ) 
```

```
## Error in mutate(., var = case_when(Indicator == "SOE board of directors independence" ~ : object 'data_api_labeled' not found
```

```r
data_api_rescaled <- 
  data_api_labeled %>%
  mutate(
    e_p_polity = ifelse(e_p_polity < -10, NA, e_p_polity)
  ) %>%
  filter(
    year >= 1950   # variable e_p_polity has values since 1800, filter to reduce # of rows
  ) %>%
  mutate(
    across(
      c(
        soe_governance,
        price_controls,
        command_control,
        complexity_procedures,
        barriers_startups,
        protection_incumbents,
        barriers_trade_expl,
        barriers_trade_oth
      ),
      ~(6 - .x)
    ),
    country_code = as.character(country_code),
    year = as.character(year)
  ) %>%
  select(
    country_code,
    year,
    all_of(vars_api)
  )
```

```
## Error in mutate(., e_p_polity = ifelse(e_p_polity < -10, NA, e_p_polity)): object 'data_api_labeled' not found
```

```r
# Drop partial data ----
rm(datalist,data_api_raw,data_api_governance,indicator_ids,i)
```

```
## Warning in rm(datalist, data_api_raw,
## data_api_governance, indicator_ids, : object
## 'data_api_governance' not found
```

```r
write_rds(
  data_api_rescaled,
  here(
    "..",
    "data",
    "clean",
    "api_data.rds"
  )
)
```

```
## Error in saveRDS(x, con, version = version, refhook = refhook, ascii = text): object 'data_api_rescaled' not found
```
