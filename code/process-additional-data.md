# Process manual additions

- Input: `data/raw/20211118_new_additions_notGov360.dta`
- Output: `data/clean/additional_data.rds`

## Load packages


```r
packages <- 
  c(
    "tidyverse",
    "here",
    "haven",
    "labelled"
  )

pacman::p_load(packages, character.only = TRUE)
```

## Import data

```r
additions_gtmi <- 
  read_dta(
    here(
      "..",
      "data",
      "raw",
      "20211118_new_additions_notGov360.dta"
    )
  )

additions_gtmi <-
  additions_gtmi %>%
  filter(year == 2020) %>%
  select(
    country,
    countryname,
    iso3code,
    ccode,
    year,
    gtmi
  )



additions <- 
  read_dta(
    here(
      "..",
      "data",
      "raw",
      "20211118_new_additions_notGov360_PMR.dta"
    )
  )
```

```
## Error: 'C:/Users/wb438023/OneDrive - WBG/github/institutional-assessment-dashboard/code/../data/raw/20211118_new_additions_notGov360_PMR.dta' does not exist.
```

```r
additions<-merge(additions,additions_gtmi, by=c('country','countryname','iso3code','ccode','year'),all=TRUE)
```

```
## Error in merge(additions, additions_gtmi, by = c("country", "countryname", : object 'additions' not found
```

## Keep only relevant data


```r
additions_selected <-
  additions %>%
  filter(year >= 2015) %>%
  mutate(
    country_code = iso3code,
    year = as.character(year)
  ) %>%
  select(
    country_code,
    year,
    all_of(vars_additions)
  )
```

```
## Error in filter(., year >= 2015): object 'additions' not found
```


```r
additions_clean <-
  additions_selected %>%
  remove_labels() %>%
  group_by(country_code, year) %>%
  summarise(
    across(
      where(is.numeric),
      ~ mean(., na.rm = TRUE) %>%
        replace(is.nan(.), NA)
    )
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.nan(.), NA, .)
    )
  )
```

```
## Error in remove_labels(.): object 'additions_selected' not found
```

## Save data


```r
write_rds(
  additions_clean,
  here(
    "..",
    "data",
    "clean",
    "additional_data.rds"
  )
)
```

```
## Error in saveRDS(x, con, version = version, refhook = refhook, ascii = text): object 'additions_clean' not found
```
