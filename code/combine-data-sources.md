# Import data

- Inputs:
  - `data/clean/original_data.rds`
  - `data/clean/additional_data.rds`
  - `data/clean/api_data.rds`

- Outputs:
  - `data/final/compiled_indicators.rds`

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

# count number of missing values
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

# Save binded datasets ====================================================
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
