data_select_original <-
  data_cleaned %>%
  select(
    country_code,
    country_name,
    year,
    all_of(vars_original)
  )

data_select_additions <-
  additions %>%
  select(
    country_code,
    year,
    all_of(vars_additions)
  )

data_select_api <-
  data_api %>%
  select(
    country_code,
    year,
    all_of(vars_api)
  )

data_binded <- data_select_original %>%
  left_join(
    data_select_additions,
    by = c("country_code","year")
  ) %>%
  left_join(
    data_select_api,
    by = c("country_code","year")
  )

rm(additions,data_api,data_cleaned,data_select_additions,data_select_api,data_select_original)

# Save binded datasets ====================================================
write_rds(data_binded,
          here("data",
               "data_cleaned",
               "selected_vars.rds"))
