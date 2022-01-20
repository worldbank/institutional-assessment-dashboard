# Import variables data and variables control----
source(file.path("app/auxiliary",
                 "vars-control.R"))

# Import original data ----
source(file.path("code/R",
                 "01-1-import-original-data.R"))

# Import API data ----
source(file.path("code/R",
                 "01-2-import-api-data.R"))

# Import additional data ----
source(file.path("code/R",
                 "01-3-import-additional-data.R"))

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


