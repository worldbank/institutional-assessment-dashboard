# 1 Set up ########################################################################################

# 1.1 Packages ===================================================================================

  packages <-
    c("tidyverse",
      "here")

  pacman::p_load(packages,
                 character.only = TRUE)

# 1.2 Inputs ====================================================================================

  country_list <-
    read_csv(here("data",
                  "data_raw",
                  "wb_country_list.csv")) %>%
    rename(country_name = country)

  countries <-
    country_list %>%
    select(country_name,
           country_code) %>%
    unique

  dtf_vars_global <-
    read_rds(here("data",
                  "data_cleaned",
                  "dtf_vars_global.rds"))

  group_codes <-
    c("EUU",
      "HIC",
      "LIC",
      "LMC",
      "LMY",
      "MIC",
      "OED",
      "NAC",
      "UMC")


# 2 Create groups vars #########################################################################

  group_vars <- function(country, code) {
    group_contries <-
      country_groups %>%
      filter(group_code == code) %>%
      select(country) %>%
      pluck(1)

    country %in% group_contries
  }

  countries <-
    group_codes %>%
    map(
      ~ countries %>%
        mutate(
          !!paste0(.x) :=
            group_vars(country_name, .x)
        ) %>%
        select(matches(.x))
    ) %>%
    bind_cols(countries, .)

  dtf_vars_global <-
    dtf_vars_global %>%
    left_join(countries)

  groups <-
    country_list %>%
    filter(group_code %in% group_codes) %>%
    rename(group_name = group) %>%
    select(group_name,
           group_code)

# 3 Save datasets ###########################################################################

  write_rds(dtf_vars_global,
            here("app",
                 "data",
                 "country_dtf.rds"))

  write_rds(groups,
            here("app",
                 "data",
                 "wb_country_groups.rds"))
