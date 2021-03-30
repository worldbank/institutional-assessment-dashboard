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

  group_codes <-
    c("EUU",
      "HIC",
      "LIC",
      "LMC",
      "LMY",
      "MIC",
      "OED",
      "UMC")

  groups <-
    country_list %>%
    filter(group_code %in% group_codes) %>%
    rename(group_name = group) %>%
    select(group_code,
           group_name) %>%
    unique

# 3 Save datasets ###########################################################################

  write_rds(country_list,
            here("app",
                 "data",
                 "wb_country_list.rds"))

  write_rds(groups,
            here("app",
                 "data",
                 "wb_country_groups.rds"))
