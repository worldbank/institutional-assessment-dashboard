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

  extra_groups <- read_rds(here("data",
                                "data_cleaned",
                                "extra_groups.rds")) %>%
    labelled::remove_labels()


  group_codes <-
    c("EUU",
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

  groups <-
    country_list %>%
    filter(group_code %in% group_codes) %>%
    rename(group_name = group) %>%
    select(group_code,
           group_name) %>%
    unique

  lac6 <- extra_groups %>%
    filter(lac6==1) %>%
    select(country_code,country_name) %>%
    mutate(
      group_code="LAC6",
      group="LAC6"
    )

  country_list <-
    country_list %>%
    bind_rows(lac6)

  groups <- bind_rows(
    groups,
    data.frame(group_code=c("LAC6"),group_name=c("LAC6"))
  )

# 3 Save datasets ###########################################################################

  write_rds(country_list,
            here("app",
                 "data",
                 "wb_country_list.rds"))

  write_rds(groups,
            here("app",
                 "data",
                 "wb_country_groups.rds"))
