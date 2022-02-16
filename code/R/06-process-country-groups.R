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

  lac6 <-
    extra_groups %>%
    filter(lac6 == 1) %>%
    select(country_code,country_name) %>%
    mutate(
      group_code = "LAC6",
      group = "LAC6"
    )

  structural <-
    extra_groups %>%
    filter(structural == 1) %>%
    select(country_code,country_name) %>%
    mutate(
      group_code = "STR",
      group = "Structural"
    )

  country_list <-
    country_list %>%
    bind_rows(lac6,
              structural)

  groups <-
    groups %>%
    bind_rows(
      data.frame(group_code = c("LAC6", "STR"),
                 group_name = c("LAC6", "Structural"))
    ) %>%
    mutate(
      group_category = case_when(
        group_code %in% c("HIC","LIC","LMC","LMY","MIC","UMC") ~ "Income",
        group_code %in% c("OED","LAC6","EUU", "STR") ~ "Economic",
        TRUE ~ "Region"
      )
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
