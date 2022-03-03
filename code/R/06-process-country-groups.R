# 1 Set up ########################################################################################

# 1.1 Packages ===================================================================================

  packages <-
    c("tidyverse",
      "here")

  pacman::p_load(packages,
                 character.only = TRUE)

# 1.2 Inputs ====================================================================================

  global_data <-
    read_rds(
      here(
        "app",
        "data",
        "country_dtf.rds"
      )
    )

  country_list <-
    read_csv(here("data",
                  "data_raw",
                  "wb_country_list.csv")) %>%
    rename(country_name = country) %>%
    filter(country_name %in% global_data$country_name)


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

  groups <-
    groups %>%
    mutate(
      group_category = case_when(
        group_code %in% c("HIC","LIC","LMC","LMY","MIC","UMC") ~ "Income",
        group_code %in% c("OED","EUU") ~ "Economic",
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
