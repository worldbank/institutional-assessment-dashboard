## Load packages

packages <- c("tidyverse",
              "here",
              "skimr",
              "labelled")

pacman::p_load(
  packages,
  character.only = TRUE
)


# Variables ----------------------------------------------------------

source(
  here(
    "app",
    "auxiliary",
    "vars-control.R"
  )
)

source(
  here(
    "app",
    "auxiliary",
    "vars-by-family.R"
  )
)

# Data ---------------------------------------------------------------

definitions <-
  read_rds(
    here(
      "data",
      "constructed",
      "indicator_definitions.rds"
    )
  )

# Map Function -------------------------------------------------------

all_years <- list(c(2014:2020), c(2014:2016), c(2017:2020))

map(1:length(all_years), function(x){
  condition <- all_years[[x]]

  # Create data name conditions
  if (min(condition)==2014 & max(condition)==2020){
    data_name <- ""
  } else if (min(condition)==2014 & max(condition)==2016){
    data_name <- "_2014_2016"
  } else if (min(condition)==2017 & max(condition)==2020){
    data_name <- "_2017_2020"
  }

  ## 1. Read data

  data <-
    read_rds(
      here(
        "app",
        "data",
        "raw_data.rds"
      )
    ) %>%
    # read_rds(
    #   here(
    #     "data",
    #     "final",
    #     "compiled_indicators.rds"
    #   )
    # ) %>%
    filter(
      year %in% condition
    )

  ## 2. Calculate country-level average for each indicator
  country_average <-
    data %>%
    group_by(
      country_code,
      country_name
    ) %>%
    summarise(
      across(
        all_of(vars_all),
        ~ mean(., na.rm = TRUE)
      )
    )

  ## 3. Identify worst and best performance for each indicator

  min_max <-
    data %>%
    summarise(
      across(
        all_of(vars_all),
        list(
          min = ~ min(., na.rm = TRUE),
          max = ~ max(., na.rm = TRUE)
        ),
        .names="{.col}-{.fn}"
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to = c("variable", ".value"),
      names_pattern = "(.*)-(.*)"
    )

  ## 4. Calculate closeness to frontier at indicator level

  ctf <-
    country_average %>%
    pivot_longer(
      all_of(vars_all),
      names_to = "variable"
    ) %>%
    left_join(
      min_max,
      by = "variable"
    ) %>%
    mutate(
      ctf = (min - value) / (min - max),
      ctf = ifelse(
        ctf == 0,
        0.01,
        ctf
      )
    ) %>%
    pivot_wider(
      id_cols = c("country_name", "country_code"),
      names_from = "variable",
      values_from = "ctf"
    ) %>%
    select(-starts_with("gdp")) %>%
    left_join(
      country_average %>%
        select(country_name, country_code)
    ) #%>%
  # mutate(
  #   log_gdp = log(gdp_pc_ppp_const) # Not available in the raw data
  # )


  ## 5. Calculate median per group

  country_list <-
    read_rds(
      here(
        "data",
        "final",
        "wb_country_list.rds"
      )
    )

  group_ctf <-
    country_list %>%
    left_join(
      ctf
    ) %>%
    group_by(
      group_code, group
    ) %>%
    summarise(
      across(
        all_of(vars_all),
        ~ median(., na.rm = TRUE)
      )
    ) %>%
    filter(!is.na(group)) %>%
    select(
      group,
      group_code,
      all_of(vars_all)
    ) %>%
    rename(
      country_name = group,
      country_code = group_code
    )

  # 6. CTF data
  ctf <-
    ctf %>%
    bind_rows(group_ctf) %>%
    ungroup

  write_rds(
    ctf,
    here(
      "data",
      "final",
      paste0("closeness_to_frontier", data_name, ".rds")
    )
  )

  ctf_long <-
    ctf %>%
    pivot_longer(
      all_of(vars_all),
      names_to = "variable"
    ) %>%
    select(-contains("gdp")) %>%
    left_join(
      db_variables %>%
        select(variable, var_name, family_name, family_var)
    ) %>%
    left_join(
      country_list %>%
        select(country_name, group)
    )

  ctf_long <-
    ctf_long %>%
    group_by(family_name, family_var, country_name, country_code, group) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(
      variable = family_var,
      var_name = family_name
    ) %>%
    bind_rows(ctf_long)

  write_rds(
    ctf_long,
    here(
      "data",
      "final",
      paste0("closeness_to_frontier_long", data_name, ".rds")
    )
  )

  ## 7. Send to app data
  write_rds(
    ctf,
    here(
      "app",
      "data",
      paste0("closeness_to_frontier", data_name, ".rds")
    )
  )

  write_rds(
    ctf_long,
    here(
      "app",
      "data",
      paste0("closeness_to_frontier_long", data_name, ".rds")
    )
  )

})










