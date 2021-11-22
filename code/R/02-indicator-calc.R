# Load packages
packages <- c("tidyverse",
              "here",
              "skimr",
              "labelled")

pacman::p_load(packages,
               character.only = TRUE)

data_selected <-
  read_rds(here("data",
                "data_cleaned",
                "selected_vars.rds"))

source(file.path("app/auxiliary",
                 "vars-by-family.R"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CALCULATE GLOBAL CLOSENESS TO FRONTIER FOR EACH INDICATOR AND FOR EACH COUNTRY -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# methodological notes:
# closeness to frontier (CTF) is global, meaning that we identify the worst and best performance in the full sample (all countries)
# with the closeness to frontier methodology, for each indicator i, we compare the last available value of indicator i with the worst and best
# performance for indicator i among all countries and in the last Y years (2013 - most recent data)
# in the graph that we want to produce, the length of the bars represents the closeness to frontier

# identify worst and best performance for each indicator, for each country (2013 - most recent data)
# methodological note: in doing business they consider the last 5 years, but here for some indicators we have shorter time series
# reference: https://www.doingbusiness.org/content/dam/doingBusiness/media/Annual-Reports/English/DB17-Chapters/DB17-DTF-and-DBRankings.pdf

# Get min and max for each variable
vars_minmax <-
  data_selected %>%
  summarise(
    across(
      all_of(vars_all),
      list(
        min = ~ min(.x[year >= 2013], na.rm = T), # year >= as.numeric(format(Sys.Date(), "%Y"))-8): Change eventually to filter for the last 7 years and disable next line
        max = ~ max(.x[year >= 2013], na.rm = T)  # year >= as.numeric(format(Sys.Date(), "%Y"))-8): Change eventually to filter for the last 7 years and disable next line
      ),
      .names="{.col}-{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to="variable",
    values_to = "value_minmax",
    values_drop_na = F
  ) %>%
  separate(
    variable,
    into = c("variable", "minmax"),
    sep = "-"
  ) %>%
  pivot_wider(
    id_cols = "variable",
    names_from = "minmax",
    values_from = "value_minmax"
  ) %>%
  remove_labels()

# Collapse at country level, keeping the most recent data for each indicator
#data_recent_country <-
#  data_selected %>%
#  arrange(country_name,
#          year) %>%
#  group_by(country_name) %>%
#  fill(
#    all_of(vars_global)
#  ) %>%
#  filter(
#    year == max(year)
#  ) %>%
#  select(-year)

# Collapse at country level. for each country, keep only the average since 2013
# SC: in the long term, this step should be flexibly adjusted in the dashboard (keep last 7 years, given the present time)
data_country <-
  data_selected %>%
  group_by(country_name,
           country_code#,
           #lac,lac6,oecd,
           #structural
           ) %>%
  summarise(
    across(
      all_of(vars_all),
      ~mean(.x[year>=2013], na.rm = T)  # year >= as.numeric(format(Sys.Date(), "%Y"))-8): Change eventually to filter for the last 7 years and disable next line
    )
  ) %>%
  mutate(
    across(
      all_of(vars_all),
      ~ifelse(is.nan(.x),NA,.x)
    )
  )

# Calculate closeness to frontier at indicator level
dtf_vars_global <-
  data_country %>%
  remove_labels %>%
  pivot_longer(
    all_of(vars_all),
    names_to = "variable",
    values_drop_na = F
  ) %>%
  left_join(vars_minmax,
            by = "variable") %>%
  mutate(
    dtf = (min - value) / (min - max),
    dtf = ifelse(dtf == 0,
                 0.01,
                 dtf) # small adjustments to display a very short bar on the graph, in case dtf = 0
  ) %>%
  pivot_wider(
    id_cols = c("country_name", "country_code"),
    names_from = "variable",
    values_from = "dtf"
  )


# Save datasets ====================================================

write_rds(dtf_vars_global,
          here("data",
               "data_cleaned",
               "dtf_vars_global.rds"))

write_rds(dtf_vars_global,
          here("app",
               "data",
               "country_dtf.rds"))
