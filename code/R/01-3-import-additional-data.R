# Load packages ----
packages <- c("tidyverse",
              "here")

pacman::p_load(packages,
               character.only = TRUE)

# Import data ----
additions <- haven::read_dta(here("data",
                                  "data_raw",
                                  "20211118_new_additions_notGov360.dta")) %>%
  filter(year >= 2015) %>%
  rename(country_code = iso3code) %>%
  select(-c(countryname,country,ccode)) %>%
  mutate(
    year=as.character(year)
  ) %>%
  labelled::remove_labels() %>%
  group_by(country_code) %>%
  summarise(
    across(
      where(is.numeric),
      mean,
      na.rm = TRUE
    )
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ifelse(is.nan(.x),NA,.x)
    )
  ) %>%
  mutate(
    year = "2020",
    proff1 = proff1*(-1)
  ) %>%
  relocate(year, .after = country_code) %>%
  ## Keep only indicators from original data ----
  select(
    country_code,
    year,
    all_of(vars_additions)
  )
