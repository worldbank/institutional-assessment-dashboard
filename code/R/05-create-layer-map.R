# 1 Set up ################################################################################################

# 1.1 Packages ============================================================================================

packages <-
  c("tidyverse",
    "sf",
    "here")

pacman::p_load(packages,
               character.only = TRUE)

# 1.2 Load Inputs ==========================================================================================

#dtf_vars_global <-
#  read_rds(here("data",
#                "data_cleaned",
#                "dtf_vars_global.rds"))

dtf_family_level <-
  read_rds(here("data",
                "data_cleaned",
                "dtf_family_level.rds"))

wb_country_geom <-
  read_sf(here("data",
               "layer",
               "WB_countries_Admin0_10m.shp"))

# 2 Operations ============================================================================================

wb_country_geom <-
  wb_country_geom %>%
  filter(TYPE=="Sovereign country" | TYPE=="Country") %>%
  select(WB_A3,WB_NAME,geometry) %>%
  inner_join(
    dtf_family_level %>%
      select(-country_name),
    by=c("WB_A3"="country_code")
  ) #%>%
  #inner_join(
  #  dtf_vars_global %>%
  #    ungroup() %>%
  #    select(-c(country_name,lac,lac6,oecd,structural)),
  #  by=c("WB_A3"="country_code")
  #)

# 3 Save datasets =========================================================================================

write_rds(wb_country_geom,
          here("app",
               "data",
               "wb_country_geom.rds"))
