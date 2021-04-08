# 1 Set up ################################################################################################

# 1.1 Packages ============================================================================================

packages <-
  c("tidyverse",
    "sf",
    "here")

pacman::p_load(packages,
               character.only = TRUE)

# 1.2 Load Inputs ==========================================================================================

dtf_vars_global <-
  read_rds(here("data",
                "data_cleaned",
                "dtf_vars_global.rds"))

dtf_family_level <-
  read_rds(here("data",
                "data_cleaned",
                "dtf_family_level.rds"))

wb_country_geom <-
  read_sf(here("data",
               "layer",
               "WB_countries_Admin0_lowres.geojson"))

# 2 Operations ============================================================================================

wb_country_geom <-
  wb_country_geom %>%
  filter(TYPE=="Sovereign country" | TYPE=="Country") %>%
  select(WB_A3,WB_NAME,geometry) %>%
  left_join(
    dtf_family_level %>%
      select(-country_name),
    by=c("WB_A3"="country_code")
  ) %>%
  left_join(
    dtf_vars_global %>%
      ungroup() %>%
      select(-c(country_name,lac,lac6,oecd,structural)),
    by=c("WB_A3"="country_code")
  ) %>%
  mutate(
    label = paste0(
      "<b>", wb_country_geom$WB_NAME,
      "</b><br/>Labor Average CTF: ",formatC(wb_country_geom$vars_lab, digits = 3, format = "f"),
      "</b><br/>Financial Average CTF: ",formatC(wb_country_geom$vars_fin, digits = 3, format = "f"),
      "</b><br/>Legal Average CTF: ",formatC(wb_country_geom$vars_leg, digits = 3, format = "f"),
      "</b><br/>Political Average CTF: ",formatC(wb_country_geom$vars_pol, digits = 3, format = "f"),
      "</b><br/>Social Average CTF: ",formatC(wb_country_geom$vars_social, digits = 3, format = "f"),
      "</b><br/>Business and Trade Average CTF: ",formatC(wb_country_geom$vars_mkt, digits = 3, format = "f"),
      "</b><br/>Public Sector Average CTF: ",formatC(wb_country_geom$vars_publ, digits = 3, format = "f"),
      "</b><br/>Governance of SOEs Average CTF: ",formatC(wb_country_geom$vars_service_del, digits = 3, format = "f"),
      "</b><br/>Accountability Average CTF: ",formatC(wb_country_geom$vars_transp, digits = 3, format = "f")
    )
  )


# 3 Save datasets =========================================================================================

write_rds(wb_country_geom,
          here("app",
               "data",
               "wb_country_geom.rds"))
