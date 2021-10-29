# 1 Set up ################################################################################################

# 1.1 Packages ============================================================================================

packages <-
  c("tidyverse",
    "rmapshaper",
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

wb_disputed_geom <-
  read_sf(here("data",
               "layer",
               "WB_disputed_areas_Admin0_10m",
               "WB_disputed_areas_Admin0_10m.shp"))

# 2 Operations ============================================================================================

wb_disputed_geom <-
  wb_disputed_geom %>%
  select(WB_A3,WB_NAME,geometry) %>%
  filter(!is.na(WB_A3))

wb_country_geom <-
  bind_rows(
    wb_country_geom,
    wb_disputed_geom
  )

wb_country_geom <-
  wb_country_geom %>%
  select(WB_A3,WB_NAME,geometry) %>%
  left_join(
    dtf_family_level %>%
      select(-country_name),
    by=c("WB_A3"="country_code")
  ) %>%
  left_join(
    dtf_vars_global %>%
      ungroup() %>%
      select(-c(country_name,lac,lac6,oecd#,
                #structural
                )),
    by=c("WB_A3"="country_code")
  )

wb_country_geom_fact <-
  wb_country_geom %>%
  mutate_at(vars(vars_pol:trust_pol),
            ~ case_when(. <= 0.2 ~ "0.0 - 0.2",
                        . > 0.2 & . <= 0.4 ~ "0.2 - 0.4",
                        . > 0.4 & . <= 0.6 ~ "0.4 - 0.6",
                        . > 0.6 & . <= 0.8 ~ "0.6 - 0.8",
                        . > 0.8 ~ "0.8 - 1.0",
                        is.na(.) ~ "Not avaiable") %>%
              as.factor
  )

wb_country_geom <-
  wb_country_geom %>%
  st_drop_geometry() %>%
  rename_with(
    ~ paste0(., "_value"),
    where(is.numeric),
  ) %>%
  mutate_at(vars(ends_with("_value")),
            ~ round(., 3) %>% as.character()) %>%
  as_tibble()

wb_country_geom_fact <-
  wb_country_geom_fact %>%
  left_join(wb_country_geom) %>%
  ms_simplify(
    keep = 0.04,
    keep_shapes = T
  )

# 3 Save datasets =========================================================================================

write_rds(wb_country_geom_fact,
          here("app",
               "data",
               "wb_country_geom_fact.rds"))
