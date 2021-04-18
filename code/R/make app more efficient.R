
definitions <-
  read_rds(file.path("data",
                     "indicator_definitions.rds"))

all_indicators <- bind_rows(definitions,
                            .id = "Institutional family")

write_rds(all_indicators,
          "data/list_of_indicators.rds")


wb_country_geom <-
  read_rds(file.path("data",
                     "wb_country_geom.rds"))

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
  left_join(wb_country_geom)

write_rds(wb_country_geom_fact,
          "data/wb_country_geom_fact.rds")
