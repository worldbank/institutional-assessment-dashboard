
definitions <-
  read_csv(here("data",
                "data_raw",
                "indicator_definitions.csv"))


families <-
  definitions$family

definitions <-
  definitions %>%
  arrange(var_name) %>%
  select(-c(family, var_name)) %>%
  split(families)

all_indicators <- bind_rows(definitions,
                            .id = "Institutional family")

write_rds(all_indicators,
          here("app",
               "data",
               "list_of_indicators.rds"))

definitions %>%
  write_rds(here("app",
                 "data",
                 "indicator_definitions.rds"))
