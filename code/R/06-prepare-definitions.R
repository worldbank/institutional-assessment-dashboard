
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

definitions %>%
  write_rds(here("app",
                 "data",
                 "indicator_definitions.rds"))
