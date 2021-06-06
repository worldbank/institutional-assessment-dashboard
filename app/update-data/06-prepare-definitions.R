definitions <-
  read_csv(
    file.path(
      "data",
      "data_cleaned",
      "indicator_definitions.csv"
    )
  )

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
          file.path(
            "data",
            "list_of_indicators.rds"))

definitions %>%
  write_rds(
    file.path(
      "data",
      "indicator_definitions.rds"
    )
  )
