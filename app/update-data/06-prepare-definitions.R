definitions <-
  read_csv2(
    file.path(
      "data",
      "definitions",
      "indicator_definitions.csv"
    )
  )

definitions <-
  definitions %>%
  mutate(
    family = case_when(
      family == "Legal Institutions" ~ "Legal institutions",
      family == "Social Institutions" ~ "Social institutions",
      family == "Political Institutions" ~ "Political institutions",
      family == "Public sector performance Institutions" ~ "Public sector performance institutions",
      TRUE ~ family
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
