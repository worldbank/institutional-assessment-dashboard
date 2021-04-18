
definitions <-
  read_rds(file.path("data",
                     "indicator_definitions.rds"))

all_indicators <- bind_rows(definitions,
                            .id = "Institutional family")

write_rds(all_indicators,
          "data/list_of_indicators.rds")




