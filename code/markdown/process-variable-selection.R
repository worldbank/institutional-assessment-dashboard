db_variables <-
  readxl::read_excel(
    here(
      "data",
      "db_variables.xlsx"
    )
  ) %>%
  filter(select != 0)

write_rds(
  db_variables,
  here(
    "data",
    "final",
    "db_variables.rds"
  )
)


  description<- function(x) {
    assign(
      x,
      db_variables %>%
        filter(family_name == x) %>%
        select(
          Indicator = var_name,
          Description = description,
          Source= source
        )
    )

  }

  description <-
    lapply(
      unique(db_variables$family_name),
      description
    )

  names(description) <- unique(db_variables$family_name)

  write_rds(
    description,
    here(
      "data",
      "final",
      "definitions.rds"
    )
  )
