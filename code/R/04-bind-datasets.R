original_data <-
  read_rds(
    here(
      "data",
      "clean",
      "original_data.rds"
    )
  )

additional_data <-
 read_rds(
   here(
     "data",
     "clean",
     "additional_data.rds"
   )
 )

api_data <-
  read_rds(
    here(
      "data",
      "clean",
      "api_data.rds"
    )
  )

full_data <-
  original_data %>%
  left_join(
    additional_data,
    by = c("country_code", "year")
  ) %>%
  left_join(
    api_data,
    by = c("country_code","year")
  )

# count number of missing values
full_data$na_count <- apply(is.na(full_data), 1, sum)


# Save binded datasets ====================================================
write_rds(
  full_data,
  here(
    "data",
    "final",
    "compiled_indicators.rds"
  )
)
