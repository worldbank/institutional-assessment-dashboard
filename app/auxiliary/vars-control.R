db_variables <- read_rds(
  here(
    "data",
    "db_variables.rds"
  )
)

# 1. Anti-corruption institutions ======================
vars_anticorruption <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_anticorruption"
  ) %>%
  pull(variable)

# 2. Business environment and trade institutions ======================
vars_mkt <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_mkt"
  ) %>%
  pull(variable)

# 3. Climate Change and Environment Institutions ======================
vars_climate <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_climate"
  ) %>%
  pull(variable)

# 4. Digital and Data Institutions ======================
vars_digital <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_digital"
  ) %>%
  pull(variable)

# 5. Financial market institutions ======================
vars_fin <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_fin"
  ) %>%
  pull(variable)

# 6. Labor market institutions ======================
vars_lab <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_lab"
  ) %>%
  pull(variable)

# 7. Justice institutions ======================
vars_leg <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_leg"
  ) %>%
  pull(variable)

# 8. Political institutions ======================
vars_pol <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_pol"
  ) %>%
  pull(variable)

# 9. Public Human Resource Management Institutions ======================
vars_hrm <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_hrm"
  ) %>%
  pull(variable)

# 10. Public Public Financial Management Institutions ======================
vars_pfm <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_pfm"
  ) %>%
  pull(variable)

# 11. Social institutions ======================
vars_social <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_social"
  ) %>%
  pull(variable)

# 12. SOE Corporate Governance ======================
vars_service_del <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_service_del"
  ) %>%
  pull(variable)

# 13. Service Delivery Institutions ======================
vars_service_delivery <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_service_delivery"
  ) %>%
  pull(variable)

# 14. Transparency and Accountability institutions ======================
vars_transp <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_transp"
  ) %>%
  pull(variable)

# variables with other families ======================
vars_other <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_other"
  ) %>%
  pull(variable)

# removed variables ======================
vars_removed <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_removed"
  ) %>%
  pull(variable)

# variables with missing families ======================
vars_missing <-
  db_variables %>%
  filter(
    var_level == "indicator",
    family_var == "vars_missing"
  ) %>%
  pull(variable)

# All variables ======================
vars_all <-
  db_variables %>%
  filter(
    var_level == "indicator"
  ) %>%
  pull(variable)

# Families variables ======================
vars_family <-
  db_variables %>%
  filter(
    family_var != "vars_other"
  ) %>%
  pull(family_var) %>%
  unique

# vars_benchmarked --------------------------------------------------------
vars_static_ctf <- db_variables |>
  filter(
    benchmarked_ctf == "Yes"
  ) |>
  pull(variable)


vars_static_ctf <- db_variables |>
  filter(
    benchmarked_ctf == "Yes"
  ) |>
  pull(variable)


vars_dynamic_ctf <- db_variables |>
  filter(
    benchmark_dynamic_indicator == "Yes"
  ) |>
  pull(variable)

vars_dynamic_partial_ctf <- db_variables |>
  filter(
    benchmark_dynamic_family_aggregate == "Partial"
  ) |>
  pull(variable)

vars_static_family_ctf <- db_variables |>
  filter(
    benchmark_static_family_aggregate_download == "Yes"
  ) |>
  pull(variable)


vars_dynamic_family_ctf <- db_variables |>
  filter(
    benchmark_dynamic_family_aggregate == "Yes"
  ) |>
  pull(variable)



vars_dynamic_family_ctf <- db_variables |>
  filter(
    benchmark_dynamic_family_aggregate == "Yes"
  ) |>
  pull(variable)
# extract family names

family_names <-
  db_variables %>%
  # exclude extraneous families
  filter(
    family_var != "vars_other" &
      family_var != "vars_missing"
  ) %>%
  transmute(
    variable = family_var,
    var_name = family_name
  ) %>%
  unique

