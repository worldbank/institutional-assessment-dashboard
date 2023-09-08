db_variables <-
  read_rds(
    here(
      "data",
      "output",
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
  filter(family_name != "Other") %>%
  pull(family_var) %>%
  unique

# Variables from Original source  ======================
vars_original <-
  db_variables %>%
  filter(
    var_level == "indicator",
    data_source == "original"
  ) %>%
  pull(variable)

# Variables from Gov360 API ======================
vars_api <-
  db_variables %>%
  filter(
    var_level == "indicator",
    data_source == "API"
  ) %>%
  pull(variable)

# Variables from Additions source  ======================
vars_additions <-
  db_variables %>%
  filter(
    var_level == "indicator",
    data_source == "additions"
  ) %>%
  pull(variable)


family_names <-
  db_variables %>%
  filter(family_name != "Other") %>%
  transmute(
    variable = family_var,
    var_name = family_name
  ) %>%
  unique

