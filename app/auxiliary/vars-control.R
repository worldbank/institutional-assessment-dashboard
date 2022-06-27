# Anti-Corruption, Transparency and Accountability institutions ======================
vars_transp <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_transp"
  ) %>%
  pull(variable)

# Business environment and trade institutions ======================
vars_mkt <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_mkt"
  ) %>%
  pull(variable)

# Financial market institutions ======================
vars_fin <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_fin"
  ) %>%
  pull(variable)

# Labor market institutions ======================
vars_lab <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_lab"
  ) %>%
  pull(variable)

# Legal institutions ======================
vars_leg <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_leg"
  ) %>%
  pull(variable)

# Political institutions ======================
vars_pol <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_pol"
  ) %>%
  pull(variable)

# Public sector performance institutions ======================
vars_publ <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_publ"
  ) %>%
  pull(variable)

# Social institutions ======================
vars_social <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_social"
  ) %>%
  pull(variable)

# SOE Corporate Governance ======================
vars_service_del <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_service_del"
  ) %>%
  pull(variable)

# All variables ======================
vars_all <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator"
  ) %>%
  pull(variable)

# Families variables ======================
family_names <-
  db_variables %>%
  filter(
    var_level == "family"
  ) %>%
  pull(variable)

# Variables from Original source  ======================
vars_original <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    data_source == "original"
  ) %>%
  pull(variable)

# Variables from Gov360 API ======================
vars_api <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    data_source == "api"
  ) %>%
  pull(variable)

# Variables from Additions source  ======================
vars_additions <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    data_source == "additions"
  ) %>%
  pull(variable)



