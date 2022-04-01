packages <-
  c("tidyverse",
    "here")

pacman::p_load(packages,
               character.only = TRUE)

db_variables <-
  readxl::read_excel(
    here(
      "data",
      "db_variables.xlsx"
    )
  )

db_variables$variable[db_variables$api_id==946] <- "gdp_pc_ppp_const"
db_variables$select[db_variables$api_id==946] <- 1
db_variables$family_var[db_variables$api_id==946] <- "vars_other"
db_variables$family_name[db_variables$api_id==946] <- "Other"

# Anti-Corruption, Transparency and Accountability institutions ======================
vars_transp <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_transp"
  ) %>%
  .$variable

# Business environment and trade institutions ======================
vars_mkt <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_mkt"
  ) %>%
  .$variable

# Financial market institutions ======================
vars_fin <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_fin"
  ) %>%
  .$variable

# Labor market institutions ======================
vars_lab <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_lab"
  ) %>%
  .$variable

# Legal institutions ======================
vars_leg <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_leg"
  ) %>%
  .$variable

# Political institutions ======================
vars_pol <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_pol"
  ) %>%
  .$variable

# Public sector performance institutions ======================
vars_publ <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_publ"
  ) %>%
  .$variable

# Social institutions ======================
vars_social <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_social"
  ) %>%
  .$variable

# SOE Corporate Governance ======================
vars_service_del <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    family_var == "vars_service_del"
  ) %>%
  .$variable

# All variables ======================
vars_all <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator"
  ) %>%
  .$variable

# Families variables ======================
family_names <-
  db_variables %>%
  filter(
    var_level == "family"
  ) %>%
  .$variable

# Variables from Original source  ======================
vars_original <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    data_source == "original"
  ) %>%
  .$variable

# Variables from Gov360 API ======================
vars_api <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    data_source == "api"
  ) %>%
  .$variable

# Variables from Additions source  ======================
vars_additions <-
  db_variables %>%
  filter(
    select == 1,
    var_level == "indicator",
    data_source == "additions"
  ) %>%
  .$variable



