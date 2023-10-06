raw_indicators<-readRDS("data/output/compiled_indicators.RDS")

vars_raw<-names(raw_indicators)[4:length(names(raw_indicators))]
raw_indicator_long <-
  raw_indicators %>%
  pivot_longer(
    all_of(vars_raw),
    names_to = "variable"
  ) %>%
  select(-contains("gdp")) %>%
  left_join(
    db_variables %>%
      select(variable, var_name, family_name, family_var),
    by = "variable"
  )

raw_indicator_long_clean <-
  ctf_dynamic_long %>%
  group_by(family_name, family_var, country_name, country_code, year) %>%
  summarise(value = median(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    variable = family_var,
    var_name = family_name
  ) %>%
  bind_rows(raw_indicator_long)


raw_indicator_long_clean_family_level <- raw_indicator_long_clean %>%
  distinct(family_name, family_var, country_name, country_code,
variable, var_name,  year,
           .keep_all = TRUE
  ) %>%
  group_by(
    family_name, family_var, country_name, country_code,
   year
  ) %>%
  summarise(value = mean(value[family_name != var_name], na.rm = TRUE)) %>%
  rowwise() %>%
  mutate(
    variable = paste0(family_var, "_avg"),
    var_name = (paste0(family_name, " Average"))
  ) %>%
  left_join(raw_indicator_long_clean %>%
              distinct(
                family_name, family_var, country_name,
                country_code, year
              )) %>%
  ungroup()

raw_indicator_long_clean <- raw_indicator_long_clean %>%
  bind_rows(raw_indicator_long_clean_family_level) %>%
  arrange(
    family_name, family_var, country_name, country_code,
    year, variable, var_name
  )

raw_indicator_clean <- raw_indicator_long_clean_family_level %>%
  distinct() %>%
  select(-family_name, -family_var, -var_name) %>%
  mutate(value = ifelse(is.nan(value), NA, value)) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  full_join(raw_indicators, ., by = c("country_code", "country_name", "year"))

write_rds(
  raw_indicator_clean,
  here(
    "data",
    "output",
    "compiled_indicators_updated.rds"
  )
)

