data <-
  read_rds(here("app",
                "data",
                "country_dtf.rds")) %>%
  select(-country_code)

old_data <-
  read_rds(here("app",
                "data",
                "old_data.rds")) %>%
  ungroup %>%
  select(-c(country_code:structural))

variable_names <-
  read_rds(here("app",
                "data",
                "variable_names.rds"))

old_variable_names <-
  read_rds(here("app",
                "data",
                "old_variable_names.rds"))


old <- 
  old_data %>% 
  filter(country_name == "Uruguay") %>%
  ungroup %>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "variable",
               values_to = "old_ctf") %>%
  left_join(old_variable_names,
            by = "variable") %>%
  select(variable, family_name, old_ctf)  %>%
  group_by(family_name) %>%
  mutate(mean_old = mean(old_ctf, na.rm = TRUE))

new <- 
  data %>% 
  filter(country_name == "Uruguay") %>%
  ungroup %>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "variable",
               values_to = "new_ctf") %>%
  left_join(variable_names,
            by = "variable") %>%
  select(variable, family_name, new_ctf) %>%
  group_by(family_name) %>%
  mutate(mean_new = mean(new_ctf, na.rm = TRUE))

compare <-
  full_join(old, new) %>%
  group_by(family_name) %>%
  arrange(family) %>%
  write_csv("C:/Users/wb501238/Downloads/compare.csv")
