library(tidyr)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CALCULATE GLOBAL CLOSENESS TO FRONTIER FOR EACH INDICATOR AND FOR EACH COUNTRY -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# methodological notes:
# closeness to frontier (CTF) is global, meaning that we identify the worst and best performance in the full sample (all countries)
# with the closeness to frontier methodology, for each indicator i, we compare the last available value of indicator i with the worst and best 
# performance for indicator i among all countries and in the last Y years (2013 - most recent data)
# in the graph that we want to produce, the length of the bars represents the closeness to frontier

# identify worst and best performance for each indicator, for each country (2013 - most recent data)
# methodological note: in doing business they consider the last 5 years, but here for some indicators we have shorter time series
# reference: https://www.doingbusiness.org/content/dam/doingBusiness/media/Annual-Reports/English/DB17-Chapters/DB17-DTF-and-DBRankings.pdf

# Get min and max for each variable
vars_minmax <- data_selected %>%
  filter(year >= 2013) %>%
  summarise(
    across(
      all_of(vars_global),
      list(
        min = ~ min(.x, na.rm = T),
        max = ~ max(.x, na.rm = T)
      ),
      .names="{.col}-{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to="variable",
    values_to = "value_minmax",
    values_drop_na = F
  ) %>%
  separate(
    variable,
    into = c("variable","minmax"),
    sep = "-"
  ) %>%
  pivot_wider(
    id_cols = "variable",
    names_from = "minmax",
    values_from = "value_minmax"
  ) %>%
  labelled::remove_labels() 

# Collapse at country level, keeping the most recent data for each indicator
data_recent_country <- data_selected %>%
  arrange(country,year) %>%
  group_by(country) %>%
  fill(
    all_of(vars_global)
  ) %>%
  filter(
    year == max(year)
  ) %>%
  select(-year)

# Calculate closeness to frontier at indicator level
dtf_vars_global <- data_recent_country %>%
  labelled::remove_labels() %>%
  pivot_longer(
    all_of(vars_global),
    names_to="variable",
    values_drop_na = F
  ) %>%
  left_join(vars_minmax,by="variable") %>%
  mutate(
    dtf = (min - value) / (min - max),
    dtf = ifelse(dtf==0,0.01,dtf) # small adjustments to display a very short bar on the graph, in case dtf = 0
  ) %>%
  pivot_wider(
    id_cols = c("country","lac","lac6","oecd","structural"),
    names_from = "variable",
    names_prefix = "dtf_",
    values_from = "dtf"
  ) 
