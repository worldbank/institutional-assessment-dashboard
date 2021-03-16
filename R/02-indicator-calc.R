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
      .names="{.col}_{.fn}"
    )
  ) 
