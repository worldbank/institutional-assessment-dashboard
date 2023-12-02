# Cicerone guide ------------------------------------------------------
guided_tour <- cicerone::Cicerone$
  new()$ 
  step(
    "tab-benchmark",
    title = "Country benchmarking",
    description = "The country benchmarking tab shows how one country compares to another group of countries in terms of closeness to frontier for each relevant indicator and institutional cluster. It works best with a relatively large group of comparator countries."
  )$
  step(
    "tab-country",
    title = "Cross-country comparison",
    description = "The cross-country comparison tab shows how one country compares to another group of countries for each relevant indicator. It works even with a few comparator countries."
  )$
  step(
    "tab-scatter",
    title = "Bivariate correlation",
    description = "The bivariate correlation tab shows correlations between the closeness to frontier scores for pairs of indicators"
  )$
  step(
    "tab-world_map",
    title = "World map",
    description = "The world map tab shows the closeness to frontier of a given indicator for all countries with available data."
  )$
  step(
    "tab-trends",
    title = "Time trends",
    description = "The time trends tab shows the evolution year by year of multiple indicators."
  )$
  step(
    "tab-data",
    title = "Data",
    description = "The data tab provides an interactive table containing the closeness to frontier data for all countries. It also allows users to download the data in different formats."
  )