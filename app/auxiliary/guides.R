# Cicerone guides ------------------------------------------------------

## Guide landing page ------------------------------
guide_landing_page <- cicerone::Cicerone$
  new()$ 
  step(
    "emph_benchmark",
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

## Guide country benchmark tab ------------------------------
guide_benchmark <- cicerone::Cicerone$
  new()$ 
  step(
    "[data-id='country']",
    title = "Base country",
    description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut facilisis id purus vel eleifend. Ut vitae tellus in lectus fringilla.",
    is_id = F
  )$
  step(
    "[data-id='groups']",
    title = "Comparison groups",
    description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut facilisis id purus vel eleifend. Ut vitae tellus in lectus fringilla.",
    is_id = F
  )$
  step(
    "[data-id='family']",
    title = "Institutional family",
    description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut facilisis id purus vel eleifend. Ut vitae tellus in lectus fringilla.",
    is_id = F
  )$
  step( 
    "create_custom_grps",
    title = "Create custom groups",
    description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut facilisis id purus vel eleifend. Ut vitae tellus in lectus fringilla.",
    is_id = T
  )$
  step( 
    "benchmark_dots",
    title = "Show comparison countries",
    description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut facilisis id purus vel eleifend. Ut vitae tellus in lectus fringilla.",
    is_id = T
  )$
  step( 
    "rank",
    title = "Show rank instead of value",
    description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut facilisis id purus vel eleifend. Ut vitae tellus in lectus fringilla.",
    is_id = T
  )$
  step( 
    "[data-id='threshold']",
    title = "Benchmarking Thresholds",
    description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut facilisis id purus vel eleifend. Ut vitae tellus in lectus fringilla.",
    is_id = F
  )$
  step( 
    "download_row",
    title = "Download options",
    description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut facilisis id purus vel eleifend. Ut vitae tellus in lectus fringilla.",
    is_id = T
  )