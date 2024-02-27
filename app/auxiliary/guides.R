# Cicerone guides ------------------------------------------------------

## Guide landing page ------------------------------
#guide_landing_page <- cicerone::Cicerone$
#  new()$ 
#  step(
#    "emph_benchmark",
#    title = "Country benchmarking",
#    description = "The country benchmarking tab shows how one country compares to another group of countries in terms of closeness to frontier for each relevant indicator and institutional cluster. It works best with a relatively large group of comparator countries."
#  )$
#  step(
#    "tab-country",
#    title = "Cross-country comparison",
#    description = "The cross-country comparison tab shows how one country compares to another group of countries for each relevant indicator. It works even with a few comparator countries."
#  )$
#  step(
#    "tab-scatter",
#    title = "Bivariate correlation",
#    description = "The bivariate correlation tab shows correlations between the closeness to frontier scores for pairs of indicators"
#  )$
#  step(
#    "tab-world_map",
#    title = "World map",
#    description = "The world map tab shows the closeness to frontier of a given indicator for all countries with available data."
#  )$
#  step(
#    "tab-trends",
#    title = "Time trends",
#    description = "The time trends tab shows the evolution year by year of multiple indicators."
#  )$
#  step(
#    "tab-data",
#    title = "Data",
#    description = "The data tab provides an interactive table containing the closeness to frontier data for all countries. It also allows users to download the data in different formats."
#  )

## Guide country benchmark tab ------------------------------
guide_benchmark <- cicerone::Cicerone$
  new()$ 
  step(
    "[data-id ='country']",
    title = "Base country",
    description = "Choose the base country of interest. (For some analysis, you can select more than one.)",
    is_id = F
  )$
  step(
    "[data-id ='groups']",
    title = "Pre-defined groups",
    description = "There are multiple ways to select the comparator countries. Here you can select one (or more) pre-defined group(s) (either as a comparator group itself or as a shortcut for selecting individual countries). When selecting more than one, it is the union (i.e., sum) of the groups that will be analyzed.",
    is_id = F
  )$
  step( 
    "show_countries_column",
    title = "List of countries",
    description = "Here you can add and remove individual comparator countries. If you have already selected one or more the pre-defined groups, those countries will appear as selected, and you can manually add or remove.",
    is_id = T
  )$
  step( 
    "custom_grps_column",
    title = "Custom groups",
    description = "Alternative, you may create up to three custom groups of countries. This feature will additionally display in the Benchmarking graphs the median estimates of each custom group.",
    is_id = T
  )$
  step( 
    "input_buttons",
    title = "Saving and loading Selection of Countries",
    description = "You can save your selected inputs to return to at a future time: click “Save Selection of Countries” button to download a .rds file to your computer with that information. When you return to the dashboard, you can click “Load Selection of Countries” button and then “Browse” to select this same .rds file. Loading this .rds file will re-populate all of the selections that you previously made.",
    is_id = T
  )$
  step(
    "[data-id ='family']",
    title = "Institutional cluster",
    description = "Choose the institutional cluster you would like to display. The overview displays the aggregate results at the institutional-cluster level. When selecting a specific institutional-cluster, the individual indicators/components will be displayed.",
    is_id = F
  )$
  step(
    "select",
    title = "Apply",
    description = "Click on this box to (re-)run the analysis and (re-)load the resulting graphs. Note that this has to be done for every new selection or option, including a different institutional cluster. This option is enabled when the base country and at least 10 comparison countries are selected.",
    is_id = T
  )$
  step( 
    "download_reports",
    title = "Pre-populated reports",
    description = "Download pre-populated Word or Power Point documents with the results. Note that you may select the “Download Advanced Report (~10 min)” box to receive more detailed information - including all dynamic graphs - which takes longer to produce and download.",
    is_id = T
  )$
  step( 
    "download_data_opt",
    title = "Data used in graphs",
    description = "Click the “Download Data” button to download a CSV file that contains the data needed to recreate the benchmarking graphs.",
    is_id = T
  )
 #step( 
 #  "benchmark_dots",
 #  title = "Show comparison countries",
 #  description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut facilisis id purus vel eleifend. Ut vitae tellus in lectus fringilla.",
 #  is_id = T
 #)$
 #step( 
 #  "rank",
 #  title = "Show rank instead of value",
 #  description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut facilisis id purus vel eleifend. Ut vitae tellus in lectus fringilla.",
 #  is_id = T
 #)$
 #step( 
 #  "[data-id='threshold']",
 #  title = "Benchmarking Thresholds",
 #  description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut facilisis id purus vel eleifend. Ut vitae tellus in lectus fringilla.",
 #  is_id = F
 #)$
 