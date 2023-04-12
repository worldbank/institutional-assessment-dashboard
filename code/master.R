library(knitr)

# - Input: `data/db_variables.xlsx`
# - Outputs:
#   - `data/final/db_variables.xlsx`
#   - `data/final/definitions.rds`
knit("select-indicators.Rmd")

# - Input: `data/raw/merged_for_residuals-v2.rds`
# - Output: `data/clean/original_data.rds`
knit("process-original-data.Rmd")

# - Input: `data/final/db_variables.rds`
# - Output: `data/clean/original_data.rds`
knit("process-api-data.Rmd")

# - Input: `data/raw/20211118_new_additions_notGov360.dta`
# - Output: `data/clean/additional_data.rds`
knit("process-additional-data.Rmd")

# - Inputs:
#   - `data/clean/original_data.rds`
#   - `data/clean/additional_data.rds`
#   - `data/clean/api_data.rds`
#
# - Output: `data/final/compiled_indicators.rds`
knit("combine-data-sources.Rmd")

# - Inputs:
#   - `data/final/compiled_indicators.rds`
#   - `data/raw/CLASS.xlsx`, obtained from https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups on September 1, 2022
#   - `data/raw/group_list.csv`, input by the research team to list relevant groups
#
# - Outputs:
#   - `data/final/wb_country_list.rds`
#   - `data/final/wb_country_groups.rds`
knit("clean-countries.Rmd")

# - Inputs:
#   - `data/final/definitions.rds`
#   - `data/final/compiled_indicators.rds`
#   - `data/raw/wb_country_list.rds`
#
# - Outputs:
#   - `data/final/closeness_to_frontier.rds`
#   - `data/final/closeness_to_frontier_long.rds`
knit("ctf.Rmd")

# - Inputs:
#   - `data/final/closeness_to_frontier.rds`
#   - `data/final/compiled_indicators.rds`
#   - `data/raw/WB_countries_Admin0_lowres.geojson`, obtained from https://datacatalog.worldbank.org/int/search/dataset/0038272 on September 1, 2022
#   - `data/raw/WB_disputed_areas_Admin0_10m.geojson`, obtained from https://datacatalog.worldbank.org/int/search/dataset/0038272 on September 1, 2022
#
# - Output: `data/final/indicators_map.rds`
knit("map.Rmd")

# Move final data to app folder
knit("copy-final-data.Rmd")

