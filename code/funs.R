
calculate_coverage <- function(indicator, id) {
  coverage_id <- n_distinct({{id}}[!is.na(indicator)])

  return(coverage_id)
}

flag_continued <- function(indicator, year_id, ref_year){
  # this function returns a flag for discontinued series
  # 1. compute the number of times the indicator is measured
  # since a reference year - 5 = last five years
  times_updated <- length(indicator[{{year_id}} >= ref_year - 5 & !is.na(indicator)])

  flag_continued <- if_else(times_updated > 0, 1, 0)

  return(flag_continued)
}

flag_country <- function(indicator, country_id, year_id, ref_year, country_region_list){
  # this function returns a flag for the country coverage
  # 1. compute the number of distinct country ids for indicators
  # if they are not missing and more recent than a reference year
  country_coverage <- n_distinct({{country_id}}[{{year_id}} >= ref_year - 5 & !is.na(indicator)])
  country_code_unique <- unique(
    {{country_id}}[{{year_id}} >= ref_year - 5 & !is.na(indicator)]
  )

  regions_covered <- country_region_list |>
      filter(
        country_code %in% country_code_unique
      ) |>
      distinct(region) |>
      nrow()

  flag_country <- if_else(
    country_coverage >= 100 | (country_coverage >= 50 & regions_covered == 7),
    1, 0
  )

  return(flag_country)
}

flag_minimum_coverage <- function(indicator, country_id, year_id){
  # this function returns a flag for countries with less than minimum coverage:
  # defined as less than two years with at least 10 countries covered
  # 1. create a table with all relevant variables
  country_coverage <- tibble(
    indicator = indicator,
    country = country_id,
    year = year_id
  )

  # 2. calculate by year the number of distinct countries
  # and only maintain years where at least 10 countries are covered
  minimum_country_coverage <- country_coverage |>
    filter(!is.na(indicator)) |>
    group_by(year) |>
    summarise(
      country_coverage = n_distinct(country)
    ) |>
    filter(
      country_coverage >= 10
    )

  # return a flag 1 if more than two years (nrows) are available for that indicator
  flag_minimum_coverage <- if_else(nrow(minimum_country_coverage) >= 2, 1, 0)
}

calculate_time_range <- function(indicator, time_id){
  year_range <- paste0(
    min({{time_id}}[!is.na(indicator)], na.rm = TRUE), "-", max({{time_id}}[!is.na(indicator)], na.rm = TRUE)
  )

  return(year_range)
}

compute_coverage <- function(data, country_id, year_id, ref_year){
  data_coverage <- data |>
    # compute (1) number of distinct country codes
    # (2) range of years covered
   summarise(
      across(
        c(
          everything(),
          -{{country_id}},
          -{{year_id}}
          ),
          list(
            country_coverage = ~ calculate_coverage(.x, {{country_id}}),
            year_coverage = ~ calculate_coverage(.x, {{year_id}}),
            flag_continued = ~ flag_continued(.x, {{year_id}}, ref_year),
            flag_country = ~ flag_country(.x, {{country_id}}, {{year_id}}, ref_year, country_region_list),
            flag_minimum_coverage = ~ flag_minimum_coverage(.x, {{country_id}}, {{year_id}}),
            year_range = ~ calculate_time_range(.x, {{year_id}}),
            percent_complete_records = ~ percent(prop_complete(.x)),
            percent_complete_records_last_five = ~ percent(prop_complete(.x[{{year_id}} >= ref_year])),
            mean = ~ mean(.x, na.rm = TRUE) |> round(2),
            median = ~ median(.x, na.rm = TRUE),
            standard_deviation = ~ sd(.x, na.rm = TRUE) |> round(2),
            min = ~ min(.x, na.rm = TRUE),
            max = ~ max(.x, na.rm = TRUE)
          ),
          .names = "{.col}__{.fn}"
      )
  ) |>
  pivot_longer(
      cols = c(everything()),
      cols_vary = "slowest",
      names_to = c("indicator", ".value"),
      names_pattern = "(.*)__(.*)"
  ) |>
    arrange(
      indicator
    ) |>
    select(
      Indicator = indicator,
      `Country Coverage` = country_coverage,
      `Year Coverage` = year_coverage,
      `Flag Continuity` = flag_continued,
      `Flag Country Coverage` = flag_country,
      `Flag Year Coverage` = flag_minimum_coverage,
      `Year Range` = year_range,
      `Percentage of Complete Records` = percent_complete_records,
      `Percentage of Complete Records in Last Five Years` = percent_complete_records_last_five,
      `Mean` = mean,
      `Median` = median,
      `Standard Deviation` = standard_deviation,
      `Minimum` = min,
      `Maximum` = max
    )

  return(data_coverage)
}

scale_values <- function(x){
  (x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
}

compute_family_average <- function(cliar_data, vars, type = "static", db_variables){
  # this function generates family averages
  # taking a simple average by grouping
  # default is static
  cliar_data_long <-
    cliar_data %>%
    pivot_longer(
      all_of({{vars}}),
      names_to = "variable"
    ) %>%
    select(-contains("gdp")) %>%
    left_join(
      db_variables %>%
        select(variable, var_name, family_name, family_var),
      by = "variable"
    )

  # only calculate family averages for relevant institutional clusters
  if(type == "static"){
    grouping <- c("country_code", "family_var")
    id_cols <- c("country_code")
  } else{
    grouping = c("country_code", "year", "family_var")
    id_cols <- c("country_code", "year")
  }

  cliar_family_level_long <- cliar_data_long |>
    group_by(
      across(all_of(grouping))
    ) |>
    summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )

  cliar_family_level <- cliar_family_level_long |>
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = family_var,
      names_glue = "{family_var}_avg",
      values_from = value
    )

  return(cliar_family_level)
}
