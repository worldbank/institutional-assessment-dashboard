
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

# 03- Compute coverage diagnostics: b. External coverage
# Make a global country-indicator coverage report
#Step 1. use calculate_time_range and create: global_available_years and global_coverage (%)
#Step 2. feed the compute_global_coverage function

# First, perform all the necessary inpits for the step 2 function
calculate_global_coverage <- function(value, id) {
  global_coverage_id <- distinct({{id}}[!is.na({{value}})])
  return(global_coverage_id)
}


coverage_range_global <- function(value, time_id) {
  global_year_range <- paste0(
    min({{time_id}}[!is.na({{value}})], na.rm = TRUE), "-",
    max({{time_id}}[!is.na({{value}})], na.rm = TRUE)
  )

  if(str_detect(global_year_range, "Inf")){
    # if there is no available years, the global_year_range becomes
    # -Inf-Inf. we overwrite that as "Not available".
    global_year_range <- "Not available"
  }

  return(global_year_range)
}


coverage_years_global <- function(value, time_id) {
  # Remove NAs from both the value and time_id
  available_years <- time_id[!is.na(value)]

  # Check if there are available years
  if (length(available_years) == 0) {
    return("Not available")
  }

  # Create a comma-separated string of the available years
  global_year_coverage <- paste(available_years, collapse = ", ")

  return(global_year_coverage)
}


coverage_share_global <- function(value, time_id) {
  # Get the non-NA time_ids corresponding to non-NA values
  available_years <- unique(time_id[!is.na(value)])

  # If there are no available years, return NA
  if (length(available_years) == 0) {
    return(NA_real_)  # Return NA instead of "Not available"
  }

  # Calculate the share: number of available years / total years
  total_years <- length(unique(time_id))  # Correct way to count total number of years
  available_years_count <- length(available_years)  # Number of available years

  # Calculate the share as a percentage
  share <- (available_years_count / total_years) * 100

  # Return the share as a numeric vector (as percentage with rounding)
  return(round(share, 2))  # Return as a numeric value, not a tibble
}



# x <- c(0, NA, 2, 4)
# year <- c(2001, 2002, 2004, 2008)
#
# # the truth is 75%
# coverage_share_global(x, year)

# Second, use the above inputs to create the desired table
compute_global_coverage <- function(data, country_id, indicator_id, time_id, value_column) {
  global_data_coverage <- data |>
    group_by({{country_id}}, {{indicator_id}}) |>
    summarise(
      year_range = coverage_range_global({{value_column}}, {{time_id}}),
      available_years = coverage_years_global({{value_column}}, {{time_id}}),
      available_share = coverage_share_global({{value_column}}, {{time_id}}),
      .groups = 'drop'
    )
  return(global_data_coverage)
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
      # we only compute family averages if all indicators are available
      # there na.rm = FALSE
      value = mean(value, na.rm = FALSE),
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

compute_family_variance <- function(cliar_data, vars, type = "static", db_variables){
  # this function generates family-level variances
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
      # we only compute statistics if all indicators are available
      # there na.rm = FALSE
      avg = mean(value, na.rm = FALSE),
      var = var(value, na.rm = FALSE),
      min = min(value, na.rm = FALSE),
      max = max(value, na.rm = FALSE),
      .groups = "drop"
    )

  cliar_family_level <- cliar_family_level_long |>
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = family_var,
      names_glue = "{family_var}_{.value}",
      values_from = c(avg, var, min, max)
    )

  return(cliar_family_level)
}



