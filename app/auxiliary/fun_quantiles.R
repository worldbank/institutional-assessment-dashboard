def_quantiles <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {

# List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)

# List all variables that are missing for the base country -- these will be removed from the data
  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country) %>%
    select(where(is.na)) %>%
    names

# List final relevant variables: those selected, minus those missing
  variables <-
    setdiff(vars, na_indicators)

  variables <-
    intersect(variables, names(data))

# This is the relevant data to be used
  quantiles <-
    data %>%
    ungroup() %>%
    filter(
      country_name %in% c(base_country, comparison_list$country_name)
    ) %>%
    select(
      country_name,
      all_of(variables)
    )

# Merge with variable dictionary
  quantiles <-
    quantiles %>%

    # Make long per indicator
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable"
    ) %>%

    # Add variables definition and family
    left_join(
      variable_names,
      by = "variable"
    )

# Calculate quantiles
  quantiles <-
    quantiles %>%

    # Remove missing values
    filter(!is.na(value)) %>%

    # Calculate relevant indicators
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(value),
      q25 = quantile(value, c(0.25)),
      q50 = quantile(value, c(0.5)),
      status = case_when(
        dtt <= .25 ~ "Weak\n(bottom 25%)",
        dtt > .25 & dtt <= .50 ~ "Emerging\n(25% - 50%)",
        dtt > .50 ~ "Strong\n(top 50%)"
      ),
      nrank = rank(-value)
    ) %>%
    ungroup %>%
    rename(dtf = value)

  # Remove indicators where there is too little variance
  low_variance_indicators <-
    quantiles %>%
    filter(country_name == base_country & q25==q50) %>%
    select(variable) %>%
    unlist

  quantiles <-
    quantiles %>%
    filter(!(variable %in% low_variance_indicators))

}
