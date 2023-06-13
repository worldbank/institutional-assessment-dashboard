def_quantiles <- function(data, base_country, country_list, comparison_countries, vars, variable_names,threshold) {

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

if (threshold=="default"){
    cutoff<-c(25,50)
}else if (threshold=="terciles")
{
  cutoff<-c(33,66)
}
  
# Calculate quantiles
  quantiles <-
    quantiles %>%
    # Remove missing values
    filter(!is.na(value)) %>%
    # Calculate relevant indicators
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(value),
      q25 = quantile(value, c(cutoff[1]/100)),
      q50 = quantile(value, c(cutoff[2]/100)),
      status = case_when(
        dtt <= cutoff[1]/100 ~ paste0("Weak\n(bottom ", cutoff[1],"%)"),
        dtt > cutoff[1]/100 & dtt <= cutoff[2]/100 ~ paste0("Emerging\n(",cutoff[1],"% - ",cutoff[2],"%)"),
        dtt > cutoff[2]/100 ~ paste0("Strong\n(top ",cutoff[2],"%)")
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
