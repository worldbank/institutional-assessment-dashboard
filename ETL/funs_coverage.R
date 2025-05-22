group_segment_limits <- function(data, group, subgroup, quantity) {
  data |>
    group_by({{group}},{{subgroup}}) |>
    mutate(
      group_av = mean({{quantity}}, na.rm = TRUE),
      max_av = max({{quantity}}, na.rm = TRUE),
      min_av = min({{quantity}}, na.rm = TRUE)
    ) |>
    ungroup() |>
    pivot_longer(
      cols = c(min_av, group_av, max_av),
      names_to = "type",
      values_to = "value"
    ) |>
    mutate(
      type = recode(type,
                    min_av = "Min",
                    group_av = "Average",  # Recode group_av to "Average"
                    max_av = "Max"),
      group_min = min(value, na.rm = TRUE),
      group_max = max(value, na.rm = TRUE)
    ) |>
    group_by({{group}},{{subgroup}}) |>
    summarise(
      # Assign the same min and max values for each group and type
      group_min = min(value, na.rm = TRUE),
      group_max = max(value, na.rm = TRUE)
    ) |>
    ungroup()
}

group_average <- function(data, group, subgroup, quantity) {
  data |>
    group_by({{group}},{{subgroup}}) |>
    summarise(
      group_av = mean({{quantity}}, na.rm = TRUE)
    ) |>
    group_by({{group}})|>
    mutate(
      group_min = min(group_av, na.rm = TRUE),
      group_max = max(group_av, na.rm = TRUE)
    )
}


group_facet_average <- function(data, facet, group, subgroup, quantity) {
  data |>
    group_by({{facet}},{{group}},{{subgroup}}) |>
    summarise(
      group_av = mean({{quantity}}, na.rm = TRUE)
    ) |>
    group_by({{facet}},{{group}})|>
    mutate(
      group_min = min(group_av, na.rm = TRUE),
      group_max = max(group_av, na.rm = TRUE)
    )
}
