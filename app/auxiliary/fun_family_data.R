# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCTION TO CREATE FAMILY LEVEL DATA BASED ON COUNTRIES -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

family_data <- function(data, base_country, comparison_countries) {

  miss_indicator_base <- data %>%
    ungroup() %>%
    filter(country_name == base_country) %>%
    select(where(is.na)) %>%
    pivot_longer(cols = 1:ncol(.), names_to = "missing_var")

  dtf_family_level <-
    data %>%
    ungroup() %>%
    select(country_code,country_name)

  i=1

  vars_global_list=list(
    vars_pol = vars_pol,
    vars_social = vars_social,
    vars_transp = vars_transp,
    vars_publ = vars_publ,
    vars_leg = vars_leg,
    vars_mkt = vars_mkt,
    vars_lab = vars_lab,
    vars_fin = vars_fin,
    vars_service_del = vars_service_del
  )

  for(group in vars_global_list){

    name_var <- names(vars_global_list[i])

    dtf_family <- dtf_vars_global %>%
      ungroup() %>%
      select(
        country_code,
        dplyr::contains(group)
      ) %>%
      pivot_longer(dplyr::contains(group)) %>%
      filter(!name %in% miss_indicator_base$missing_var) %>%
      select(-name) %>%
      group_by(country_code) %>%
      summarise(
        "{name_var}" := mean(value,na.rm=T)
      )

    i=i+1

    dtf_family_level <- left_join(dtf_family_level,
                                  dtf_family,
                                  by="country_code")

  }

  rm(dtf_family, i, group, vars_minmax, data_country, data_selected, packages,name_var,vars_global_list)

  dtf_family_level <- dtf_family_level %>%
    mutate(
      across(
        all_of(family_names),
        ~ifelse(
          is.nan(.x),
          NA,
          .x
        )
      )
    ) %>%
    mutate(
      across(
        all_of(family_names),
        ~ifelse(.x == 0,
                0.01,
                .x) # small adjustments to display a very short bar on the graph, in case dtf = 0
      )
    )

}
