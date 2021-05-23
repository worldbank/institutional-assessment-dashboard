# LOAD PACKAGES ----
packages <- c("tidyverse",
              "here",
              "stringdist",
              "data360r")

# LOAD INDICATORS MATCH EXACT ----
match_indicators <- read_rds(here("code",
               "data360r",
               "match_indicators.rds"))

match_indicators <-
  match_indicators %>%
  filter(!duplicated(id))

# GET DATA FROM INDICATOR ID ----
data_api <- get_data360(
  indicator_id = match_indicators$id,
  output_type = 'long')

# PREPARE DATA TO CLEANING AND CTF CALCULATION ----
data_api <-
  data_api %>%
  filter(
    nchar(as.character(Period))<=4
  ) %>%
  mutate(
    Period = as.numeric(levels(Period))[Period]
  ) %>%
  filter(
    #Indicator == "Irregular payments and bribes" |
    #Indicator == "Public trust in politicians" |
    #Indicator == "Transparency of government policymaking" |
    Indicator == "Other barriers to trade and investment"
  ) %>%
  mutate(
    var = case_when(
      Indicator == "Other barriers to trade and investment" ~ "barriers_trade_oth"#,
      #Indicator == "Irregular payments and bribes" ~ "bribes",
      #Indicator == "Public trust in politicians" ~ "trust_pol",
      #Indicator == "Transparency of government policymaking" ~ "transparency_polmak",
    )
  ) %>%
  select(
    country_name = `Country Name`,
    country_code = `Country ISO3`,
    year = Period,
    var,
    value = Observation
  ) %>%
  pivot_wider(
    id_cols = c(country_name,country_code,year),
    names_from = var
  ) %>%
  arrange(
    country_code,country_name,year
  )

# 01-import-clean ----

# Fix vars with opposite scale
# reason: for the CTF methodology, we need for all indicators that "higher values" means "better performance"
# freedom house: Countries are graded between 1 (most free) and 7 (least free).
data_selected <-
  data_api %>%
  #mutate(
  #  across(
  #    c(e_fh_pr,e_fh_cl),
  #    ~(8 - .x)
  #  )
  #) %>%
  # transform missing
  #mutate(
  #  e_p_polity = ifelse(e_p_polity < -10, NA, e_p_polity)
  #) %>%
  # PRM indicators: Countries are graded between 0 (less control/involvement) and 6 (more control/involvement)
  mutate(
    across(
      c(barriers_trade_oth), # c(governance_soe,price_controls,command_control,complexity_procedures,barriers_startups,protection_incumbents,barriers_trade_expl,barriers_trade_oth),
      ~(6 - .x)
    )
  )

# 02-indicator-calc ----

# Get min and max for each variable
vars_minmax <-
  data_selected %>%
  summarise(
    across(
      c(barriers_trade_oth), # all_of(vars_all) replace later
      list(
        min = ~ min(.x[year >= 2013], na.rm = T), # year >= as.numeric(format(Sys.Date(), "%Y"))-8): Change eventually to filter for the last 7 years and disable next line
        max = ~ max(.x[year >= 2013], na.rm = T)  # year >= as.numeric(format(Sys.Date(), "%Y"))-8): Change eventually to filter for the last 7 years and disable next line
      ),
      .names="{.col}-{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to="variable",
    values_to = "value_minmax",
    values_drop_na = F
  ) %>%
  separate(
    variable,
    into = c("variable", "minmax"),
    sep = "-"
  ) %>%
  pivot_wider(
    id_cols = "variable",
    names_from = "minmax",
    values_from = "value_minmax"
  ) %>%
  remove_labels()

# Collapse at country level. for each country, keep only the average since 2013
# SC: in the long term, this step should be flexibly adjusted in the dashboard (keep last 7 years, given the present time)
data_country <-
  data_selected %>%
  group_by(country_name,
           country_code#,
           #lac,lac6,oecd,structural
           ) %>%
  summarise(
    across(
      c(barriers_trade_oth), # all_of(vars_all) replace later
      ~mean(.x[year>=2013], na.rm = T)  # year >= as.numeric(format(Sys.Date(), "%Y"))-8): Change eventually to filter for the last 7 years and disable next line
    )
  ) %>%
  mutate(
    across(
      c(barriers_trade_oth), # all_of(vars_all) replace later
      ~ifelse(is.nan(.x),NA,.x)
    )
  )

# Calculate closeness to frontier at indicator level
dtf_vars_global <-
  data_country %>%
  remove_labels %>%
  pivot_longer(
    c(barriers_trade_oth), # all_of(vars_all) replace later
    names_to = "variable",
    values_drop_na = F
  ) %>%
  left_join(vars_minmax,
            by = "variable") %>%
  mutate(
    dtf = (min - value) / (min - max),
    dtf = ifelse(dtf == 0,
                 0.01,
                 dtf) # small adjustments to display a very short bar on the graph, in case dtf = 0
  ) %>%
  pivot_wider(
    id_cols = c("country_name", "country_code"#,
                #"lac", "lac6", "oecd", "structural"
                ),
    names_from = "variable",
    values_from = "dtf"
  )

# Calculate closeness to frontier at institutional family level (mean of DTF of each indicator)
#dtf_family_level <-
#  dtf_vars_global %>%
#  ungroup() %>%
#  select(country_code,country_name)
#
#i=1
#
#vars_global_list=list(vars_pol = vars_pol,
#                      vars_social = vars_social,
#                      vars_transp = vars_transp,
#                      vars_publ = vars_publ,
#                      vars_leg = vars_leg,
#                      vars_mkt = vars_mkt,
#                      vars_lab = vars_lab,
#                      vars_fin = vars_fin,
#                      vars_service_del = vars_service_del)
#
#for(group in vars_global_list){
#
#  name_var <- names(vars_global_list[i])
#
#  dtf_family <- dtf_vars_global %>%
#    ungroup() %>%
#    select(
#      country_code,
#      dplyr::contains(group)
#    ) %>%
#    pivot_longer(dplyr::contains(group)) %>%
#    select(-name) %>%
#    group_by(country_code) %>%
#    summarise(
#      "{name_var}" := mean(value,na.rm=T)
#    )
#
#  i=i+1
#
#  dtf_family_level <- left_join(dtf_family_level,
#                                dtf_family,
#                                by="country_code")
#
#}
#
#rm(dtf_family, i, group, vars_minmax, data_country, data_selected, packages,name_var,vars_global_list)
#
#dtf_family_level <- dtf_family_level %>%
#  mutate(
#    across(
#      all_of(family_names),
#      ~ifelse(
#        is.nan(.x),
#        NA,
#        .x
#      )
#    )
#  ) %>%
#  mutate(
#    across(
#      all_of(family_names),
#      ~ifelse(.x == 0,
#              0.01,
#              .x) # small adjustments to display a very short bar on the graph, in case dtf = 0
#    )
#  )
