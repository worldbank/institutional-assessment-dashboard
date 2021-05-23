# LOAD PACKAGES ----
packages <- c("tidyverse",
              "here",
              "stringdist",
              "data360r")

pacman::p_load(packages,
               character.only = TRUE)

# LOAD INDICATORS FROM DASHBOARD ----
all_indicators <-
  read_rds(here("app",
                "data",
                "list_of_indicators.rds"))

# LOAD ALL INDICATORS FROM 360 DATABASES ----
gov_indicators <- get_metadata360(site="gov", metadata_type = "indicators")
tc_indicators <- get_metadata360(site="tc", metadata_type = "indicators")

# SIMILARITY ANALYSIS BY INDICATOR NAME AND SOURCE NAME ----
match_indicators <- data.frame()

## Over TC360 ----
for(i in 1:nrow(all_indicators)){

  print(i)

  ind <- all_indicators$Indicator[i]

  dist <- data.frame(name_dist=c(stringdist(tc_indicators$name,ind)))

  joined <-
    bind_cols(
      tc_indicators %>%
        mutate(DB="TC360") %>%
        select(DB,id,name,definition,dataset,datasetId,valueType,units,dateRange),
      dist
    ) %>%
    arrange(name_dist) %>%
    filter(name_dist <= 10) %>%
    mutate(indicator = ind) %>%
    rename_all( list(~paste0(., "_360"))) %>%
    left_join(all_indicators %>%
                filter(Indicator==ind) %>%
                rename_all( list(~paste0(., "_dash"))),
              by=c("indicator_360"="Indicator_dash")) %>%
    rename(
      name_dist=name_dist_360,
      id=id_360,
      Indicator_dash=indicator_360
    ) %>%
    select(-`Institutional family_dash`) %>%
    relocate(
      all_of(c("Indicator_dash","Description_dash","Source_dash","name_dist")),
      .before = DB_360
    )

  match_indicators <- bind_rows(match_indicators,joined)

}

## Over GOV360 ----
for(i in 1:nrow(all_indicators)){

  print(i)

  ind <- all_indicators$Indicator[i]

  dist <- data.frame(name_dist=c(stringdist(gov_indicators$name,ind)))

  joined <-
    bind_cols(
      gov_indicators %>%
        mutate(DB="GOV360") %>%
        select(DB,id,name,definition,dataset,datasetId,valueType,units,dateRange),
      dist
    ) %>%
    arrange(name_dist) %>%
    filter(name_dist <= 10) %>%
    mutate(indicator = ind) %>%
    rename_all( list(~paste0(., "_360"))) %>%
    left_join(all_indicators %>%
                filter(Indicator==ind) %>%
                rename_all( list(~paste0(., "_dash"))),
              by=c("indicator_360"="Indicator_dash")) %>%
    rename(
      name_dist=name_dist_360,
      id=id_360,
      Indicator_dash=indicator_360
    ) %>%
    select(-`Institutional family_dash`) %>%
    relocate(
      all_of(c("Indicator_dash","Description_dash","Source_dash","name_dist")),
      .before = DB_360
    )

  match_indicators <- bind_rows(match_indicators,joined)

}

rm(i,ind,joined,dist)

match_indicators <-
  match_indicators %>%
  arrange(Indicator_dash,name_dist) %>%
  mutate(
    across(
      c(Source_dash,dataset_360),
      ~toupper(.x)
    )
  ) %>%
  mutate(
    source_dist = stringdist(Source_dash,dataset_360)
  ) %>%
  relocate(
    source_dist,
    .after = name_dist
  ) %>%
  filter(
    units_360=="Index"
  )

# SAVES INDICATORS WITH EXACT MATCH ----
write_rds(match_indicators %>% filter(name_dist==0),
          here("code",
               "data360r",
               "match_indicators.rds"))
