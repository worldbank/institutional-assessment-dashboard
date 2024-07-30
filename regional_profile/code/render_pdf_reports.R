packages <- c("tidyverse","here","knitr","sf","ggtext","showtext","geomtextpath","patchwork")
pacman::p_load(packages,character.only = TRUE)

render_subject <- function(file_region_name) {
  rmarkdown::render(
    input = here("regional_profile/regional_profile.Rmd"),
    params = list(region = file_region_name),
    output_file = paste0(janitor::make_clean_names(file_region_name), '.pdf'),
    output_dir = here("regional_profile/pdf"),
    intermediates_dir = here("regional_profile/pdf"),
    clean = T,
    quiet = F
  )
}

country_groups <-
  read_rds(
    here(
      "app",
      "data",
      "wb_country_groups.rds"
    )
  )

group_list <-
  list(
    `Economic` = country_groups %>% filter(group_category == "Economic") %>% pull(group_name),
    `Region` = country_groups %>% filter(group_category == "Region") %>% filter(group_name != "Europe & Central Asia (Excluding Western Europe)") %>% pull(group_name) ,
    `Income` = country_groups %>% filter(group_category == "Income") %>% pull(group_name)
  )

#for(region in group_list$Region){
#  cat("Rendering", region,"report...\n")
#  render_subject(region)
#}

for(region in "Latin America & Caribbean"){
  cat("Rendering", region,"report...\n")
  render_subject(region)
}

for(region in c(
 "Latin America & Caribbean",
 "Europe & Central Asia",
 "Middle East & North Africa",
 "South Asia",
 "East Asia & Pacific",
 "Africa Eastern and Southern",
 "Africa Western and Central"
)){
  cat("Rendering", region,"report...\n")
  render_subject(region)
}








