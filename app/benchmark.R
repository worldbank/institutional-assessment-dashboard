library(tidyverse)
library(here)

# Years --------------------------------------------------------------

all_years <- c("", "2014_2016", "2017_2020")

# Variables ----------------------------------------------------------

source("global.R")
source("auxiliary/static_plot2.R")
source("auxiliary/vars-by-family.R")

# Countries ----------------------------------------------------------

base_country <- "Jordan"

comparison_group <- c(
  "Morocco", "Egypt", 
  "Tunisia", "Pakistan",
  "Turkey", "Philippines",
  "Indonesia", "Georgia",
  "Finland", "Ireland", 
  "Croatia", "Bulgaria", 
  "Uruguay"
)

# Map globals --------------------------------------------------------

map(all_years, function(x){
  if (x == "") {
    yy = x
    plot_year = ""
    plot_note = "2014-2020."
  } else if (x == "2014_2016"){
    y = x
    plot_year = "_2014_2016"
    plot_note = "2014-2016."
    
  } else if (x == "2017_2020"){
    y = x
    plot_year = "_2017_2020"
    plot_note = "2017-2020."
  }
  
  ## Read Data ====
  global_data <- read_rds(
    here(
      "data",
      paste0("closeness_to_frontier", plot_year, ".rds")
    )
  ) %>%
    ungroup()
  

  # Overview Plot ------------------------------------------------------

  # Families and countries
  families <- variable_names %>% distinct(family_name)

  # Overview Plot
  data_family <- family_data(
    global_data,
    base_country,
    variable_names
  ) %>%
    def_quantiles(
      base_country,
      country_list,
      comparison_group,
      family_names,
      variable_names
    ) %>% 
    filter(family_name != "Labor market")
  
  data_family  %>%
    static_plot(
      base_country,
      "Overview",
      overview = TRUE,
      note = paste0("Indicators standardized using the min and max in the full sample over ", plot_note),
      title = FALSE
    ) +
    scale_x_continuous(limits = c(0,1))
  
  ggsave(paste0("figs/overview", plot_year, ".png"), bg = "white",  width = 12, height = 8, scale = 0.85)
  
})


# By Family ----------------------------------------------------------

source("auxiliary/vars-by-family.R")

families <- variable_names %>% 
  distinct(family_name) %>% 
  .$`family_name`

fam_vars <- list(
  "Anti-Corruption, Transparency and Accountability" = vars_transp,
  "Political" = vars_pol,
  "Justice" = vars_leg,
  # "Labor market" = vars_lab,
  "Financial market" = vars_fin,
  "Business environment and trade" = vars_mkt,
  "Public sector performance" = vars_publ,
  "Social" = vars_social
)

map(all_years, function(x){
  if (x == "") {
    yy = x
    plot_year = ""
    plot_note = "2014-2020."
  } else if (x == "2014_2016"){
    y = x
    plot_year = "_2014_2016"
    plot_note = "2014-2016."
    
  } else if (x == "2017_2020"){
    y = x
    plot_year = "_2017_2020"
    plot_note = "2017-2020."
  }
  
  ## Read Data ====
  global_data <- read_rds(
    here(
      "data",
      paste0("closeness_to_frontier", plot_year, ".rds")
    )
  ) %>% ungroup()
  
  make_fam_plot <- function(fam) {
    pp <- global_data %>%
      def_quantiles(
        base_country,
        country_list,
        comparison_group,
        fam_vars[[fam]],
        variable_names
      ) %>%
      static_plot2(
        base_country,
        fam_vars[[fam]],
        note = paste0("Indicators standardized using the min and max in the full sample over ", plot_note)
      ) +
      scale_x_continuous(limits = c(0,1))
  
    return(pp)
  }
  
  map(1:length(fam_vars), ~ {
    name <- names(fam_vars)[.x] %>% str_to_lower() %>% str_replace_all(., " ", "_")
    name <- paste0("figs/", name, plot_year, ".png")
    make_fam_plot(.x)
    ggsave(filename = name, bg = "white", device = png, width = 12, height = 8, scale = 0.85)
  })

})
