  
  library(tidyverse)
  
  
  country_list <-
    read_csv(file.path("data",
                       "wb_country_list.csv"))
  
  countries <-
    country_list %>%
    select(country, 
           country_code) %>%
    unique

  groups <- 
    c("EUU",
      "HIC",
      "LIC",
      "LMC",
      "LMY",
      "MIC",
      "OED",
      "NAC",
      "UMC")
  
    
  countries <-
    mutate
  
  code <- "ARB"
  country <- c("Brazil", "Argentina", "Oman")
  
  group_vars <-
    function(country, code) {
      
      group_contries <-
        country_groups %>%
        filter(group_code == code) %>%
        select(country) 
      
      country %in% group_contries
      
    }

  