source("global.R")

base_country = "Mozambique"
tab_name = "Social"
rank = FALSE
group_median = c("Sub-Saharan Africa", "Arab World")
custom_df = NULL
title = TRUE
dots = TRUE
note = NULL
threshold = "default"
vars_all = vars_all
vars = variable_names %>%
  filter(family_name == tab_name) %>%
  pull(variable) %>%
  unique()
countries = country_list %>% 
               filter(group == "Sub-Saharan Africa" & country_name != base_country) %>% 
               distinct(country_name) %>% 
               pull()


               
  
data =  global_data_dyn %>%
  def_quantiles_dyn(
    base_country,
    country_list,
    countries,
    vars_all,
    variable_names,
    threshold) %>% 
  filter(variable %in% vars) 

spec_varname =  unique(data$var_name)[2:5]
data <- data %>% filter(var_name %in% spec_varname)


if (threshold=="default"){
  cutoff<-c(25,50)
}else if (threshold=="terciles")
{
  cutoff<-c(33,66)
}


data$var_name <-
  factor(
    data$var_name,
    levels = sort(unique(data$var_name),
      decreasing = TRUE),
    ordered = TRUE
  )

data <- data %>% 
  rowwise() %>% 
  mutate(var_name2 = paste(var_name, year, sep = " : ")) %>% 
  arrange(var_name2) 

base_country_vars <-  data %>% 
  filter(country_name == base_country ) %>% 
  distinct(var_name2) %>% 
  pull()

data <- data %>% 
  filter(var_name2 %in% base_country_vars)

ctf_long_dyn <- ctf_long_dyn %>% 
  rowwise() %>% 
  mutate(var_name2 = paste(var_name, year, sep = " : ")) %>% 
  arrange(var_name2) %>% 
  filter(var_name2 %in% base_country_vars)

vars <-
  data %>%
  select(var_name) %>%
  unique %>%
  unlist %>%
  unname

if (cutoff[[1]]==25){
  colors <-
    c("Weak\n(bottom 25%)" = "#D2222D",
      "Emerging\n(25% - 50%)" = "#FFBF00",
      "Strong\n(top 50%)" = "#238823"
    )}else if (cutoff[[1]]==33){
      colors <-c(
        "Weak\n(bottom 33%)" = "#D2222D",
        "Emerging\n(33% - 66%)" = "#FFBF00",
        "Strong\n(top 66%)" = "#238823"
      )
    }

if (rank == FALSE) {
  y_lab <- "Closeness to frontier"
  
  data <-
    data %>%
    mutate(
      var = dtf,
      text = paste(
        " Country:", country_name, "<br>",
        "Closeness to frontier:", round(dtf, 3)
      )
    )
  
} else {
  data <-
    data %>%
    mutate(
      q25 = cutoff[[1]]/100,
      q50 = cutoff[[2]]/100,
      var = dtt,
      text = paste(
        " Country:", country_name, "<br>",
        "Year: ", year, 
        "Closeness to frontier:", round(dtf, 3), "<br>",
        "Rank:", nrank
      )
    )
  
  y_lab <- "Rank"
}


## Percentile segments
plot <-
  ggplot() +
  geom_segment(
    data = data,
    aes(
      x = year,
      xend = year,
      y = 0,
      yend = q25
    ),
    color = "#e47a81",
    size = 2,
    alpha = .1
  ) +
  geom_segment(
    data = data,
    aes(
      x = year,
      xend = year,
      y = q25,
      yend = q50
    ),
    color = "#ffd966",
    size = 2,
    alpha = .3
  ) +
  geom_segment(
    data = data,
    aes(
      x = year,
      xend = year,
      y = q50,
      yend = 1
    ),
    color = "#8ec18e",
    size = 2,
    alpha = .3
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    legend.box = "vertical",
    plot.caption = element_text(size = 8, hjust = 0),
    plot.caption.position =  "plot"
  ) +
  labs(
    y = y_lab,
    x = NULL,
    fill = NULL,
    shape = NULL,
    caption = note
  ) +
  scale_fill_manual(
    values = colors
  )
    
## adding the base country point and line graph
plot <-
  plot +
    geom_point(
    data = data %>% filter(country_name == base_country),
    aes(
      y = var,
      x = year,
      fill = status ,
      text = text
    ),
    size = 3,
    shape = 21,
    color = "gray0"
  ) +
  geom_line(
    data = data %>% filter(country_name == base_country),
    aes(
      y = var,
      x = year,
      group = 1
    )
  )
 

## add group medians
if (!is.null(group_median) & !rank) {
  
  median_data <-
    ctf_long_dyn %>%
    filter(
      var_name %in% spec_varname,
      country_name %in% group_median
    ) %>%
    select(
      var_name,
      year, 
      value,
      country_name
    )
  
  ## If custom groups exists, ------------------------------------------------------------------------------------
  
  if(!is.null(custom_df)){
    
    ## If any of the benchmark medians is a custom group
    if(any(group_median %in% custom_df$Grp)){
      
      ## create a place holder that will hold the medians for all the groups
      # custom_grp_median_data <- list()
      # 
      ## create a vector of these groups
      selected_custom_grps <- unique(custom_df$Grp)
      
      custom_grp_median_data_func <- function(selected_custom_grp){
        custom_df_per_group <- custom_df %>% 
          filter(Grp == selected_custom_grp)
        
        
        ## calculate medians for each group
        custom_grp_median_data <-
          ctf_long_dyn %>%
          filter(
            var_name %in% spec_varname,
            country_name %in% custom_df_per_group$Countries ## extract countries that fall in this group
          ) %>%
          mutate(
            country_name = unique(custom_df_per_group$Grp), ## the country name will be the 
            ## name of the group.
            group = NA
          ) %>%
          unique %>%
          group_by(
            country_name,
            year,
            var_name
          ) %>%
          mutate(value = median(value, na.rm = TRUE)) %>%
          distinct(var_name, year, value, country_name) %>% 
          ungroup
        
        return(custom_grp_median_data)
      }
      
      custom_grp_median_data_df <- purrr::map_df(selected_custom_grps, custom_grp_median_data_func)
 
      ## and append this to median data generated for pre-determined groups
      median_data <- median_data %>%
        bind_rows(custom_grp_median_data_df)
    }
    
  } 
  
  ## ------------------------------------------------------------------------------------
  
  if ("Comparison countries" %in% group_median) {
    
    countries <-
      ctf_long_dyn %>%
      filter(
        var_name %in% spec_varname,
        country_name %in% data$country_name,
        country_name != base_country
      ) %>%
      mutate(
        country_name = "Comparison countries",
        group = NA
      ) %>%
      unique %>%
      group_by(
        country_name,
        year,
        var_name2
      ) %>%
      mutate(value = median(value, na.rm = TRUE)) %>%
      distinct(country_name, year, var_name, var_name2, value) %>% 
      ungroup
    
    median_data <-
      median_data %>%
      bind_rows(countries)
  }

  
  plot <-
    plot +
    suppressWarnings(geom_point(
      data = median_data %>% filter(!is.na(value)),
      aes(
        y = value,
        x = year,
        shape = country_name,
        text = paste(
          " Group:", country_name,"<br>",
          "Median closeness to frontier:", round(value, 3)
        )
      ),
      alpha = .5,
      color = "black",
      fill = "white",
      size = 3
    )) +
    scale_shape_manual(
      values = 22:25 #,
      #lab = NULL
    )
}




## add comparison countries
if (dots) {
  plot <-
    plot +
    suppressWarnings(geom_point(
      data = data,
      aes(
        x = year,
        y = var,
        text = text
      ),
      shape = 21,
      size = 2,
      color = "gray30",
      fill = "white",
      alpha = .5
    ))  
}

n_col = ifelse(length(unique(data$var_name)) >= 5, 1, 2)

plot <- plot +
         facet_wrap(vars(var_name), ncol = n_col)+
         theme(strip.text = element_text(face = "bold", size = 10))
plot 