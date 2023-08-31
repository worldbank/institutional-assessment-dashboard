source("global.R")

base_country = "United States"
tab_name = "Social"
rank = FALSE
group_median = c("Sub-Saharan Africa", "Arab World")
custom_df = NULL
title = TRUE
dots = FALSE
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
    
## adding the line graph
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
plot <- plot +
         facet_wrap(vars(var_name), ncol = 1)
plot 