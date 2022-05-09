library(tidyverse)
library(here)

# Variables ----------------------------------------------------------
source("global.R")
source("auxiliary/static_plot2.R")
source("auxiliary/vars-by-family.R")

# Read data ----------------------------------------------------------

global_data_2014_2016 <- read_rds(
  here(
    "data",
    "closeness_to_frontier_2014_2016.rds"
  )
) %>%
  ungroup()

global_data_2017_2020 <- read_rds(
  here(
    "data",
    "closeness_to_frontier_2017_2020.rds"
  )
) %>%
  ungroup()

# Variables ----------------------------------------------------------

families <- variable_names %>% 
  distinct(family_name)

base_country <- "Jordan"

comparison_group <- c(
  "Morocco", "Egypt", "Tunisia", "Pakistan",
  "Turkey", "Philippines", "Indonesia", "Georgia",
  "Finland", "Ireland", "Croatia", "Bulgaria", "Uruguay"
)

# Overview Plot
data_family_2014_2016 <- family_data(
  global_data_2014_2016,
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
  select(country_name, variable, dtf1 = dtf, var_name)

data_family_2017_2020 <- family_data(
  global_data_2017_2020,
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
  select(country_name, variable, dtf2 = dtf, var_name)

data_family_comparison <- data_family_2014_2016 %>% 
  left_join(data_family_2017_2020) %>% 
  mutate(
    diff   = dtf2 - dtf1,
    jordan = if_else(country_name == "Jordan", TRUE, FALSE),
    case   = case_when(diff > 0 ~ "Improved",
                       diff == 0 ~ "No change",
                       diff < 0 ~ "Worsened"),
  ) %>% 
  filter(var_name != "Labor market")

# Overivew plot ------------------------------------------------------

data_family_comparison %>% 
  filter(var_name != "Labor market") %>% 
  ggplot(
    aes(
      x = diff,
      y = var_name
    )
  ) +
  geom_vline(xintercept = 0) +
  geom_point(
    aes(
      color = case,
      shape = jordan
    ),
    alpha = 0.5,
    size = 3
  ) +
  geom_point(
    aes(
      fill = case
    ),
    shape = 23,
    size = 7,
    show.legend = FALSE,
    data = filter(data_family_comparison, country_name == "Jordan")
  ) +
  scale_fill_manual(values = c("#8ec18e", "#e47a81"), name = "") +
  scale_color_manual(values = c("#8ec18e", "#e47a81"), name = "") +
  scale_shape_manual(values = c(16, 23), label = c("Comparison countries", "Jordan"), name = "") +
  scale_x_continuous(limits = c(-0.35,0.35)) +
  scale_y_discrete(labels = scales::wrap_format(30), limits = rev) + 
  theme_minimal() +
  labs(
    x = "Closeness to frontier (Average 2017-2020 minus Average 2014-2016)",
    y = "",
    color = NULL,
    caption = ""
  ) +
  theme(
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 12),
    # legend.box.just = "top",
    # legend.spacing = unit(3, "cm"),
    panel.grid.minor = element_blank(),
    axis.ticks   = element_blank(),
    axis.text    = element_text(color = "black"),
    axis.text.y  = element_text(size = 12, hjust = 0),
    axis.text.x  = element_text(size = 12),
    plot.caption = element_text(size = 12, hjust = 1),
    plot.caption.position =  "plot"
  )

ggsave("figs/benchmark_trends_overview.png", bg = "white", width = 12, height = 8, scale = 0.85)
  
# By Family ----------------------------------------------------------

# By family
source("auxiliary/vars-by-family.R")

families <- variable_names %>% 
  distinct(family_name) %>% 
  .$`family_name`

fam_vars <- list(
  "Anti-Corruption, Transparency and Accountability" = vars_transp,
  "Political" = vars_pol,
  "Justice" = vars_leg,
  "Financial market" = vars_fin, 
  # "Labor market" = vars_lab,
  "Business environment and trade" = vars_mkt,
  "Public sector performance" = vars_publ,
  "Social" = vars_social
)

# Make plot function
make_fam_plot <- function(fam) {
  
  df1 <- global_data_2014_2016 %>%
    def_quantiles(
      base_country,
      country_list,
      comparison_group,
      fam_vars[[fam]],
      variable_names
    ) %>% 
    select(country_name, variable, dtf1 = dtf, var_name)
  
  df2 <- global_data_2017_2020 %>%
    def_quantiles(
      base_country,
      country_list,
      comparison_group,
      fam_vars[[fam]],
      variable_names
    ) %>% 
    select(country_name, variable, dtf2 = dtf, var_name)
  
  df <- df1 %>% 
    left_join(df2) %>% 
    mutate(
      diff   = dtf2 - dtf1,
      jordan = if_else(country_name == "Jordan", TRUE, FALSE),
      case   = case_when(
        diff > 0  ~ "Improved",
        diff == 0 ~ "No change",
        diff < 0  ~ "Worsened"
      )
    ) %>% 
    filter(!is.na(diff))
  
  levels <- df %>% filter(country_name == "Jordan") %>% .$`case`
  values <- df %>% filter(country_name == "Jordan") %>% .$`diff`
  
  if (length(levels)==1 & values > 0){
    color_fill <- "#8ec18e"
  } else if (length(levels)==1 & values < 0){
    color_fill <- "#e47a81"
  } else if (length(levels)==1 & values == 0){
    color_fill <- "#FFBA01"
  } else {
    color_fill <- c("#8ec18e", "#e47a81", "#FFBA01")
  }

    
  if (length(unique(df$case))==3){
    colores <- c("#8ec18e", "#e47a81", "#FFBA01")
    df <- df %>% 
      mutate(
        case = fct_relevel(case, "Improved", "Worsened", "No change") 
      )
  } else {
    colores <- c("#8ec18e", "#e47a81")
    df <- df %>% 
      mutate(
        case = fct_relevel(case, "Improved", "Worsened") 
      )
  }
  
  plot <- df %>% 
    ggplot(
      aes(
        x = diff,
        y = var_name
      )
    ) +
    geom_vline(xintercept = 0) +
    geom_point(
      aes(
        color = case,
        shape = jordan
      ),
      alpha = 0.5,
      size = 3
    ) +
    geom_point(
      aes(
        fill = case,
      ),
      shape = 23,
      size = 7,
      show.legend = FALSE,
      data = filter(df, country_name == "Jordan")
    ) +
    scale_fill_manual(values = color_fill, name = "") +
    scale_color_manual(values = colores, name = "") +
    scale_shape_manual(values = c(16, 23), label = c("Comparison countries", "Jordan"), name = "") +
    scale_x_continuous(limits = c(-0.35,0.35)) +
    scale_y_discrete(labels = scales::wrap_format(30), limits = rev) + 
    theme_minimal() +
    labs(
      x = "Closeness to frontier (Average 2017-2020 minus Average 2014-2016)",
      y = "",
      color = NULL,
      caption = ""
    ) +
    theme(
      legend.direction = "horizontal", 
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text  = element_text(size = 12),
      panel.grid.minor = element_blank(),
      axis.ticks   = element_blank(),
      axis.text    = element_text(color = "black"),
      axis.text.y  = element_text(size = 12, hjust = 0),
      axis.text.x  = element_text(size = 12),
      plot.caption = element_text(size = 12, hjust = 1),
      plot.caption.position =  "plot"
    ) 
  
  return(plot)
}

map(1:length(fam_vars), ~ {
  name <- names(fam_vars)[.x] %>% str_to_lower() %>% str_replace_all(., " ", "_")
  name <- paste0("figs/benchmark_trends_", name, ".png")
  make_fam_plot(.x)
  ggsave(filename = name, bg = "white", width = 12, height = 8, scale = 0.85)
})

