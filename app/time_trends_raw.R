library(tidyverse)
library(ggtext)
library(here)

source("global.R")
source("auxiliary/vars-by-family.R")

comparison_group <- c(
  "Morocco", "Egypt", "Tunisia", "Pakistan",
  "Turkey", "Philippines", "Indonesia", "Georgia",
  "Finland", "Ireland", "Croatia", "Bulgaria", "Uruguay"
)

data <-
  read_rds(
    here(
      "data",
      "raw_data.rds"
    )
  ) %>% 
  filter(country_name %in% c(comparison_group, "Jordan")) %>%  
  select(1:3, all_of(vars_all)) %>%
  mutate(
    groups = case_when(
      country_name %in% c("Morocco", "Egypt", "Tunisia") ~ "Regional peers",
      country_name == "Jordan" ~ "Jordan",
      TRUE ~ "Emerging/aspirational peers"
    ),
    groups = fct_relevel(groups, "Jordan", "Regional peers", "Emerging/aspirational peers")
  ) %>% 
  group_by(groups, year) %>% 
  filter(year >= 1990) %>% 
  summarise(
    across(
      bribes:v2xcs_ccsi,
      ~ mean(.x, na.rm = TRUE)
    )
  ) 

note_chart <- 120

# Functions ----------------------------------------------------------

make_plot_jordan <- function(x) {
  source <- db_variables %>% 
    filter(variable == x) %>% 
    .$`source`
  
  note <- db_variables %>% 
    filter(variable == x) %>% 
    .$`description`
  
  values <- data %>% 
    group_by(groups) %>% 
    summarise(
      min = min(.data[[x]], na.rm = TRUE),
      max = max(.data[[x]], na.rm = TRUE),
      sd = sd(.data[[x]],   na.rm = TRUE)
    )
  
  limits <- c(min(values$min + (-2 * values$sd)), max(values$max + (2 * values$sd)))
  
  plot <- data %>% 
    filter(groups == "Jordan") %>% 
    filter(!is.na(.data[[x]])) %>% 
    ggplot(
      aes(
        x = year,
        y = .data[[x]],
        color = groups, 
        group = groups
      )
    ) +
    geom_point() +
    geom_line() +
    scale_y_continuous(limits = limits) +
    labs(
      x = NULL,
      y = NULL,
      color = NULL,
      caption = 
        paste(
          stringr::str_wrap(
            paste(
              "**Definition:**",
              note
            ),
            note_chart
          ),
          stringr::str_wrap(
            paste(
              "**Source:**",
              source
            ),
            note_chart
          ),
          sep = "<br>"
        ) %>% str_replace_all(., "\\n", "<br>")
    ) +
    theme_minimal() +
    theme(
      legend.direction = "horizontal", 
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text  = element_text(size = 12),
      panel.grid.minor = element_blank(),
      axis.ticks   = element_blank(),
      axis.text    = element_text(color = "black"),
      axis.text.y  = element_text(size = 12, hjust = 0),
      axis.text.x  = element_text(size = 12, angle = 45, vjust = 0.5),
      plot.caption = element_markdown(size = 12, hjust = 0),
      plot.caption.position =  "plot"
    ) 
  
  plot
}

make_plot_groups <- function(x) {
  source <- db_variables %>% 
    filter(variable == x) %>% 
    .$`source`
  
  note <- db_variables %>% 
    filter(variable == x) %>% 
    .$`description`
  
  values <- data %>% 
    group_by(groups) %>% 
    summarise(
      min = min(.data[[x]], na.rm = TRUE),
      max = max(.data[[x]], na.rm = TRUE),
      sd = sd(.data[[x]], na.rm = TRUE)
    )
  
  limits <- c(min(values$min + (-2 * values$sd)), max(values$max + (2 * values$sd)))
  
  plot <- data %>% 
    filter(groups != "Jordan") %>% 
    filter(!is.na(.data[[x]])) %>% 
    ggplot(
      aes(
        x = year,
        y = .data[[x]],
        group = groups,
        color = groups
      )
    ) +
    geom_point() +
    geom_line() +
    scale_y_continuous(limits = limits) +
    labs(
      x = NULL,
      y = NULL,
      color = NULL,
      caption = 
        paste(
          stringr::str_wrap(
            paste(
              "**Definition:**",
              note
            ),
            note_chart
          ),
          stringr::str_wrap(
            paste(
              "**Source:**",
              source
            ),
            note_chart
          ),
          sep = "<br>"
        ) %>% str_replace_all(., "\\n", "<br>")
    ) +
    theme_minimal() +
    theme(
      legend.direction = "horizontal", 
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text  = element_text(size = 12),
      panel.grid.minor = element_blank(),
      axis.ticks   = element_blank(),
      axis.text    = element_text(color = "black"),
      axis.text.y  = element_text(size = 12, hjust = 0),
      axis.text.x  = element_text(size = 12, angle = 45, vjust = 0.5),
      plot.caption = element_markdown(size = 12, hjust = 0),
      plot.caption.position =  "plot"
    ) 
  
  
  plot
}

make_plot <- function(x) {
  source <- db_variables %>% 
    filter(variable == x) %>% 
    .$`source`
  
  note <- db_variables %>% 
    filter(variable == x) %>% 
    .$`description`
  
  values <- data %>% 
    group_by(groups) %>% 
    summarise(
      min = min(.data[[x]], na.rm = TRUE),
      max = max(.data[[x]], na.rm = TRUE),
      sd = sd(.data[[x]], na.rm = TRUE)
    )
  
  limits <- c(min(values$min + (-2 * values$sd)), max(values$max + (2 * values$sd)))
  
  plot <- data %>% 
    filter(!is.na(.data[[x]])) %>% 
    ggplot(
      aes(
        x = year,
        y = .data[[x]],
        group = groups,
        color = groups
      )
    ) +
    geom_point() +
    geom_line() +
    scale_y_continuous(limits = limits) +
    labs(
      x = NULL,
      y = NULL,
      color = NULL,
      caption = 
        paste(
          stringr::str_wrap(
            paste(
              "**Definition:**",
              note
            ),
            note_chart
          ),
          stringr::str_wrap(
            paste(
              "**Source:**",
              source
            ),
            note_chart
          ),
          sep = "<br>"
        ) %>% str_replace_all(., "\\n", "<br>")
    ) +
    theme_minimal() +
    theme(
      legend.direction = "horizontal", 
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text  = element_text(size = 12),
      panel.grid.minor = element_blank(),
      axis.ticks   = element_blank(),
      axis.text    = element_text(color = "black"),
      axis.text.y  = element_text(size = 12, hjust = 0),
      axis.text.x  = element_text(size = 12, angle = 45, vjust = 0.5),
      plot.caption = element_markdown(size = 12, hjust = 0),
      plot.caption.position =  "plot"
    ) 
  
  
  plot
}


# PURRR Plots --------------------------------------------------------

## Jordan

mapvars   <- names(data)[3:48]
all_plots <- map(mapvars, ~ make_plot_jordan(.x))

walk2(
  mapvars, 
  all_plots, 
  ~ ggsave(
    filename = paste0("figs/time_trends_vars/", .x, "_jordan", ".png"),
    plot = .y,
    bg = "white",
    width = 12, 
    height = 8,
    scale = 0.8
  )
)

## Averages
all_plots <- map(mapvars, ~ make_plot_groups(.x))

walk2(
  mapvars, 
  all_plots, 
  ~ ggsave(
    filename = paste0("figs/time_trends_vars/", .x, "_averages", ".png"),
    plot = .y,
    bg = "white",
    width = 12, 
    height = 8,
    scale = 0.8
  )
)

## The 3 groups together
all_plots <- map(mapvars, ~ make_plot(.x))

walk2(
  mapvars, 
  all_plots, 
  ~ ggsave(
    filename = paste0("figs/time_trends_vars/", .x, ".png"),
    plot = .y,
    bg = "white",
    width = 12, 
    height = 8,
    scale = 0.8
  )
)

