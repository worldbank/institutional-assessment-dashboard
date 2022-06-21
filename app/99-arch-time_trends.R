# Time trends graphs
library(tidyverse)
library(haven)

gbid_jordan <- read_dta("C:/Users/ifyou/Downloads/time_trends/data/gbid-data_jordan_long.dta") %>% 
  janitor::clean_names()

gbid_jordan %>% 
  filter(family == "") %>%
  mutate(
    diff = if_else(dtf_2018_2020 > dtf_2013_2017, "Improved", "Worsened"),
    indicatornamefromdefinitions = fct_reorder(indicatornamefromdefinitions, -dtf_2018_2020)
  ) %>% 
  ggplot(
    aes(
      x = dtf_2018_2020,
      y = indicatornamefromdefinitions
    )
  ) +
  geom_col(
    aes(
      fill = diff
    ), 
    color = "black"
  ) +
  geom_point(
    aes(
      x = dtf_2013_2017
    ), 
    color = "black",
    size = 10,
  ) +
  geom_point(
    aes(
      x = dtf_2013_2017,
    ),
    color = "white",
    size = 9,
  ) +
  labs(
    x = "Closeness to frontier",
    y = NULL,
    fill = "2018-2019",
    caption = "Note: Indicators standardized using the min and max in the full sample over the full period.\nDot represents 2013-2017."
  ) +
  scale_fill_manual(values = c("#8ec18e", "#e47a81")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.ticks   = element_blank(),
    axis.text    = element_text(color = "black"),
    axis.text.y  = element_text(size = 12),
    axis.text.x  = element_text(size = 11),
    legend.box   = "vertical",
    plot.caption = element_text(size = 12, hjust = 1),
    plot.caption.position =  "plot"
  ) 

ggsave("figs/trends_overall.png", bg = "white")



# Function per family ------------------------------------------------

data <- gbid_jordan %>% 
  filter(family != "") %>% 
  mutate(
    diff = if_else(dtf_2018_2020 > dtf_2013_2017, "Improved", "Worsened"),
    indicatornamefromdefinitions = fct_reorder(indicatornamefromdefinitions, -dtf_2018_2020)
  )

all_plots <- data %>% 
  split(data$family) %>% 
  map(
    .,
    ~ ggplot(.x, 
      aes(
        x = dtf_2018_2020,
        y = indicatornamefromdefinitions
      )
    ) +
      geom_col(
        aes(
          fill = diff
        ), 
        color = "black"
      ) +
      geom_point(
        aes(
          x = dtf_2013_2017
        ), 
        color = "black",
        size = 10,
      ) +
      geom_point(
        aes(
          x = dtf_2013_2017,
        ),
        color = "white",
        size = 9,
      ) +
      labs(
        x = "Closeness to frontier",
        y = NULL,
        fill = "2018-2019",
        caption = "Note: Indicators standardized using the min and max in the full sample over the full period.\nDot represents 2013-2017."
      ) +
      scale_fill_manual(values = c("#8ec18e", "#e47a81")) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.ticks   = element_blank(),
        axis.text    = element_text(color = "black"),
        axis.text.y  = element_text(size = 12),
        axis.text.x  = element_text(size = 11),
        legend.box   = "vertical",
        plot.caption = element_text(size = 12, hjust = 1),
        plot.caption.position =  "plot"
      )
  )

names <- data %>% 
  split(data$family_short) %>% 
  names() %>% 
  str_to_lower() %>% 
  str_replace_all(., " ", "_")

names <- paste0("figs/trends_", names, ".png")

walk2(
  names, all_plots, 
  ~ ggsave(
    filename = .x, 
    plot = .y,
    bg = "white"
  )
)
