# Time trends graphs
library(tidyverse)
library(haven)

# Countries ----------------------------------------------------------

base_country <- "Jordan"

comparison_group <- c(
  "Morocco", "Egypt", "Tunisia", "Pakistan",
  "Turkey", "Philippines", "Indonesia", "Georgia",
  "Finland", "Ireland", "Croatia", "Bulgaria", "Uruguay"
)

# Get data -----------------------------------------------------------
source("auxiliary/vars-by-family.R")

gbid_1720 <- read_rds(
  here(
    "data",
    "closeness_to_frontier_long_2017_2020.rds"
  )
) %>% 
  filter(country_name %in% base_country) %>% 
  rename(dtf_2017_2020 = value)

gbid_1416 <- read_rds(
  here(
    "data",
    "closeness_to_frontier_long_2014_2016.rds"
  )
) %>% 
  filter(country_name %in% base_country) %>% 
  rename(dtf_2014_2016 = value)

gbid_overview <- gbid_1416 %>% 
  right_join(gbid_1720) %>% 
  filter(
    variable %in% c("vars_transp", "vars_pol", "vars_leg", "vars_fin", "vars_mkt", "vars_publ", "vars_social")
  ) %>% 
  mutate(
    diff = case_when(
      dtf_2017_2020 >  dtf_2014_2016 ~ "Improved", 
      dtf_2017_2020 == dtf_2014_2016 ~ "No change",
      dtf_2017_2020 <  dtf_2014_2016 ~ "Worsened"
    ),
    diff = fct_relevel(diff, "Improved", "Worsened") 
  ) %>% 
  group_by(var_name) %>% 
  slice(1) %>% 
  ungroup()

gbid <- gbid_1416 %>% 
  right_join(gbid_1720) %>% 
  filter(variable %in% c(vars_transp, vars_pol, vars_leg, vars_fin, vars_mkt, vars_publ, vars_social)) %>% 
  mutate(
    diff = case_when(
      dtf_2017_2020 >  dtf_2014_2016 ~ "Improved", 
      dtf_2017_2020 == dtf_2014_2016 ~ "No change",
      dtf_2017_2020 <  dtf_2014_2016 ~ "Worsened"
    ),
    diff = fct_relevel(diff, "Improved", "Worsened", "No change") 
  ) %>% 
  group_by(var_name) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(
    family_name = ifelse(variable == "favoritism", "Public sector performance", family_name),
    family_name = ifelse(variable == "v2clrspct", "Anti-Corruption, Transparency and Accountability", family_name),
    family_short = case_when(
      family_name == "Anti-Corruption, Transparency and Accountability" ~ "anticorruption",
      family_name == "Business environment and trade" ~ "business",
      family_name == "Financial market" ~ "financial",
      family_name == "Justice" ~ "justice",
      family_name == "Labor market" ~ "labor",
      family_name == "Political" ~ "political",
      family_name == "Public sector performance" ~ "public",
      family_name == "Social" ~ "social",
    )
  )

# Overview -----------------------------------------------------------

gbid_overview %>% 
  ggplot(
    aes(
      y = var_name
    )
  ) +
  geom_col(
    aes(
      x = dtf_2017_2020
    ),
    alpha = 0.7,
    color = "black",
    fill = "grey70",
    show.legend = FALSE,
    width = 0.5
  ) + 
  geom_point(
    aes(
      x = dtf_2014_2016,
      fill = country_name
    ),
    size = 5
  ) +
  geom_point(
    aes(
      x = dtf_2014_2016,
      fill = country_name
    ),
    color = "white",
    size = 4
  ) +
  geom_segment(
    aes(
      x = dtf_2014_2016,
      xend = dtf_2017_2020,
      yend = var_name,
      color = diff
    ),
    arrow = arrow(length = unit(0.3, "cm")),
    size = 1.5
  ) +
  labs(
    x = "Closeness to frontier",
    y = NULL,
    color = "",
    fill = "",
    caption = "Note: Indicators standardized using the min and max in the full sample over the full period."
  ) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_discrete(labels = scales::wrap_format(30), limits = rev) + 
  scale_fill_manual(label = NULL, values = c("white"), name = "2014-2016") +
  scale_color_manual(values = c("#00b800", "#e47a81"), name = "2017-2020") +
  guides(
    fill  = guide_legend(order = 1, title.position = "top", title.hjust = 0.5),
    color = guide_legend(order = 2, title.position = "top", label.vjust = 0.5, label.position = "bottom")
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
    axis.text.x  = element_text(size = 12),
    plot.caption = element_text(size = 12, hjust = 1),
    plot.caption.position =  "plot"
  ) 

ggsave("figs/trends_overview.png", bg = "white", width = 12, height = 8, scale = 0.7)
ggsave("figs/trends_overview.svg", bg = "white", width = 12, height = 8, scale = 0.7)


# Families -----------------------------------------------------------

all_plots <- gbid %>% 
  split(gbid$family_name) %>% 
  map(
    .,
    ~ .x %>% 
      ggplot(
        aes(y = var_name)
      ) +
      geom_col(
        aes(
          x = dtf_2017_2020
        ),
        alpha = 0.7,
        color = "black",
        fill = "grey70",
        show.legend = FALSE,
        width = 0.5
      ) + 
      geom_point(
        aes(
          x = dtf_2014_2016,
          fill = country_name
        ),
        size = 7
      ) +
      geom_point(
        aes(
          x = dtf_2014_2016,
        ),
        color = "white",
        size = 5,
        show.legend = FALSE
      ) +
      geom_segment(
        aes(
          x     = dtf_2014_2016,
          xend  = dtf_2017_2020,
          yend  = var_name,
          color = diff
        ),
        show.legend = TRUE, 
        size = 1.5,
      ) +
      geom_segment(
        aes(
          x     = dtf_2014_2016 ,
          xend  = dtf_2017_2020,
          yend  = var_name,
          color = diff
        ),
        show.legend = FALSE, 
        data = filter(.x, diff != "No change"),
        arrow = arrow(length = unit(0.35, "cm")),
        size = 1.5,
      ) +
      geom_point(
        aes(
          x = dtf_2014_2016,
          y = var_name,
          color = diff
        ),
        show.legend = FALSE,
        data = filter(.x, diff == "No change"),
        size = 6
      ) +
      scale_x_continuous(limits = c(0,1)) +
      scale_y_discrete(labels = scales::wrap_format(30), limits = rev) + 
      scale_fill_manual(label = NULL, values = "white", name = "2014-2016") +
      scale_color_manual(values = c("#00b800","#e47a81", "#FFBA01"), name = "2017-2020") +
      guides(
        fill = guide_legend(
          order = 1,
          title.position = "top",
          title.hjust = 0.5
        ),
        color = guide_legend(
          order = 2,
          title.position = "top",
          label.position = "bottom",
          override.aes = list(arrow = NULL)
        )
      ) +
      labs(
        x = "Closeness to frontier",
        y = NULL,
        color = "",
        fill = "",
        caption = "Note: Indicators standardized using the min and max in the full sample over the full period."
      ) +
      theme_minimal() +
      theme(
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text  = element_text(size = 12),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 12),
        axis.ticks   = element_blank(),
        axis.text    = element_text(color = "black"),
        axis.text.y  = element_text(size = 12, hjust = 0),
      )
  )

names <- gbid %>% 
  split(gbid$family_short) %>% 
  names() %>% 
  str_to_lower() %>% 
  str_replace_all(., " ", "_")

names_png <- paste0("figs/trends_", names, ".png")
names_svg <- paste0("figs/trends_", names, ".svg")

walk2(
  names_png, all_plots, 
  ~ ggsave(
    filename = .x, 
    plot = .y,
    bg = "white",
    width = 12, 
    height = 8,
    scale = 0.7
  )
)

walk2(
  names_svg, all_plots, 
  ~ ggsave(
    filename = .x, 
    plot = .y,
    bg = "white",
    width = 12, 
    height = 8,
    scale = 0.7
  )
)
