library(here)
library(tidyverse)

colors_blind <-
  c("Weak" = "#D55E00",
    "Emerging" = "#E69F00",
    "Advanced" = "#009E73"
  )

colors <-
  c("Weak" = "#D2222D",
    "Emerging" = "#FFBF00",
    "Advanced" = "#238823"
  )

shapes <- c(
  "LAC6 Median" = 22,
  "OECD Median" = 23,
  "Structural Median" = 24
)

base_country <- "Uruguay"
family <- "vars_leg"

# Prepare data --------------------------------------

variable_names <-
  read_rds(here("app",
                "data",
                "variable_names.rds"))

data <-
  read_rds(here("app",
                "data",
                "country_dtf.rds"))

data[data$country_code == "COL", "oecd"] <- 1

country_level_data <-
  data %>%
  filter(oecd == 1 | country_name == base_country)

variables <-
  variable_names %>%
  filter(family_var == family,
         var_level == "indicator",
         variable %in% names(data)) %>%
  select(variable) %>%
  unlist %>%
  unname

vars <-
  country_level_data %>%
  ungroup %>%
  filter(country_name == base_country) %>%
  select(all_of(variables)) %>%
  select_if(~ !any(is.na(.))) %>%
  names

benchmark_data <-
  country_level_data %>%
  ungroup %>%
  select(country_name, all_of(vars),  oecd) %>%
  pivot_longer(cols = vars,
               names_to = "variable") %>%
  left_join(variable_names,
            by = "variable") %>%
  filter(!is.na(value)) %>%
  group_by(country_name, var_name,  oecd) %>%
  summarise(dtf = mean(value)) %>%
  group_by(var_name) %>%
  mutate(
    n = n(),
    dtt = percent_rank(dtf),
    q25 = quantile(dtf, c(0.25)),
    q50 = quantile(dtf, c(0.5)),
    r25 = floor(n * .25) / n,
    r50 = floor(n * .5) / n,
    status_dtt = case_when(
      dtt <= .25 ~ "Weak",
      dtt > .25 & dtt <= .50 ~ "Emerging",
      dtt > .50 ~ "Advanced"
    ),
    status_dtf = case_when(
      dtf <= q25 ~ "Weak",
      dtf > q25 & dtf <= q50 ~ "Emerging",
      dtf > q50 ~ "Advanced"
    )
  )

  order <-
    benchmark_data %>%
    filter(country_name == base_country) %>%
    arrange(dtt) %>%
    select(var_name) %>%
    unlist

  benchmark_data$var_name = factor(benchmark_data$var_name,
                                      levels = order,
                                      ordered = TRUE)

  bar_colors_dtt <-
    benchmark_data %>%
    transmute(var_name = var_name,
              Weak = .25,
              Emerging = .25,
              Advanced = .50) %>%
    unique %>%
    pivot_longer(cols = c(Weak, Emerging, Advanced),
                 names_to = "status",
                 values_to = "dtt")

  # By rank with median ---------------------------------------------------

  ggplot() +
    geom_col(
      data = bar_colors_dtt,
      aes(y = var_name,
          x = dtt,
          fill = status),
      width = .2,
      alpha = .6
    ) +
    geom_point(
      data = benchmark_data %>%
        filter(oecd == 1) %>%
        group_by(var_name) %>%
        summarise(dtt = median(dtt, na.rm = TRUE)) %>%
        mutate(group ="OECD Median"),
      aes(y = var_name,
          x = dtt,
          shape = group,
          fill = group,
          color = group,
          alpha = gr),
      alpha = .5,
      fill = "white",
      color = "black",
      size = 4
    ) +
    geom_point(
      data = benchmark_data %>%
        filter(lac6 == 1) %>%
        group_by(var_name) %>%
        summarise(dtt = median(dtt, na.rm = TRUE)) %>%
        mutate(group ="LAC6 Median"),
      aes(y = var_name,
          x = dtt,
          shape = group),
      alpha = .5,
      fill = "white",
      color = "black",
      size = 4
    ) +
    geom_point(
      data = benchmark_data %>%
        filter(structural == 1) %>%
        group_by(var_name) %>%
        summarise(dtt = median(dtt, na.rm = TRUE)) %>%
        mutate(group ="Structural Median"),
      aes(y = var_name,
          x = dtt,
          shape = group),
      alpha = .5,
      fill = "white",
      color = "black",
      size = 4
    ) +
    geom_point(
      data = benchmark_data %>% filter(country_name == base_country),
      aes(y = var_name,
          x = dtt,
          fill = status_dtf),
      size = 6,
      shape = 21,
      color = "gray0"
    ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      color = colors["Advanced"],
      size = 1
    ) +
    theme_minimal() +
    theme(legend.position = "top",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(color = "black"),
          axis.text.y = element_text(size = 10),
          legend.box = "vertical") +
    labs(y = NULL,
         x = NULL,
         fill = NULL,
         shape = NULL) +
    scale_shape_manual(
      values = shapes
    )+
    scale_fill_manual(
      values = colors
    ) +
    scale_color_manual(
      values = colors
    ) +
    guides(fill = guide_legend(ncol = 3),
           shape = guide_legend(ncol = 3)) +
    scale_x_continuous(breaks = c(0, 0.5, 1),
                       labels = c("Worst ranked",
                                  "Middle of ranking",
                                  "Top ranked")) +
    annotate(
      geom = "text",
      size = 3,
      x = .13,
      y = .7,
      label = "Bottom 25%"
    ) +
    annotate(
      geom = "text",
      size = 3,
      x = .4,
      y = .7,
      label = "25% - 50%"
    ) +
    annotate(
      geom = "text",
      size = 3,
      x = .75,
      y = .7,
      label = "Top 50%"
    )


# Annotation ----------------------------
#
#   benchmark_graph +
#      annotate(
#        geom = "text",
#        x = .86,
#        y = 9.5,
#        label = "Frontier",
#        fontface = 2
#       ) +
#      annotate(
#        geom = "curve",
#        xend = .91,
#        yend =  9.45,
#        x = 1,
#        y = 9.3,
#        curvature = .2,
#        arrow = arrow(length = unit(2, "mm")),
#        color = "#009E73",
#        size = .8
#      ) +
#      annotate(
#        geom = "text",
#        x = .42,
#        y = 5.5,
#        label = "Base country",
#        fontface =2
#      ) +
#      annotate(
#        geom = "curve",
#        xend = .33,
#        yend =  5.45,
#        x = .27,
#        y = 5,
#        curvature = -.4,
#        arrow = arrow(length = unit(2, "mm")),
#        color = "#D55E00",
#        size = .8
#      ) +
#      annotate(
#        geom = "text",
#        x = .26,
#        y = 1.5,
#        label = "Comparison group average",
#        fontface = 2
#      ) +
#     annotate(
#       geom = "curve",
#       xend = .43,
#       yend =  1.45,
#       x = .465,
#       y = 1.1,
#       curvature = .4,
#       arrow = arrow(length = unit(2, "mm")),
#       size = .8
#     ) +
#      annotate(
#        geom = "text",
#        x = .5,
#        y = 3.5,
#        label = "Comparison countries",
#        fontface = 2
#      ) +
#     annotate(
#       geom = "curve",
#       xend = .64,
#       yend =  3.45,
#       x = .68,
#       y = 3.9,
#       curvature = -.4,
#       arrow = arrow(length = unit(2, "mm")),
#       size = .8
#     )
