

static_plot <-
  function(data,
           base_country,
           tab_name) {

    order <-
      data %>%
      filter(country_name == base_country) %>%
      arrange(dtt) %>%
      select(var_name) %>%
      unlist

    data$var_name = factor(data$var_name,
                           levels = order,
                           ordered = TRUE)

    bar_colors_dtf <-
      data %>%
      transmute(var_name = var_name,
                Weak = q25,
                Emerging = q50 - q25,
                Advanced = 1 - q50) %>%
      unique %>%
      pivot_longer(cols = c(Weak, Emerging, Advanced),
                   names_to = "status",
                   values_to = "dtf")

    bar_colors_dtt <-
      data %>%
      transmute(var_name = var_name,
                Weak = .25,
                Emerging = .25,
                Advanced = .50) %>%
      unique %>%
      pivot_longer(cols = c(Weak, Emerging, Advanced),
                   names_to = "status",
                   values_to = "dtt")
    shapes <- c(
      "LAC6 Median" = 22,
      "OECD Median" = 23,
      "Structural Median" = 24
    )

    colors <-
      c("Weak" = "#D2222D",
        "Emerging" = "#FFBF00",
        "Advanced" = "#238823"
      )

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
        data = data %>%
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
        data = data %>%
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
        data = data %>%
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
        data = data %>% filter(country_name == base_country),
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
      ) +
      labs(title = paste0(tab_name))

  }

interactive_plot <-
  function(x, y, z, tab_name) {
    x %>%
      ggplotly(tooltip = "text") %>%
      layout(
        margin = list(l=50, r=50, t=75, b=140),
        annotations =
          list(x = 0, y = -0.475,
               text = paste0("<b>Notes:</b> ", y, " compared to ", paste(z, collapse = ", "), ".",
                             #"<br>Black squares show the group average.",
                             #"<br>Closeness to frontier is calculated as (worst-y)/(worst-frontier).",
                             #"<br>1 identifies the best performer and 0 the worst performer.",
                             "<br>Weak = bottom 25%; Emerging = 25%-50%; Advanced = top 50%."),
               showarrow = F,
               xref = 'paper',
               yref = 'paper',
               align = 'left',
               font = list(size = 12)
          )
      ) %>%
      config(
        modeBarButtonsToRemove = c("zoomIn2d",
                                   "zoomOut2d",
                                   "pan2d",
                                   "autoScale2d",
                                   "lasso2d",
                                   "select2d",
                                   "toggleSpikelines",
                                   "hoverClosest3d",
                                   "hoverClosestCartesian",
                                   "hoverCompareCartesian"),
        toImageButtonOptions= list(filename = paste0(tolower(stringr::str_replace_all(tab_name,"\\s","_"))))
      )


  }

interactive_map <-
  function(x, title) {
    x %>%
      ggplotly(tooltip = "text") %>%
      layout(
        legend = list(
          title=list(text='<b>Closeness to\nfrontier:</b>'),
          y=0.5
        ),
        margin = list(t=75,b=125),
        annotations =
          list(x = 0, y = -0.2,
               text = map(paste0("<b>Disclaimer:</b> Country borders or names do not necessarily reflect the World Bank Group's official position.",
                                 "<br>This map is for illustrative purposes and does not imply the expression of any opinion on the part of the World Bank,",
                                 "<br>concerning the legal status of any country or territory or concerning the delimitation of frontiers or boundaries."), HTML),
               showarrow = F,
               xref = 'paper',
               yref = 'paper',
               align = 'left',
               font = list(size = 13)
          )
      ) %>%
      config(
        modeBarButtonsToRemove = c(#"zoomIn2d",
          #"zoomOut2d",
          #"pan2d",
          #"autoScale2d",
          #"lasso2d",
          #"select2d",
          "hoverClosestCartesian",
          "toggleSpikelines",
          "hoverClosest3d",
          "hoverCompareCartesian"),
        toImageButtonOptions= list(filename = paste0(tolower(stringr::str_replace_all(title,"\\s","_")),"_map"),
                                   width = 1050,
                                   height =  675)
      )

  }


