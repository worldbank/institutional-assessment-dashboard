#data <- quantiles_group

static_plot <-
  function(data,
           base_country,
           tab_name) {

    order <-
      data %>%
      filter(country_name == base_country) %>%
      arrange(dtt) %>%
      select(var_name) %>%
      unique %>%
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

    colors <-
      c("Weak" = "#D2222D",
        "Emerging" = "#FFBF00",
        "Advanced" = "#238823"
      )

    data_group <- data %>%
      filter(!is.na(group)) %>%
      group_by(group, var_name) %>%
      summarise(dtt = median(dtt, na.rm = TRUE)) %>%
      mutate(group_med = paste0(group," Median"))

    shapes <- c(
      "High income Median" = 21,
      "Latin America & Caribbean Median" = 22,
      "OECD members Median" = 23,
      "European Union Median" = 24,
      "Low income Median" = 25
    )

    ggplot() +
      #geom_col(
      #  data = bar_colors_dtt,
      #  aes(y = var_name,
      #      x = dtt,
      #      fill = status),
      #  width = .2,
      #  alpha = .6
      #) +
      #geom_point(
      #  data = data_group,
      #  aes(y = var_name,
      #      x = dtt,
      #      shape = group_med),
      #  alpha = .5,
      #  color = "black",
      #  fill = "white",
      #  size = 4
      #) +
      geom_col(
        data = bar_colors_dtf,
        aes(y = var_name,
            x = dtf,
            fill = status),
        width = .3,
        alpha = .6
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
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 11),
            legend.box = "vertical") +
      labs(y = NULL,
           #x = NULL,
           x = "CTF",
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
      guides(
        fill = guide_legend(ncol = 3),
        shape = guide_legend(ncol = 3)
      ) +
      #scale_x_continuous(breaks = c(0, 0.5, 1),
      #                   labels = c("Worst ranked",
      #                              "Middle of ranking",
      #                              "Top ranked")) +
      #annotate(
      #  geom = "text",
      #  size = 3,
      #  x = .13,
      #  y = .65,
      #  label = "Bottom 25%"
      #) +
      #annotate(
      #  geom = "text",
      #  size = 3,
      #  x = .4,
      #  y = .65,
      #  label = "25% - 50%"
      #) +
      #annotate(
      #  geom = "text",
      #  size = 3,
      #  x = .75,
      #  y = .65,
      #  label = "Top 50%"
      #) +
      labs(title = paste0(tab_name))

  }

interactive_plot <-
  function(x, y, z, tab_name) {
    x %>%
      ggplotly(tooltip = "text") %>%
      layout(
        margin = list(l=50, r=50, t=75, b=140),
        annotations =
          list(x = 0, y = -0.4,
               text = paste0("<b>Notes:</b> ", y, " compared to ", paste(z, collapse = ", "), ".",
                             #"<br>Black squares show the group average.",
                             #"<br>Closeness to frontier is calculated as (worst-y)/(worst-frontier).",
                             #"<br>1 identifies the best performer and 0 the worst performer.",
                             "<br>Weak = bottom 25%; Emerging = 25%-50%; Advanced = top 50%."),
               showarrow = F,
               xref = 'paper',
               yref = 'paper',
               align = 'left',
               font = list(size = 13)
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
        toImageButtonOptions= list(filename = paste0(tolower(stringr::str_replace_all(tab_name,"\\s","_"))),
                                   width = 1100,
                                   height =  1000)
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


