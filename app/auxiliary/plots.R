

  static_plot <-
    function(x) {

      ggplot(x) +
        geom_segment(
          aes(x = reorder(var_name,
                          -dtf),
              xend = reorder(var_name,
                             -dtf),
              y = 0,
              yend = dtf,
              color = classification),
          size = 1) +
        geom_point(
          aes(x = reorder(var_name,
                          -dtf),
              y = dtf,
              text = map(paste('<b>Country:</b>', selected_country, '<br>',
                               '<b>Closeness to frontier:</b>', round(dtf, digits = 3), '<br>',
                               '<b>Classification:</b>', classification), HTML),
              color = classification),
          size = 3)  +
        coord_flip() +
        scale_color_manual(
          values =
            c("Advanced"="#009E73",
              "Emerging"="#E69F00",
              "Weak"="#D55E00"
            )
        ) +
        scale_y_continuous(
          limits = c(0,1)
        )  +
        theme_ipsum() +
        theme(
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()
        ) +
        ylab("Closeness to frontier") +
        xlab("")

    }

  interactive_plot <-
    function(x, y, z) {
      x %>%
        ggplotly(tooltip = "text") %>%
        layout(
          margin = list(b = -1.5),
          annotations =
            list(x = 0, y = -0.25,
                 text = map(paste0("Note: ",y,", ",z,".",
                                   "<br>Closeness to frontier is calculated as (worst-y)/(worst-frontier).",
                                   "<br>1 identifies the best performer and 0 the worst performer",
                                   "<br>Weak = bottom 25%; Emerging = 25%-50%; Advanced = top 50%."), HTML),
                 showarrow = F,
                 xref = 'paper',
                 yref = 'paper',
                 align = 'left',
                 font = list(size = 9))
        ) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d",
                                          "zoomOut2d",
                                          "pan2d",
                                          "autoScale2d",
                                          "lasso2d",
                                          "select2d",
                                          "toggleSpikelines",
                                          "hoverClosest3d",
                                          "hoverCompareCartesian"))

    }

