

  static_plot <-
    function(data,
             selected_country,
             tab_name) {

      ggplot(data) +
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
              text = map(paste(
                '<b>Country:</b>', selected_country, '<br>',
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
        xlab("") +
        labs(title = paste0(tab_name))

    }

  interactive_plot <-
    function(x, y, z, tab_name) {
      x %>%
        ggplotly(tooltip = "text") %>%
        layout(
          margin = list(l=50, r=50, t=75, b=125),
          annotations =
            list(x = 0, y = -0.55,
                 text = map(paste0("Note: ",y,", ",paste(z, collapse = ", "),".",
                                   "<br>Closeness to frontier is calculated as (worst-y)/(worst-frontier).",
                                   "<br>1 identifies the best performer and 0 the worst performer",
                                   "<br>Weak = bottom 25%; Emerging = 25%-50%; Advanced = top 50%."), HTML),
                 showarrow = F,
                 xref = 'paper',
                 yref = 'paper',
                 align = 'left',
                 font = list(size = 10)
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
          margin = list(b = -1.5),
          legend = list(
            title=list(text='<b>Closeness to\nfrontier:</b>'),
            #orientation = 'h',
            y=0.5#,
            #x=0.5,
            #xanchor = "center",
            #bordercolor = "#00000",
            #borderwidth = 1
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

