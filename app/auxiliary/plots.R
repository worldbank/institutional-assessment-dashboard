# Benchmark plots ##############################################################

## Static plot =================================================================
static_plot <-
  function(data, base_country, tab_name) {

    data$var_name <-
      factor(
        data$var_name,
        levels = sort(unique(data$var_name),
                      decreasing = TRUE),
        ordered = TRUE
      )

    colors <-
      c("Weak\n(bottom 25%)" = "#D2222D",
        "Emerging\n(25% - 50%)" = "#FFBF00",
        "Advanced\n(top 50%)" = "#238823"
      )

    ggplot() +
      geom_segment(
        data = data,
        aes(
          y = var_name,
          yend = var_name,
          x = 0,
          xend = q25
        ),
        color = "#e47a81",
        size = 2,
        alpha = .1
      ) +
      geom_vline(
        xintercept = 1,
        linetype = "dashed",
        color = colors["Advanced"],
        size = 1
      ) +
      geom_segment(
        data = data,
        aes(
          y = var_name,
          yend = var_name,
          x = q25,
          xend = q50
        ),
        color = "#ffd966",
        size = 2,
        alpha = .3
      ) +
      geom_segment(
        data = data,
        aes(
          y = var_name,
          yend = var_name,
          x = q50,
          xend = 1
        ),
        color = "#8ec18e",
        size = 2,
        alpha = .3
      ) +
      geom_point(
        data = data %>% filter(country_name == base_country),
        aes(
          y = var_name,
          x = dtf,
          fill = status_dtf,
          text = paste("Country:", base_country,"<br>",
                       "Closeness to frontier:", round(dtf, 3))
        ),
        size = 3,
        shape = 21,
        color = "gray0"
      ) +
      theme_minimal() +
      theme(legend.position = "top",
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_text(color = "black"),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 11),
            legend.box = "vertical") +
      labs(y = NULL,
           x = "Closeness to Frontier",
           fill = NULL) +
      scale_fill_manual(
        values = colors
      ) +
      labs(title = paste0("<b>", tab_name, "</b>"))

  }

## Interactive plot ============================================================

interactive_plot <-
  function(x, y, z, tab_name, buttons) {
    x %>%
      ggplotly(tooltip = "text") %>%
      layout(
        margin = list(l = 50, r = 50, t = 75, b = 150),
        annotations =
          list(x = 0, y = -0.4,
               text = paste0("<b>Notes:</b> ", y, " compared to ", paste(z, collapse = ", "), "."),
               showarrow = F,
               xref = 'paper',
               yref = 'paper',
               align = 'left',
               font = list(size = 13)
          )
      ) %>%
      config(
        modeBarButtonsToRemove = buttons,
        toImageButtonOptions= list(filename = paste0(tolower(stringr::str_replace_all(tab_name,"\\s","_"))),
                                   width = 1100,
                                   height =  1000)
      )
  }

# Maps #########################################################################

## Static map ===================================================================

static_map <-
  function(data, var_selected, latest_year, title) {

    data %>%
      st_transform("+proj=robin") %>%
      left_join(latest_year %>% ungroup %>% select(country_code,max), by=c("WB_A3"="country_code")) %>%
      mutate(max = ifelse(is.na(max),"Not available", max)) %>%
    ggplot() +
      geom_sf(
        aes(
          fill = get(var_selected),
          text = paste0(WB_NAME, ": ",
                        get(paste0(var_selected, "_value")),"<br>",
                        "Year of latest information: ", max)
        ),
        color = "black",
        size = 0.1
      ) +
      scale_fill_manual(
        name = NULL,
        values = c("0.0 - 0.2" = "#D55E00",
                   "0.2 - 0.4" = "#DD7C00",
                   "0.4 - 0.6" = "#E69F00",
                   "0.6 - 0.8" = "#579E47",
                   "0.8 - 1.0" = "#009E73",
                   "Not available" = "#808080"),
        na.value = "#808080",
        drop = FALSE) +
      labs(title = paste0("<b>", title, "</b>")) +
      theme_void()

  }


## Interactive map =============================================================

interactive_map <-
  function(x, title, buttons) {

    x %>%
      ggplotly(tooltip = "text") %>%
      layout(
        legend = list(
          title = list(text = '<b>Closeness to\nfrontier:</b>'),
          y = 0.5
        ),
        margin = list(t = 75, b = 125),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
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
        modeBarButtonsToRemove = buttons,
        toImageButtonOptions = list(
          filename = paste0(tolower(stringr::str_replace_all(title,"\\s","_")),"_map"),
          width = 1050,
          height =  675
        )
      )

  }


