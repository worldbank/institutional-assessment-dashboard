
note_size <- 11
note_chars <- 170
color_groups <- colorRampPalette(c("#053E5D", "#60C2F7"))

plotly_remove_buttons <-
  c("zoomIn2d",
    "zoomOut2d",
    "pan2d",
    "autoScale2d",
    "lasso2d",
    "select2d",
    "toggleSpikelines",
    "hoverClosest3d",
    "hoverClosestCartesian",
    "hoverCompareCartesian")

# Benchmark plots ##############################################################

static_plot <-
  function(data, base_country, tab_name, title = TRUE) {

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
        "Strong\n(top 50%)" = "#238823"
      )

    plot <-
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
            fill = status,
            text = paste(" Country:", base_country,"<br>",
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
              legend.box = "vertical",
              plot.caption = element_text(size = 8,
                                          hjust = 0),
              plot.caption.position =  "plot") +
        labs(y = NULL,
             x = "Closeness to Frontier",
             fill = NULL) +
          scale_fill_manual(
          values = colors
        )

    if (title) {
      plot <-
        plot +
        labs(title = paste0("<b>", tab_name, "</b>"))
    }

    return(plot)


  }

median_static_plot <-
  function(data, base_country, group_medians, tab_name, title = TRUE, note = NULL) {

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
        "Strong\n(top 50%)" = "#238823"
      )

    shapes <-
      c(22,23,24)

    plot <-
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
          fill = status,
          text = paste(" Country:", base_country,"<br>",
                       "Closeness to frontier:", round(dtf, 3))
        ),
        size = 3,
        shape = 21,
        color = "gray0"
      ) +
      geom_point(
        data = data %>% filter(group %in% group_medians),
        aes(y = var_name,
            x = dtf,
            shape = country_name),
        alpha = .5,
        color = "black",
        fill = "transparent",
        size = 3
      ) +
      theme_minimal() +
      theme(legend.position = "top",
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_text(color = "black"),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 11),
            legend.box = "vertical",
            plot.caption = element_text(size = 8,
                                        hjust = 0),
            plot.caption.position =  "plot") +
      labs(y = NULL,
           x = "Closeness to Frontier",
           fill = NULL,
           shape = NULL,
           caption = note) +
      scale_fill_manual(
        values = colors
      ) +
      scale_shape_manual(
        values = shapes
      )

    if (title) {
      plot <-
        plot +
        labs(title = paste0("<b>", tab_name, "</b>"))
    }

    return(plot)


  }

interactive_plot <-
  function(x, y, z, tab_name, buttons, miss_var) {

    if (length(miss_var) > 0) {

      notes <-
        paste0(
          "<b>Notes:</b> ",
          y,
          " compared to ",
          paste(z, collapse = ", "),
          ". Indicators not considered because base country has no information or because of low variance: ",
          paste(miss_var, collapse = ", "),
          "."
        )

    }

    if (length(miss_var) == 0) {

      notes <-
        paste0(
          "<b>Notes:</b> ",
          y,
          " compared to ",
          paste(z, collapse = ", "),
          "."
        )

    }

    if (tab_name == "Overview") {
      notes <-
        paste(
          notes,
          "Family-level closeness to frontier is calculated by taking the average closeness to frontier for all the latest available indicators in each family."
        )
    }

    x %>%
      ggplotly(tooltip = "text") %>%
      layout(
        margin = list(l = 50, r = 50, t = 75, b = 150),
        annotations =
          list(
            x = -0.2,
            y = -0.6,
            text = HTML(str_wrap(notes, note_chars)),
            showarrow = F,
            xref = 'paper',
            yref = 'paper',
            align = 'left',
            font = list(size = note_size)
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
  function(x, var, definitions, buttons) {

    def <-
      definitions %>%
      filter(var_name == var)

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
          list(x = 0,
               y = -0.2,
               text = HTML(
                 paste(
                   str_wrap(
                     "<b>Disclaimer:</b> Country borders or names do not necessarily reflect the World Bank Group's official position.
                     This map is for illustrative purposes and does not imply the expression of any opinion on the part of the World Bank,
                     concerning the legal status of any country or territory or concerning the delimitation of frontiers or boundaries.",
                     note_chars
                   ),
                   str_wrap(
                     paste(
                       "<b>Definition:</b>",
                       def$description
                     ),
                     note_chars
                   ),
                   str_wrap(
                     paste(
                       "<b>Source:</b>",
                       def$source
                     ),
                     note_chars
                   ),
                   str_wrap(
                     "<b>Note:</b> The color illustrates the latest value of the indicator available for each country.",
                     note_chars
                   ),
                   sep = "<br>"
                 )
               ),
               showarrow = F,
               xref = 'paper',
               yref = 'paper',
               align = 'left',
               font = list(size = note_size)
          )
      ) %>%
      config(
        modeBarButtonsToRemove = buttons,
        toImageButtonOptions = list(
          filename = paste0(tolower(stringr::str_replace_all(var,"\\s","_")),"_map"),
          width = 1050,
          height =  675
        )
      )

  }

# Time series ###################################################################

trends_plot <- function(raw_data,
                        indicator, indicator_name,
                        base_country, comparison_countries, country_list, groups,
                        definitions) {

  def <-
    definitions %>%
    filter(var_name == indicator_name)

  indicator_data <-
    raw_data %>%
    select(Year, country_name, all_of(indicator))

  data_groups <-
    if (!is.null(groups)) {
      country_list %>%
        filter(group %in% groups) %>%
        left_join(indicator_data) %>%
        group_by(Year, group) %>%
        summarise_at(vars(all_of(indicator)),
                     ~ mean(., na.rm = TRUE)) %>%
        rename(country_name = group) %>%
        mutate(country_name = paste(country_name, "average"))
    } else {
      NULL
    }

  data <-
    indicator_data %>%
    filter(country_name == base_country |
             country_name %in% comparison_countries) %>%
    bind_rows(data_groups) %>%
    mutate_at(vars(all_of(indicator)),
              ~ round(., 3)) %>%
    mutate(alpha = ifelse(country_name == base_country, .8, .5)) %>%
    rename(Country = country_name)

  static_plot <-
    ggplot(data,
           aes_string(x = "Year",
                      y = indicator,
                      color = "Country",
                      group = "Country",
                      alpha = "alpha")) +
    geom_point(aes(text = paste("Country:", Country, "<br>",
                                "Year:", Year, "<br>",
                                "Value:", get(indicator))),
               size = 3) +
    geom_line() +
    theme_ipsum() +
    labs(
      x = "Year",
      y = "Indicator value",
      title = paste0("<b>",indicator_name,"</b>")
    ) +
    scale_color_manual(
      name = NULL,
      values = c("#FB8500",
                 gray.colors(length(country_list)),
                 color_groups(length(groups))),
      breaks = c(base_country,
                 country_list,
                 paste(groups, "average"))
    ) +
    scale_alpha_identity() +
    theme(
      axis.text.x = element_text(angle = 90)
    )

  ggplotly(
    static_plot,
    tooltip = "text"
  ) %>%
    layout(
      legend = list(
        title = list(text = '<b>Country:</b>'),
        y = 0.5
      ),
      margin = list(l = 50, r = 50, t = 75, b = 135),
      annotations =
        list(x = 0, y = -0.2,
             text = HTML(
               paste(
                 str_wrap(
                   paste(
                     "<b>Definition:</b>",
                     def$description
                   ),
                   note_chars
                 ),
                 str_wrap(
                   paste(
                     "<b>Source:</b>",
                     def$source
                   ),
                   note_chars
                 ),
                 sep = "<br>"
               )
             ),
             showarrow = F,
             xref = 'paper',
             yref = 'paper',
             align = 'left',
             font = list(size = note_size)
        )
    ) %>%
    config(
      modeBarButtonsToRemove = plotly_remove_buttons,
      toImageButtonOptions = list(filename = paste(tolower(base_country),
                                                   "- trends",
                                                   tolower(indicator_name)))
    )

}

# Cross-country comparison #####################################################

static_bar <-
  function(data, country_list,
           base_country, comparison_countries,
           var, variable_names) {

    data <-
      data %>%
      def_quantiles(
        base_country,
        country_list,
        comparison_countries,
        vars_all,
        variable_names
      ) %>%
      filter(var_name == var)


    median <-
      data %>%
      filter(
        country_name %in% comparison_countries
      ) %>%
      group_by(var_name) %>%
      summarise(
        country_name = "Comparison group median",
        dtf = median(dtf, na.rm = TRUE)
      )

    data <-
      data %>%
      bind_rows(median) %>%
      mutate(
        color =
          case_when(
            country_name == base_country ~ 1,
            country_name %in% comparison_countries ~ 2,
            TRUE ~ 3
          ),
        order = fct_reorder(
          country_name,
          -dtf
        )
      ) %>%
      select(dtf, order, color)

    ggplot(
      data = data,
      aes(
        x = dtf,
        y = order
      )
    ) +
      geom_col(
        aes(
          fill = factor(color)
        )
      ) +
      geom_text(
        aes(
          x = dtf + .03,
          label = round(dtf, 3)
        )
      ) +
      geom_vline(
        xintercept = 1,
        linetype = "dashed",
        color = "#238823",
        size = 1
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        plot.caption = element_text(size = 8,
                                    hjust = 0),
        plot.caption.position =  "plot"
      ) +
      labs(
        y = NULL,
        x = "Closeness to Frontier",
        fill = NULL,
        title = paste0("<b>", var, "</b>")
      ) +
      scale_fill_manual(
        values = c("#EC7663", "#001f3f", "#6c757d")
      )

  }


interactive_bar <-
  function(x, var, definitions, buttons) {

    def <-
      definitions %>%
      filter(var_name == var)

    x %>%
      ggplotly(tooltip = "text") %>%
      layout(
        legend = list(
          title = list(text = '<b>Closeness to\nfrontier:</b>'),
          y = 0.5
        ),
        margin = list(t = 75, b = 150),
        annotations =
          list(x = -.1,
               y = -.3,
               text = HTML(
                 paste(
                   str_wrap(
                     paste(
                       "<b>Definition:</b>",
                       def$description
                     ),
                     note_chars
                   ),
                   str_wrap(
                     paste(
                       "<b>Source:</b>",
                       def$source
                     ),
                     note_chars
                   ),
                   sep = "<br>"
                 )
               ),
               showarrow = F,
               xref = 'paper',
               yref = 'paper',
               align = 'left',
               font = list(size = note_size)
          )
      ) %>%
      config(
        modeBarButtonsToRemove = buttons,
        toImageButtonOptions = list(
          filename = paste0(tolower(stringr::str_replace_all(var,"\\s","_")),"_map"),
          width = 1050,
          height =  675
        )
      )

  }

# Bivariate correlation #####################################################

static_scatter <-
  function(data, base_country, comparison_countries, high_group,
           y_scatter,
           variable_names, country_list) {

    y <-
      variable_names %>%
      filter(var_name == y_scatter) %>%
      select(variable) %>%
      unlist %>%
      unname

    data <-
      data %>%
      mutate(
        label = paste0(
          "Country: ", country_name, "<br>",
          y_scatter, ": ", get(y) %>% round(3), "<br>",
          "GDP per capita, PPP: ", gdp_pc_ppp_const %>% round(2)
        ),
        log = log(gdp_pc_ppp_const),
        type = case_when(
          country_name == base_country ~ "Base country",
          country_name %in% comparison_countries ~ "Comparison countries",
          TRUE ~ "Others"
        )
      ) %>%
      left_join(
        high_group, by = c("country_code")
      )

    ggplot(
      data,
      aes_string(
        x = "log",
        y = y,
        text = "label"
      )
      ) +
      geom_point(
        data = data %>% filter(group %in% high_group$group),
        size = 4,
        shape = 1,
        color = "#60C2F7"
      ) +
      geom_point(
        aes(
          color = type,
          shape = type
        ),
        size = 2
      ) +
      scale_color_manual(
        values = c("#FB8500","#001f3f","#6c757d")
      ) +
      scale_shape_manual(
        values = c(16, 16, 1)
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        plot.caption = element_text(size = 8,
                                    hjust = 0),
        plot.caption.position =  "plot"
      ) +
      labs(
        y = paste0("<b>", y_scatter,"<br>(closeness to frontier)</b>"),
        x = paste0("<b>Log GDP per capita, PPP</b>")
      )
  }

interactive_scatter <-
  function(plot,
           y_scatter,
           definitions,
           buttons) {

    y <-
      definitions %>%
      filter(var_name == y_scatter)

    x <-
      definitions %>%
      filter(variable == "gdp_pc_ppp_const")

    plot %>%
      ggplotly(tooltip = "text") %>%
      layout(
        margin = list(
          t = 50,
          b = 200
        ),
        legend = list(
          title = list(text = ''),
          y = 0.5
        ),
        annotations = list(
          x = -0.03,
          y = -0.5,
          text = HTML(
            paste(
              "<b>Definitions:</b>",
              str_wrap(
                paste0(
                  "<br>", "<em>", x$var_name, ":</em> ", x$description, " (Source: ", x$source, ")"
                ),
                note_chars
              ),
              str_wrap(
                paste0(
                  "<br>", "<em>", y$var_name, ":</em> ", y$description, " (Source: ", y$source, ")"
                ),
                note_chars
              )
            )
          ),
          showarrow = F,
          xref = 'paper',
          yref = 'paper',
          align = 'left',
          font = list(size = note_size)
        )
      ) %>%
        config(
          modeBarButtonsToRemove = buttons,
          toImageButtonOptions = list(
            filename = paste("GDP per capita x", y_scatter),
            width = 1050,
            height =  675
          )
      )
   }

annotations =
  list(
    x = -.1,
    y = -.4,
    showarrow = F,
    xref = 'paper',
    yref = 'paper',
    align = 'left',
    font = list(size = note_size)
  )
