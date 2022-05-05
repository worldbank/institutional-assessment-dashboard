
static_plot2 <-
  function(data, base_country,
           tab_name, 
           group_median = NULL, 
           overview = FALSE,
           title = FALSE, 
           dots = FALSE,
           note = NULL) {
    
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
      theme_minimal() +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_text(color = "black"),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 11),
            legend.box = "vertical",
            plot.caption = element_text(size = 12,
                                        hjust = 0),
            plot.caption.position =  "plot") +
      labs(
        y = NULL,
        x = "Closeness to Frontier",
        fill = NULL,
        shape = NULL,
        caption = paste0("Source: GBID developed as part of the CLAIR framework (GOV GP).\n", note)
      ) +
      scale_fill_manual(
        values = colors
      )
    
    if (title) {
      plot <-
        plot +
        labs(title = paste0(tab_name))
    }
    
    if (dots) {
      plot <-
        plot +
        geom_point(
          data = data,
          aes(
            y = var_name,
            x = dtf,
            text = paste(
              " Country:", country_name,"<br>",
              "Closeness to frontier:", round(dtf, 3)
            )
          ),
          shape = 21,
          size = 2,
          color = "gray30",
          fill = "white",
          alpha = .5
        )
    }
    
    if (!is.null(group_median)) {
      
      if (!overview) {
        median_data <- 
          ctf_long %>%
          filter(
            var_name %in% data$var_name,
            country_name %in% group_median
          )
      } else {
        
        median_data <-
          ctf_long %>%
          filter(
            family_name %in% data$var_name,
            group %in% group_median
          ) %>%
          group_by(
            group,
            family_name
          ) %>%
          summarise(
            value = mean(value, na.rm = TRUE)
          ) %>%
          rename(
            country_name = group,
            var_name = family_name
          )
        
      }
      
      plot <-
        plot +
        geom_point(
          data = median_data,
          aes(
            y = var_name,
            x = value,
            shape = country_name,
            text = paste(
              " Group:", country_name,"<br>",
              "Median closeness to frontier:", round(value, 3)
            )
          ),
          alpha = .5,
          color = "black",
          fill = "white",
          size = 3
        ) +
        scale_shape_manual(
          values = 22:25,
          lab = NULL
        )
    }
    
    plot <-
      plot +
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
      )
    
    return(plot)
    
    
  }
