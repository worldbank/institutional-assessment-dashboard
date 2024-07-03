violin_plot <- function(data, family, group) {

  #data <- data_clusters
  #family <- "Climate Change and Environment Institutions"
  #group <- params$region
  
  data_plot <- 
    data |>
    filter(comparison_group == group) |>
    filter(family_name == family) |>
    filter(base_country == country_name) |>
    filter(!grepl("Average", var_name)) |>
    filter(!is.na(income_group))
  
  if(nrow(data_plot)>0){
    
    ind_order <- 
      data_plot |> 
      arrange(var_name) |> 
      pull(var_name) |> 
      unique()
    
    avg_dtf <- data_plot %>%
      group_by(income_group, var_name) %>%
      summarise(
        avg_dtf = mean(dtf),
        median_dtf = median(dtf)
      ) %>%
      arrange(median_dtf)
    
    data_plot <- data_plot %>%
      left_join(avg_dtf, by = c("income_group","var_name"))
    
    ggplot(
      data = data_plot,
      aes(
        x = reorder(var_name, dtf),
        y = dtf,
        fill = median_dtf,
        group = var_name
      )
    ) +
      geom_hline(linewidth = 0.75, color = id_colors$primary, yintercept = c(0, 1)) +
      geom_violin(width = 1.5, linewidth = 0.5) +
      scale_x_discrete(
        limits = levels(data_plot$var_name),
        labels = function(x) str_wrap(x, width = 35)
      ) +
      geom_point(
        size = 8,
        fill = id_colors$primary,
        color = "transparent",
        shape = 21
      ) +
      ggflags::geom_flag(
        aes(
          country = ecb
        ),
        size = 6.5
      ) +
      labs(
        y = "Closeness to frontier",
        x = ""
      ) +
      scale_fill_stepsn(
        "Median CTF",
        limits = c(0,1),
        breaks = c(0.25,0.5),
        values = scales::rescale(c(0,0.25,0.5,1)),
        colours = c(
          "#D2222D", "#FFBF00", "#238823"
        )
      ) +
      guides(
        fill = guide_colorsteps(
          barwidth = 14, barheight = .5, title.position = "top", title.hjust = .5, 
          even.steps = FALSE,
          show.limits = T
        )
      ) +
      facet_wrap(
        ~factor(
          income_group,
          levels = income_order
        ),
        ncol = length(unique(data_plot$income_group))
      ) +
      coord_flip(clip = "off") +
      theme(
        text = element_text(color = id_colors$primary, family = 'Roboto Condensed'),
        plot.title = element_text(
          size = 18,
          hjust = 0.5,
          face = 'bold',
          margin = margin(t = 0, r = 0, b = 10, l = 0)
        ),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.25, linetype = "dotted"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 18, hjust = 0.5),
        legend.title = element_text(size = 20, hjust = 0.5, margin = margin(t = 0, r = 0, b = 5, l = 0)),
        legend.background = element_rect(fill = NA),
        legend.box = "vertical",
        legend.title.position = "top",
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 18, color = id_colors$primary, face = "bold"),
        axis.ticks = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 18, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size = 16, color = id_colors$primary),
        axis.text.x = element_text(color = id_colors$primary, size = 14)
      )
  } 
  else (paste0("No data available."))

}
