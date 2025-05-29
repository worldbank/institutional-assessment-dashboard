# Static Plot for Dynamic Benchmarking
#
# This function generates a static visualization to compare country performance 
# based on a specified metric (closeness to frontier or rank) across years and groups.
# The function allows flexibility to include custom group comparisons, thresholds 
# for performance categorization, and dynamic median calculations.

static_plot_dyn <-
  function(data,
    base_country,
    tab_name,
    rank,
    group_median = NULL,
    custom_df = NULL, ## New addition made by Shel in August 2023 to accommodate custom groups
    title = TRUE,
    dots = FALSE,
    note = NULL,
    threshold) {
    
    if (threshold=="default"){
      cutoff<-c(25,50)
    }else if (threshold=="terciles")
    {
      cutoff<-c(33,66)
    }
    
    
    data$var_name <-
      factor(
        data$var_name,
        levels = sort(unique(data$var_name),
          decreasing = TRUE),
        ordered = TRUE
      )
    
    data <- data %>% 
      rowwise() %>% 
      mutate(var_name2 = paste(var_name, year, sep = " : ")) %>% 
      arrange(var_name2) 
    
   base_country_vars <-  data %>% 
     filter(country_name == base_country ) %>% 
     distinct(var_name2) %>% 
     pull()

   data <- data %>% 
             filter(var_name2 %in% base_country_vars)
    
    ctf_dyn_long <- closeness_to_frontier_dyn_long %>% 
      rowwise() %>% 
      mutate(var_name2 = paste(var_name, year, sep = " : ")) %>% 
      arrange(var_name2) %>% 
      filter(var_name2 %in% base_country_vars)

    vars <-
      data %>%
      select(var_name) %>%
      unique %>%
      unlist %>%
      unname
    
    if (cutoff[[1]]==25){
      colors <-
        c("Weak\n(bottom 25%)" = "#D2222D",
          "Emerging\n(25% - 50%)" = "#FFBF00",
          "Strong\n(top 50%)" = "#238823"
        )}else if (cutoff[[1]]==33){
          colors <-c(
            "Weak\n(bottom 33%)" = "#D2222D",
            "Emerging\n(33% - 66%)" = "#FFBF00",
            "Strong\n(top 66%)" = "#238823"
          )
        }
    
    if (rank == FALSE) {
      x_lab <- "Closeness to frontier"
      
      data <-
        data %>%
        mutate(
          var = dtf,
          text = paste(
            " Country:", country_name, "<br>",
            "Closeness to frontier:", round(dtf, 3)
          )
        )
      
    } else {
      data <-
        data %>%
        mutate(
          q25 = cutoff[[1]]/100,
          q50 = cutoff[[2]]/100,
          var = dtt,
          text = paste(
            " Country:", country_name, "<br>",
            "Year: ", year, 
            "Closeness to frontier:", round(dtf, 3), "<br>",
            "Rank:", nrank
          )
        )
      
      x_lab <- "Rank"
    }
    

    plot <-
      ggplot() +
      geom_segment(
        data = data,
        aes(
          y = var_name2,
          yend = var_name2,
          x = 0,
          xend = q25
        ),
        color = "#e47a81",
        size = 2,
        alpha = .1
      ) +
      geom_segment(
        data = data,
        aes(
          y = var_name2,
          yend = var_name2,
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
          y = var_name2,
          yend = var_name2,
          x = q50,
          xend = 1
        ),
        color = "#8ec18e",
        size = 2,
        alpha = .3
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        legend.box = "vertical",
        plot.caption = element_text(size = 8, hjust = 0),
        plot.caption.position =  "plot"
      ) +
      labs(
        y = NULL,
        x = x_lab,
        fill = NULL,
        shape = NULL,
        caption = note
      ) +
      scale_fill_manual(
        values = colors
      ) 
    
    
    
    if (rank) {
      plot <-
        plot +
        scale_x_continuous(
          breaks = c(0, 0.5, 1),
          labels = c("Worst ranked", "Middle of ranking","Top ranked")
        )
    }
    
    if (title) {
      plot <-
        plot +
        labs(title = paste0("<b>", tab_name, "</b>"))
    }
    
    if (dots) {
      plot <-
        plot +
        suppressWarnings(geom_point(
          data = data,
          aes(
            y = var_name2,
            x = var,
            text = text
          ),
          shape = 21,
          size = 2,
          color = "gray30",
          fill = "white",
          alpha = .5
        ))  
    }
    
    if (!is.null(group_median) & !rank) {
      
      median_data <-
        ctf_dyn_long %>%
        filter(
          var_name %in% vars,
          country_name %in% group_median
        ) %>%
        select(
          var_name,
          year, 
          value,
          country_name
        )
      
      
      ## ------------------------------------------------------------------------------------
      ## This is how custom median groups are calculated, only if custom_df exists and is not null

      
      if(!is.null(custom_df)){
        
        ## If any of the benchmark medians is a custom group
        if(any(group_median %in% custom_df$Grp)){
          
          ## create a place holder that will hold the medians for all the groups
          custom_grp_median_data <- list()
          
          ## create a vector of these groups
          selected_custom_grps <- unique(custom_df$Grp)
          
          ## for each custom group
          for(i in 1: length(selected_custom_grps)){
            
            ## extract its data from the custom_df data. See sample custom_df data below

            #     Category Grp           Countries
            # 1    Custom  xd             Denmark
            # 2    Custom  xd  Russian Federation
            # 3    Custom  xd              Sweden
            # 4    Custom  ts          Tajikistan
            # 5    Custom  ts            Thailand
            # 6    Custom  ts Trinidad and Tobago
            # 7    Custom  ts             Tunisia
            # 8    Custom  ts        Turkmenistan
            # 9    Custom POL          Uzbekistan
            # 10   Custom POL       Venezuela, RB
            # 11   Custom POL             Vietnam
            # 12   Custom POL         Yemen, Rep.
            
            custom_df_per_group <- custom_df %>% 
              filter(Grp == selected_custom_grps[i])
            
            
            ## calculate medians for each group
            custom_grp_median_data[[i]] <-
              ctf_dyn_long %>%
              filter(
                var_name %in% vars,
                country_name %in% custom_df_per_group$Countries ## extract countries that fall in this group
              ) %>%
              mutate(
                country_name = unique(custom_df_per_group$Grp), ## the country name will be the 
                ## name of the group.
                group = NA
              ) %>%
              unique %>%
              group_by(
                country_name,
                year,
                var_name2
              ) %>%
              mutate(value = median(value, na.rm = TRUE)) %>%
              distinct(var_name, year, value, country_name) %>% 
              ungroup
          }
          
          ## append all the group median datasets to one
          custom_grp_median_data <- bind_rows(custom_grp_median_data)
          
          ## and append this to median data generated for pre-determined groups
          median_data <- median_data %>%
            bind_rows(custom_grp_median_data)
        }
        
      } 
      
      ## ------------------------------------------------------------------------------------
      
      if ("Comparison countries" %in% group_median) {
        
        countries <-
          ctf_long %>%
          filter(
            var_name2 %in% vars,
            country_name %in% data$country_name,
            country_name != base_country
          ) %>%
          mutate(
            country_name = "Comparison countries",
            group = NA
          ) %>%
          unique %>%
          group_by(
            country_name,
            year,
            var_name2
          ) %>%
          summarise(value = median(value, na.rm = TRUE)) %>%
          ungroup
        
        median_data <-
          median_data %>%
          bind_rows(countries)
      }

      ## Generate var_name2 for the median_data
      median_data <- median_data %>% 
        rowwise() %>% 
        mutate(var_name2 = paste(var_name, year, sep = " : ")) %>% 
        arrange(var_name2)
      

      plot <-
        plot +
        suppressWarnings(geom_point(
          data = median_data %>% filter(!is.na(value)),
          aes(
            y = var_name2,
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
          size = 2
        )) +
        scale_shape_manual(
          values = 22:25 #,
          #lab = NULL
        )
    }
    
    plot <-
      plot +
      suppressWarnings(geom_point(
        data = data %>% filter(country_name == base_country),
        aes(
          y = var_name2,
          x = var,
          fill = status ,
          text = text
        ),
        size = 2,
        shape = 21,
        color = "gray0"
      ))

    ynames_df <- data.frame(ynames = sort(ggplot_build(plot)$layout$panel_params[[1]]$y$get_labels()))

    ynames_df <- ynames_df %>%
      rowwise() %>%
      mutate(varname = strsplit(ynames, split = ":")[[1]][1],
        year = strsplit(ynames, split = ":")[[1]][2]
      ) %>%
      group_by(varname) %>%
      mutate(sequence = seq_along(year)) %>%
      mutate(ynames = ifelse(sequence != max(sequence), year, ynames))



    plot <- plot +
      scale_y_discrete(labels = ynames_df$ynames)

    return(plot)
  }
