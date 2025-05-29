
note_size <- 11
note_chars <- 200
color_groups <- colorRampPalette(c("#001f3f", "#60C2F7"))
color_countries <- colorRampPalette(c("grey20", "grey50"))


#============


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
  function(data,
           base_country,
           tab_name,
           rank,
           group_median = NULL,
           custom_df = NULL, ## New addition made by Shel in August 2023 to accomModate custom groups
           title = TRUE,
           dots = FALSE,
           note = NULL,
           threshold,
           preset_order = FALSE,
           report = FALSE) {
  
    #browser()
    

    data$var_name <- ifelse(grepl("Average", data$var_name, ignore.case = TRUE), toupper(data$var_name), data$var_name)
    
    if (threshold=="Default"){
      cutoff<-c(25,50)
      custom_levels <- c("Weak\n(bottom 25%)", "Emerging\n(25% - 50%)", "Strong\n(top 50%)")
    }else if (threshold=="Terciles")
    {
      cutoff<-c(33,66)
      custom_levels <- c("Weak\n(bottom 33%)", "Emerging\n(33% - 66%)", "Strong\n(top 34%)")
    }

    if(preset_order == TRUE){
      ## temporary
      
      data <- data %>%
        group_by(country_name) %>%
        mutate(rank_id = rank(-dtf,ties.method = "max"))
      
        base_country_df<-data%>%
          filter(country_name==base_country[1])
        
        base_country_df$status <- factor(base_country_df$status, levels = custom_levels, ordered = TRUE)
        if(rank==FALSE){
        base_country_df <- base_country_df[order(base_country_df$status,base_country_df$dtf), ]
        }else{
          base_country_df <- base_country_df[order(base_country_df$status,base_country_df$dtt), ]
        }
        

        
        unique_indicators = base_country_df %>% 
            distinct(var_name) %>% 
            pull(var_name)
        
        #Issue 283 - Remove Institutions keywork in y axis for plots
        data$var_name <- gsub("Institutions", "", data$var_name)
        unique_indicators <- gsub("Institutions","",unique_indicators)
        
        data$var_name <-
          factor(
            data$var_name,
            levels = unique_indicators,
            ordered = TRUE
          )

    }
    else{

      data <- data %>% 
        left_join(., db_variables %>% select(variable, rank_id),
          by = "variable")
      
      # ===================OVERVIEW
      if(tab_name== "Overview")
      {
        unique_indicators <-family_order%>%
          arrange(family_order) %>% 
          pull(family_name)
        
      }else{
        unique_indicators <- data %>% 
          distinct(var_name, rank_id) %>% 
          arrange(desc(rank_id)) %>% 
          pull(var_name)
        
      }
    
      #Change old name to new one for factoring
      #data$var_name[data$var_name == "Public Finance Institutions"] <- "Public Financial Management Institutions"
      
      #Issue 283 - Remove Institutions keyword in y axis for plots
      
      data$var_name <- gsub("Institutions", "", data$var_name)
      unique_indicators <- gsub("Institutions","",unique_indicators)
      
      

      data$var_name <-
        factor(
          data$var_name,
          levels = unique_indicators,
          ordered = TRUE
         )
    }
    
    
    vars <-
      data %>%
      select(var_name) %>%
      unique() %>%
      unlist()
    if (cutoff[[1]]==25){
    colors <-
      c("Weak\n(bottom 25%)" = "#D2222D",
        "Emerging\n(25% - 50%)" = "#FFBF00",
        "Strong\n(top 50%)" = "#238823"
      )}else if (cutoff[[1]]==33){
        colors <-c(
        "Weak\n(bottom 33%)" = "#D2222D",
        "Emerging\n(33% - 66%)" = "#FFBF00",
        "Strong\n(top 34%)" = "#238823"
        )
      }
    
    if (rank == FALSE) {
      x_lab <- "Closeness to frontier"
      
      # data <-
      #   data %>%
      #   mutate(
      #     var = dtf,
      #     text = paste(
      #       "Country:", country_name, "<br>",
      #       "Closeness to frontier:", round(dtf, 3)
      #     )
      #   )
      
      data<-data %>%
        group_by(dtf) %>%
        mutate(
          var = dtf,
          text = paste(
            "Closeness to frontier:", round(dtf, 3), "<br>",
            "Country:", paste(country_name, collapse = ", ")
          )
        ) %>%
        ungroup()
      
      
    } else {
      data <-
        data %>%
        group_by(variable,nrank) %>%
        mutate(
          q_cutoff1 = cutoff[[1]]/100,
          q_cutoff2 = cutoff[[2]]/100,
          var = dtt,
          text = paste(
            "Rank:", nrank, "<br>",
            "Country:", paste(country_name, collapse = ", "), "<br>",
            "Closeness to frontier:", round(dtf, 3)
          )
        ) %>%
        ungroup()
      
      x_lab <- "Rank"
    }
    
    if(report==FALSE){
      aspect_ratio = 1.6/1
    }else{
      aspect_ratio = 1
    }
   
    #====================
   
    plot <-
      ggplot() +
        geom_segment(
          data = data,
          aes(
            y = var_name,
            yend = var_name,
            x = 0,
            xend = q_cutoff1
            
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
            x = q_cutoff1,
            xend = q_cutoff2
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
            x = q_cutoff2,
            xend = 1
          ),
          color = "#8ec18e",
          size = 2,
          alpha = .3
        )+ 
        theme_minimal() +
        theme(
          #aspect.ratio = aspect_ratio,
          legend.position = "top",
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(color = "black"),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 11),
          legend.box = "vertical",
          plot.caption = element_text(size = 8, hjust = 0),
          plot.caption.position =  "plot"
        ) +
        labs(
          y = '',
          x = x_lab,
          fill = NULL,
          shape = NULL,
          caption = note
        ) +
      scale_fill_manual(
        values = colors
      )
    
        
    if(report==FALSE){
      plot <-plot+

        scale_y_discrete(labels = function(x) str_wrap(x, width = 40))
        scale_y_discrete(labels = function(x) str_wrap(x, width = 35))
    }      
        

    
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
            y = var_name,
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
        ctf_long %>%
        filter(
          var_name %in% vars,
          country_name %in% group_median
        ) %>%
        select(
          var_name,
          value,
          country_name
        )
      

      ## ------------------------------------------------------------------------------------
      ## This is how custom median groups are calculated, only if custom_df exists and is not null

      if(!is.null(custom_df)){
     
      ## If any of the benchmark medians is a custom group
      if(any(group_median %in% custom_df$Grp)){

        ## create a place holder that will hold the medians for all the groups
        # custom_grp_median_data <- list()
        
        ## create a vector of these groups
        selected_custom_grps <- unique(custom_df$Grp)

        custom_grp_median_data_func <- function(selected_custom_grp){
          
          custom_df_per_group <- custom_df %>% 
            filter(Grp == selected_custom_grp)
          
          
          ## calculate medians for each group
          custom_grp_median_data <-
            ctf_long %>%
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
              var_name
            ) %>%
            summarise(value = median(value, na.rm = TRUE)) %>%
            ungroup
        }
        
        custom_grp_median_data_df <- purrr::map_df(selected_custom_grps, custom_grp_median_data_func)
        
        
        median_data <- median_data %>%
                         bind_rows(custom_grp_median_data_df)
      }
 
      } 

      ## ------------------------------------------------------------------------------------
      
      if ("Comparison countries" %in% group_median) {

        countries <-
          ctf_long %>%
          filter(
            var_name %in% vars,
            country_name %in% data$country_name,
            !(country_name %in% unlist(base_country))
          ) %>%
          mutate(
            country_name = "Comparison countries",
            group = NA
          ) %>%
          unique %>%
          group_by(
            country_name,
            var_name
          ) %>%
          summarise(value = median(value, na.rm = TRUE)) %>%
          ungroup

        median_data <-
          median_data %>%
          bind_rows(countries)
      }
      # Custom Group Point needs to be excluded in the plot . Issue #294 
      
      # plot <-
      #   plot +
      #   suppressWarnings(geom_point(
      #     data = median_data,
      #     aes(
      #       y = var_name,
      #       x = value,
      #       shape = country_name,
      #       text = paste(
      #         "Group:", country_name,"<br>",
      #         "Median closeness to frontier:", round(value, 3)
      #       )
      #     ),
      #     alpha = .5,
      #     color = "black",
      #     fill = "white",
      #     size = 3
      #   )) +
      #   scale_shape_manual(
      #     values = 22:25 #,
      #     #lab = NULL
      #   )
    }
    if (length(base_country)==1){
      #Alex 
      
      plot <-
        plot +
        suppressWarnings(geom_point(
          data = data %>% filter(country_name %in% base_country),
          aes(
            y = var_name,
            x = var,
            fill = status ,
            text = text
          ),
          shape = 21,
          size = 3,
          color = "gray0",
          show.legend = TRUE
        ))
    }else {
      
      if(report==TRUE){
        
        data$status <- factor(data$status, levels = custom_levels, ordered = TRUE)
        
        
        plot <-
          plot +
          suppressWarnings(geom_point(
            data = data %>% filter(country_name %in% base_country),
            aes(
              y = var_name,
              x = var,
              shape = country_name,
              fill = status ,
              text = text
            ),
            size = 3,
            color = "gray0",
            show.legend = TRUE
          ))+
          guides(fill=guide_legend(override.aes=list(shape=21)))+
          scale_shape_manual(values = 21:25)+
          guides(colour = guide_legend(order = 1), 
                 shape = guide_legend(order = 2))
      }else{
        plot <-
          plot +
          suppressWarnings(geom_point(
            data = data %>% filter(country_name %in% base_country),
            aes(
              y = var_name,
              x = var,
              shape = country_name,
              fill = status ,
              text = text
            ),
            size = 3,
            color = "gray0",
            show.legend = TRUE
          ))+scale_shape_manual(values = 21:25)

      }
      
      
        #guides(fill=guide_legend(override.aes=list(shape=21)))#+
        #scale_shape_manual(values = 21:25)      
        # scale_fill_manual(values= c("Weak\n(bottom 25%)" = "#D2222D",
        #                            "Emerging\n(25% - 50%)" = "#FFBF00",
        #                            "Strong\n(top 50%)" = "#238823"))+
        # guides(fill=guide_legend(override.aes=list(shape=21)))

    }
    
    

    
    
    
    return(plot)

  }

# Dynamic benchmark static plot ##############################################################

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
    threshold,
    preset_order = FALSE) {
    
    
    if (threshold=="Default"){
      cutoff<-c(25,50)
      custom_levels <- c("Weak\n(bottom 25%)", "Emerging\n(25% - 50%)", "Strong\n(top 50%)")
    }else if (threshold=="Terciles")
    {
      cutoff<-c(33,66)
      custom_levels <- c("Weak\n(bottom 33%)", "Emerging\n(33% - 66%)", "Strong\n(top 34%)")
    }

    
    if(preset_order == TRUE){
      
      ## temporary placeholder
      data$var_name <-
        factor(
          data$var_name,
          levels = sort(unique(data$var_name),
            decreasing = TRUE),
          ordered = TRUE
        )
      
    }else{

      data$var_name <-
        factor(
          data$var_name,
          levels = sort(unique(data$var_name),
            decreasing = TRUE),
          ordered = TRUE
        )
    }
    
    
    data <- data %>% 
      rowwise() %>% 
      mutate(var_name2 = paste(var_name, year, sep = " : ")) %>% 
      arrange(var_name2) 
    
    base_country_vars <-  data %>% 
      filter(country_name %in% base_country ) %>% 
      distinct(var_name2) %>% 
      pull()
    
    data <- data %>% 
      filter(var_name2 %in% base_country_vars) %>% 
      ## if we only have one year worth of data for a particular indicator, drop it
      group_by(var_name) %>% 
      mutate(counter = length(unique(year))) %>% 
      filter(counter > 1) %>% 
      select(-counter) %>% 
      ungroup()
    
    
    ctf_long_dyn <- ctf_long_dyn %>% 
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
            "Strong\n(top 34%)" = "#238823"
          )
        }
    
    if (rank == FALSE) {
      y_lab <- "Closeness to frontier"
      
      data <-
        data %>%
        mutate(
          var = dtf,
          text = paste(
            "Country:", country_name, "<br>",
            "Year: ", year, "<br>",
            "Closeness to frontier:", round(dtf, 3)
          )
        )
      
      
      
    } else {
      data <-
        data %>%
        mutate(
          q_cutoff1 = cutoff[[1]]/100,
          q_cutoff2 = cutoff[[2]]/100,
          var = dtt,
          text = paste(
            "Country:", country_name, "<br>",
            "Year: ", year,  "<br>",
            "Closeness to frontier:", round(dtf, 3), "<br>",
            "Rank:", nrank
          )
        )
      
      y_lab <- "Rank"
    }
    
    ## calculate the delta and the new facet labels that will contain it.
      
        
      data <- data %>% 
        group_by(family_name, var_name) %>% 
        mutate(n_countries_min = length(country_name[year == min(as.numeric(year), na.rm = TRUE)]),
          n_countries_max = length(country_name[year == max(as.numeric(year), na.rm = TRUE)])
          ) %>% 
        ungroup() %>% 
        group_by(country_name, var_name) %>% 
        mutate(earliest_value_ctf = var[year == min(as.numeric(year), na.rm = TRUE)],
          latest_value_ctf = var[year == max(as.numeric(year), na.rm = TRUE)]
        )%>% 
        mutate(earliest_value_rank = nrank[year == min(as.numeric(year), na.rm = TRUE)],
          latest_value_rank = nrank[year == max(as.numeric(year), na.rm = TRUE)]
        ) %>% 
        ungroup() %>% 
        rowwise() %>% 
        mutate(delta = round((latest_value_ctf - earliest_value_ctf), 3)) %>% 
        mutate(new_labels = 
          ifelse(earliest_value_rank != latest_value_rank, 
            paste0(var_name, 
              "\n\n(Change in CTF: ", delta , ")",
              "\n(Change in Percentile Rank: ", "from ",earliest_value_rank , " out of ", 
              n_countries_min, " to ",latest_value_rank ," out of ", n_countries_max, ")"
            ), 
            paste0(var_name, 
              "\n\n(Change in CTF: ", delta , ")",
              "\n(No significant change in Percentile Rank)"
            )
          )
      ) %>% 
      ungroup()
     
    
    ## The year var should be character or factor
    data <- data %>% 
      mutate(year = as.character(year))
    
    ## Percentile segments
    plot <-
      ggplot() +
      geom_segment(
        data = data,
        aes(
          x = year,
          xend = year,
          y = 0,
          yend = q_cutoff1
        ),
        color = "#e47a81",
        size = 2,
        alpha = .1
      ) +
      geom_segment(
        data = data,
        aes(
          x = year,
          xend = year,
          y = q_cutoff1,
          yend = q_cutoff2
        ),
        color = "#ffd966",
        size = 2,
        alpha = .3
      ) +
      geom_segment(
        data = data,
        aes(
          x = year,
          xend = year,
          y = q_cutoff2,
          yend = 1
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
        y = y_lab,
        x = NULL,
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
        scale_y_continuous(
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
            x = year,
            y = var,
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
        ctf_long_dyn %>%
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
          # custom_grp_median_data <- list()
          # 
          ## create a vector of these groups
          selected_custom_grps <- unique(custom_df$Grp)
          
          custom_grp_median_data_func <- function(selected_custom_grp){
            custom_df_per_group <- custom_df %>% 
              filter(Grp == selected_custom_grp)
            
            
            ## calculate medians for each group
            custom_grp_median_data <-
              ctf_long_dyn %>%
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
                var_name
              ) %>%
              mutate(value = median(value, na.rm = TRUE)) %>%
              distinct(var_name, year, value, country_name) %>% 
              ungroup
            
            return(custom_grp_median_data)
          }
          
          custom_grp_median_data_df <- purrr::map_df(selected_custom_grps, custom_grp_median_data_func)
          
          ## and append this to median data generated for pre-determined groups
          median_data <- median_data %>%
            bind_rows(custom_grp_median_data_df)
        }
        
      } 
      
      ## ------------------------------------------------------------------------------------
      
      if ("Comparison countries" %in% group_median) {
        
        countries <-
          ctf_long_dyn %>%
          filter(
            var_name %in% vars,
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
            var_name
          ) %>%
          mutate(value = median(value, na.rm = TRUE)) %>%
          distinct(country_name, year, var_name,  value) %>% 
          ungroup
        
        median_data <-
          median_data %>%
          bind_rows(countries)
      }
      
      
      plot <-
        plot +
        suppressWarnings(geom_point(
          data = median_data %>% ungroup() %>% filter(!is.na(value)),
          aes(
            y = value,
            x = as.character(year),
            shape = country_name,
            text = paste(
              "Group:", country_name,"<br>",
              "Year: ", year,  "<br>",
              "Median closeness to frontier:", round(value, 3)
            )
          ),
          alpha = .5,
          color = "black",
          fill = "white",
          size = 2
        )) +
        scale_shape_manual(
          values = 22:25
        )
      
      
    }
    
    
    data$status <- factor(data$status, levels = custom_levels, ordered = TRUE)
    
    ## add base country
    plot <-
      plot +
      suppressWarnings(
        geom_point(
          data = data %>% filter(country_name == base_country),
          aes(
            y = var,
            x = year,
            fill = status ,
            text = text
          ),
          size = 2,
          shape = 21,
          color = "gray0"
        )) +
      geom_line(
        data = data %>% filter(country_name == base_country),
        aes(
          y = var,
          x = year,
          group = 1
        )
      )
    
    
    ## Facet the plot

    ### number of columns will depend on the number of variables  
    n_col = ifelse(length(unique(data$var_name)) <= 3, 1, 2)
      
    
    # sc = ifelse(length(unique(data$var_name)) <= 6, "free_x", "fixed")
    sc = "free"
    
    ### instead of having the var name as the titles, we want to append the delta on it.
    ### Delta was calculated at the beginning before any plot was generated
    
      plot_titles_df <- data %>% 
        filter(var_name %in% vars & country_name == base_country) %>% 
        distinct(var_name, delta, new_labels)

    
    plot_titles <- unique(plot_titles_df$new_labels)
    names(plot_titles) <- unique(plot_titles_df$var_name)
    
    
    ### create the plot
    
    plot <- plot +
      facet_wrap(~var_name, ncol = 2, 
                 labeller = labeller(var_name = plot_titles),
                 shrink = FALSE, scales = sc) +
      theme(strip.text = element_text(face = "bold", size = 8),
            panel.spacing.x = unit(1, "lines"),
            panel.spacing.y = unit(3, "lines"))
    
   ## fix facets
   # plot <- fixfacets(figure = plot, facets = names(plot_titles), domain_offset = 0.16) 
   
    return(plot)
  }


plot_notes_function <-
  function(y, z, tab_name, miss_var, plot_type, custom_df) {
    
    if(!is.null(custom_df)){
      
      custom_grp_notes <- custom_df %>% 
        group_by(Grp) %>% 
        arrange(desc(Grp)) %>% 
        mutate(note = paste0(Grp, " (", paste0(Countries, collapse = " , ") , ")")) %>% 
        distinct(note) %>% 
        pull() %>% 
        paste0(., collapse = " ; ") 
      
      
      custom_grp_notes <-
        paste(
          "<br>",
          str_wrap(custom_grp_notes, note_chars)
          
        )
    }else{
      custom_grp_notes <- ""
    }

    
    if (length(miss_var) > 0) {
      
      ## Shel added "<br>" to include line breaks in the notes
      notes <-
        paste0(
          "Notes:<br><br>",
          paste(y,collapse = ","),
          " compared to ",
          str_wrap(paste(z, collapse = ", "), note_chars),".",
          custom_grp_notes,
          "<br><br>The following indicators are not considered because the base country (or countries) has no information or because of low variance: ",
          str_wrap(paste(miss_var, collapse = ", "), note_chars),
          "."
        )
      
    }
    
    if (length(miss_var) == 0) {
      
      notes <-
        paste0(
          "Notes:<br> ",
          
          str_wrap(
            paste0(
            y,
            " compared to ",
            str_wrap(paste(z, collapse = ", "), note_chars),
            "."),
            note_chars),
          custom_grp_notes
        )
      
    }
    
    

    
    
    if (tab_name == "Overview") {
      notes <-
        paste(
          notes,
          "<br><br>Cluster-level closeness to frontier is calculated by taking the average closeness to frontier for the estimated indicators within each cluster."
        )
    }
    
    
    if(plot_type == "dynamic"){
      notes = NULL
    }

    
   return(shiny::HTML(notes)) 
    
  }


interactive_plot <-
  function(x, tab_name, buttons,  plot_type) {
    if(tab_name=='Justice Institutions' & plot_type=='dynamic'){
      plt_height = 3000
    }else if(plot_type=='dynamic') {
      plt_height = 1200
    }else if(tab_name=='Service Delivery Institutions' | tab_name=='Justice Institutions'){
      plt_height = 1200
    }
    else if(tab_name=='Overview'){
      plt_height = 900
    }else{
      plt_height = 750
    }
    
    x <- x +
      theme(
        legend.position = "top"
      )
    
    int_plot <- x %>%
      ggplotly(tooltip = "text",height = plt_height ) %>%
      layout(
        margin = list(l = 50, r = 50, t = 150, b = 150)#,

      ) %>%
      config(
        modeBarButtonsToRemove = buttons,
        toImageButtonOptions= list(filename = paste0(tolower(stringr::str_replace_all(tab_name,"\\s","_"))),
          width = 1100,
          height =  1000)
      )
    
    
    if(plot_type == "dynamic"){
      
      int_plot <- int_plot %>% 
        layout(
                    legend = list(
                     orientation = "h",xanchor = "center", x = 0.5
                    )
        )
    }
    
    ## Solution to remove ",1" that appears on the legend
    ## https://stackoverflow.com/questions/49133395/strange-formatting-of-legend-in-ggplotly-in-r
    
    for (i in 1:length(int_plot$x$data)){
      if (!is.null(int_plot$x$data[[i]]$name)){
        int_plot$x$data[[i]]$name =  gsub("^\\(","",str_split(int_plot$x$data[[i]]$name,",")[[1]][1])
      }

    }

    int_plot <- clean_plotly_legend(int_plot)
    

    
    # names_lst <- names(int_plot$x$layout)
    # names_lst <- names_lst[grep("yaxis",names_lst)]
    # if(length(names_lst)>2){
    #   nrows = ceiling(length(names_lst)/2)
    #   height_gap = (nrows-1)*0.1182804
    #   height_plt = (1-height_gap)/nrows
    #   height_start = 1
    #   for(i in seq(1,length(names_lst),1)){
    #     int_plot$x$layout[[names_lst[i]]][['domain']] <-c(max(0,height_start-height_plt),height_start)
    #     int_plot$x$layout$annotations[[i+1]][['y']] <- height_start
    #     if((i+1)%%2 ==1){
    #       height_start <-height_start-height_plt-0.1182804
    #     }
    #   }
    # }
    
    return(int_plot)
    
  }

# Maps #########################################################################

## Static map ===================================================================

static_map <-
  function(source, var, title,
           selected, base_country, comparison_countries) {

    spatial_data <-  spatial_data %>%
      st_cast("MULTIPOLYGON")
    
    
    if (source == "raw") {

      color <- paste0("value_", var)

      
      data <-
        spatial_data %>%
        mutate(
          text = paste0(
            "Latest value (",
            get(paste0("year_", var)),
            "): ",
            get(color) %>% round(3),
            "<br>",
            "Closeness to frontier (2013-2020): ",
            get(paste0("ctf_", var)) %>% round(3)
          )
        )
      
      
      plot <-
        data %>%
        ggplot() +
        geom_sf(
          aes(
            fill = as.numeric(get(color)),
            text = paste0(
              "<b>", country_name, "</b><br>",
              text
            )
          ),
          color = "black",
          size = 0.1
        ) +
        labs(title = paste0("<b>", title, "</b>")) +
        theme_void()

    } else if (source == "ctf") {
      color <- paste0("bin_", var)
      value <- paste0("value_", var)

      data <-
        spatial_data %>%
        mutate(
          text = paste0(
            "Closeness to frontier: ",
            get(paste0("ctf_", var)) %>% round(3)
          )
        )
      
      
      plot <-
        data %>%
        ggplot() +
        geom_sf(
          aes(
            fill = get(color),
            text = paste0(
              "<b>", country_name, "</b><br>",
              text
            )
          ),
          color = "black",
          size = 0.1
        ) +
        labs(title = paste0("<b>", title, "</b>")) +
        theme_void()
    }



    if (selected == "TRUE" & !is.null(base_country) & !is.null(comparison_countries)) {

      plot <-
        plot +
        geom_sf(
          data = spatial_data %>%
            filter(!country_name %in% c(base_country, comparison_countries)),
          fill = "white"
        )
    }

    if (source == "raw") {
      plot <-
        plot +
        scale_fill_gradientn(
          colours = c(
            "#D55E00",
            "#DD7C00",
            "#E69F00",
            "#579E47",
            "#009E73"
          ),
          name = "Original Scale",
          na.value = "#808080"
        )
    } else if (source == "ctf") {
      plot <-
        plot +
        scale_fill_manual(
          name = "CTF",
          values = c(
            "0.0 - 0.2" = "#D55E00",
            "0.2 - 0.4" = "#DD7C00",
            "0.4 - 0.6" = "#E69F00",
            "0.6 - 0.8" = "#579E47",
            "0.8 - 1.0" = "#009E73",
            "Not available" = "#808080"
          ),
          na.value = "#808080",
          drop = FALSE)
    }

    return(plot)
  }


## Interactive map =============================================================

interactive_map <-
  function(x, var, definitions, buttons, source) {

    def <-
      definitions %>%
      filter(variable == var)
  
    
    # if (source == "ctf") {
    #   leg_title <- "Closeness to\nfrontier"
    # }else{
    #   leg_title <-NULL
    # }

    x %>%
      ggplotly(tooltip = "text") %>%
      layout(
        # legend = list(
        #   title = list(text = paste("<b>", leg_title, "</b>")),
        #   y = 0.2
        # ),

        margin = list(t = 75, b = 200),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations =
          list(x = 0,
               y = -0.8,
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
                     "<b>Note:</b> The color illustrates the latest value of the indicator available for each country.The data presented here for CTF is obtained by taking the average of the indicator for the period 2019-2023 and for original indicator, it is latest datapoint available.",
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
                        definitions, custom_df = NULL, base_color, comp_color, groups_color) {

  
  def <-
    definitions %>%
    filter(var_name == indicator_name)

  indicator_data <-
    raw_data %>%
    select(Year, country_name, all_of(indicator))
  
  years <-
    indicator_data %>%
    filter(
      country_name == base_country,
      !is.na(get(indicator))
    ) %>%
    summarise(
      min = min(Year, na.rm = TRUE),
      max = max(Year, na.rm = TRUE)
    )
  
  indicator_data <-
    indicator_data %>%
    filter(
      Year >= years$min,
      Year <= years$max
    )

  data_groups <-
    if (!is.null(groups)) {
      
      avg_df <- country_list %>%
        filter(group %in% groups) 
      
      if(!is.null(custom_df) & any(groups %in% custom_df$Grp)){
        avg_df <- avg_df %>% 
          bind_rows(., 
            custom_df %>% 
              rename(group = Grp,
                country_name = Countries)
            ) %>% 
          select(-Category)
               
      }
      
      avg_df <- avg_df %>% 
        inner_join(indicator_data) %>%
        group_by(Year, group) %>%
        summarise(
          across(
            all_of(indicator),
            ~ mean(., na.rm = TRUE)
          )
        ) %>%
        rename(country_name = group) %>%
        mutate(country_name = paste(country_name, "average"))
      
      avg_df <- avg_df %>%
        arrange(country_name, Year)
      
    } else {
      NULL
    }

  data <-
    indicator_data %>%
    filter(country_name == base_country |
             country_name %in% comparison_countries) %>%
    bind_rows(data_groups) %>%
    mutate(
      alpha = ifelse(country_name == base_country, .8, .5),
      color = case_when(
        country_name == base_country ~ base_color,
        country_name %in% comparison_countries ~ comp_color,
        #This is how we assign country group color
        grepl("average", country_name) ~ groups_color,
         
        TRUE ~ '#000000' # Adding default black color
      ),
      legend_label = country_name
    ) %>%
    rename(Country = country_name) %>% 
    mutate(Year = as.factor(Year))

  #==================PLOTTING: TIME TRENDS
  static_plot <-
    ggplot(
      data,
      aes(
        x = Year,
        y = get(indicator),
        color = Country,
        group = Country,
        alpha = alpha)
    ) +
    geom_line(aes(y = na.approx(get(indicator)),color = Country)) +
    geom_point(
      aes(
        text = paste(
          "Country:", Country, "<br>",
          "Year:", Year, "<br>",
          "Value:", get(indicator) %>% round(3)
        )
      ),
      #Alex change 3 to 1
     size = 3
    ) +
    #scale_alpha_identity() +
    theme_ipsum() +
    labs(
      x = "Year",
      y = "Indicator value",
      title = paste0("<b>",indicator_name,"</b>")
    ) +
    scale_color_manual(
      values = setNames(data$color, data$Country),  # Map each Country to its color
      name = "Country"  # Set the legend title
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
      margin = list(l = 50, r = 150, t = 100, b = 300),
      annotations =
        list(x = 0, y = -0.5,
             text = HTML(
               paste(
                 str_wrap(
                   paste(
                     "<b>Note:</b>,Data displayed in based on the original indicators<br>",
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
  function(data,
           base_country, comparison_countries, groups,
           var, variable_names, custom_df, color_base_bar, color_comp_bar, color_groups_bar) {

    varname <-
      variable_names %>%
      filter(var_name == var) %>%
      select(variable) %>%
      unlist %>%
      unname

    data <-
      data %>%
      filter(
        country_name %in% c(base_country, comparison_countries, groups)
      )

    if ((!is.null(comparison_countries)) & (length(comparison_countries) > 1)) {
      
      median <-
        data %>%
        filter(
          country_name %in% comparison_countries
        ) %>%
        ungroup %>%
        summarise(
          across(
            all_of(varname),
            ~ median(., na.rm = TRUE)
          )
        ) %>%
        mutate(country_name = "Comparison countries median")
      
      data <-
        data %>%
        bind_rows(median)
    }
    
    if(!is.null(custom_df)){

      ## If any of the benchmark medians is a custom group
      if(any(groups %in% custom_df$Grp)){

        ## create a vector of these groups
        selected_custom_grps <- unique(custom_df$Grp)[unique(custom_df$Grp) %in% groups]

        custom_grp_median_data_func <- function(selected_custom_grp){
          custom_df_per_group <- custom_df %>%
            filter(Grp == selected_custom_grp)


          ## calculate medians for each group
          custom_grp_median_data <-
            ctf_long_dyn %>%
            filter(
              var_name %in% var,
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
              var_name
            ) %>%
            mutate(value = median(value, na.rm = TRUE)) %>%
            distinct(var_name, value, country_name) %>%
            ungroup

          return(custom_grp_median_data)
        }

        custom_grp_median_data_df <- purrr::map_df(selected_custom_grps, custom_grp_median_data_func)

        custom_grp_median_data_df <- custom_grp_median_data_df %>% 
          left_join(., variable_names %>% select(var_name, variable), by = "var_name") %>% 
          relocate(variable, .before = var_name) %>% 
          select(-var_name) %>% 
          spread(variable, value) %>% 
          mutate(country_group = 1)
        
        ## and append this to median data generated for pre-determined groups
        data <- data %>%
          bind_rows(custom_grp_median_data_df)
      }
    }
    #NEW PIPELINE START=============
    data <- data %>%
      ungroup() %>%
      mutate(
        # Initial color assignment 
        color = case_when(
          country_name == base_country ~ color_base_bar,
          country_name %in% comparison_countries ~ color_comp_bar,
          country_name %in% groups ~ color_groups_bar,
          TRUE ~ '#7a7d7b' # Default gray color for median
        ),
        country_name = fct_reorder(country_name, get(varname), min)
      ) %>%
      select(all_of(varname), country_name, color) %>%
      ungroup()
    #NEW PIPELINE END============
    #PLOTTING:
    ggplot(
      data = data,
      aes(
        x = get(varname),
        y = country_name
      )
    ) +
      geom_col(
        aes(
          fill=color
          # fill = factor(color)
        )
      ) +

      scale_fill_identity() +
      
      geom_text(
        aes(
          x = get(varname) + .03,
          label = round(get(varname), 3)
        )
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
      )} 

  


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
        margin = list(t = 75, b = 220),
        annotations =
          list(x = -.6,
               y = -.4,
               text = HTML(
                 paste(
                   
                   str_wrap(
                     paste(
                       "<b>Note:</b>,Data displayed is based on the CTF data. <br>",
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
  function(data, 
           base_country, comparison_countries, high_group,
           y_scatter, x_scatter,
           variable_names, country_list,
           linear_fit, color_base_scatter, color_comp_scatter ) {
    
    y <-
      ifelse(
        y_scatter == "Log GDP per capita, PPP",
        "log",
        variable_names %>%
          filter(var_name == y_scatter) %>%
          select(variable) %>%
          unlist %>%
          unname
      )


    x <-
      ifelse(
        x_scatter == "Log GDP per capita, PPP",
        "log",
        variable_names %>%
          filter(var_name == x_scatter) %>%
          select(variable) %>%
          unlist %>%
          unname
      )


    data <- data %>%
      mutate(
        # label = paste0(
        #   "Country: ", country_name, "<br>"
        # ),
        log = log(wdi_nygdppcapppkd),
        type = case_when(
          country_name == base_country ~ "Base country",
          country_name %in% comparison_countries ~ "Comparison countries",
          TRUE ~ "Others"
        )
       ) %>%
      left_join(
        high_group, by = "country_name"
      )

   
  ## generate a different label, one that is a combination of country, x axis and y axis variable
  
    xvar <- sym(x) # sym() enables us to use a string variable as is, as long as we wrap them in {{...}}
    yvar <- sym(y)
    

    data <- data %>% 
      dplyr::rowwise() %>% 
      mutate(label = 
               paste0(
                 "Country: ", country_name, "<br>","<br>",
                 "x: ", {{xvar}} , "<br>","<br>",
                 "y: ", {{yvar}} , "<br>", "<br>"
               )
      )
  
  
  sc_data<-data %>%
    select(country_code, country_name, income_group, region, country_group, x, y)
  
  sc_data<- sc_data %>%
    rename(
      !!x_scatter := x,  # Rename 'x' to the value in x_scatter
      !!y_scatter := y   # Rename 'y' to the value in y_scatter
    )
  
  #PLOTTING THE SCATTER PLOT
  sc_plot <-  ggplot(
      data,
      aes(
        x = {{xvar}}, ## see how xvar is defined above
        y = {{yvar}},
        text = label 
      )
      )+
      geom_point(
        data = data %>% filter(group %in% high_group$group),
        size = 4,
        shape = 1,
        color = "#60C2F7"
      )+
      geom_point(
        aes(
          color = type,
          shape = type
        ),
        size = 2
      ) +

      scale_color_manual(
        values = c(
          "Base country" = color_base_scatter,
          "Comparison countries" = color_comp_scatter,
          group_name = "#60C2F7"
        )
      ) +
      scale_shape_manual(
        values = c(
          "Base country" = 16,
          "Comparison countries" = 16,
          "Others" = 1
        )
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
        y = ifelse(
          y_scatter == "Log GDP per capita, PPP",
          "<b>Log GDP per capita, PPP</b>",
          paste0("<b>", y_scatter,"<br>(closeness to frontier)</b>")
        ),
        x = ifelse(
          x_scatter == "Log GDP per capita, PPP",
          "<b>Log GDP per capita, PPP</b>",
          paste0("<b>", x_scatter,"<br>(closeness to frontier)</b>")
        )
      ) 

  
## linear fit line
  if(linear_fit == TRUE){
    sc_plot <- sc_plot + geom_smooth(
                                   aes(
                                    x = {{xvar}},
                                    y = {{yvar}}
                                    ),
                                     method = "lm", 
                                     color = "#e94152", 
                                     formula = 'y ~ x',
                                     linewidth = 0.5,
                                     se = FALSE,
                                     inherit.aes = FALSE
                                     )

  }else
    sc_plot <- sc_plot
   
  return(list(sc_plot = sc_plot, sc_data = sc_data))
  }

interactive_scatter <-
  function(plot,
           y_scatter,
           x_scatter,
           definitions,
           high_group,
           buttons) {

    y <-
      definitions %>%
      filter(var_name == y_scatter)

    x <-
      definitions %>%
      filter(var_name == x_scatter)

    if (x_scatter == "Log GDP per capita, PPP") {
      x <- definitions %>%
        filter(variable == "wdi_nygdppcapppkd")
    }

    if (y_scatter == "Log GDP per capita, PPP") {
      y <- definitions %>%
        filter(variable == "wdi_nygdppcapppkd")
    }
    
    def_note <-
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
    
    if (nrow(high_group) > 0) {
      def_note <-
        paste0(
          "<b>Note:</b> ",
          str_wrap(
            paste(
              unique(high_group$group),
              "countries highlighted in light blue."
            ),
            note_chars
          ),
          "<br><br>",
          def_note
        )
    }

    
    plot %>%
      ggplotly(tooltip = c("text")) %>% 
      layout(
        margin = list(
          t = 50,
          b = 300
        ),
        legend = list(
          title = list(text = ''),
          y = 0.5
        ),
        annotations = list(
          x = -0.03,
          y = -0.5,
          text = HTML(def_note),
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

