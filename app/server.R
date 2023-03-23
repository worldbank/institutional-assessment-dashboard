# Server ################################################################################

  server <- function(input, output, session) {


   # Handle inputs ======================================================================
    
    ## Base country ------------------------------------------------------------
    base_country <-
      eventReactive(
        input$select,
        input$country,
        ignoreNULL = FALSE
      )

    ## Comparison countries ----------------------------------------------------

    observeEvent(
      input$groups,

      {
        selected_groups  <- input$groups
        selected_country <- input$country

        # Can use character(0) to remove all choices
        if (is.null(selected_groups)) {
          selected <- NULL
        } else {
          selected <-
            country_list %>%
            filter(group %in% selected_groups) %>%
            select(country_name) %>%
            unique

          if (!is.null(selected_country)) {
            selected <-
              selected %>%
              filter(country_name != selected_country)
          }

          selected <-
            selected %>%
            pluck(1)

        }

        updateCheckboxGroupButtons(
          session,
          "countries",
          label = NULL,
          choices = countries,
          checkIcon = list(
            yes = icon("ok",
                       lib = "glyphicon")
          ),
          selected = selected
        )
      },

      ignoreNULL = FALSE
    )

    ## Validate options -------------------------------------------------------

    output$select_button <-
      renderUI({
        if (length(input$countries) >= 10 & input$country != "") {
          actionButton(
            "select",
            "Apply selection",
            
            icon = icon("check"),
            class = "btn-success",
            width = "100%"
          )
        }
        else {
          actionButton(
            "select",
            "Select a base country and at least 10 comparison countries to apply selection",
            icon = icon("triangle-exclamation"),
            class = "btn-warning",
            width = "100%",
            shinyjs::disable('report'),
          )
        }
      })

    observeEvent(
      input$countries,
      
       {
         toggleState(
           id = "select",
           condition = length(input$countries) >= 10,
           shinyjs::disable('report'),
         )
       },

       ignoreNULL = FALSE
    )

    # Reactive objects ==============================================

    ## Benchmark data -----------------------------------------------
    vars <-
      eventReactive(
        input$select,
        {

          if (input$family == "Overview") {
            vars_all
          } else {
            variable_names %>%
              filter(family_name == input$family) %>%
              pull(variable) %>%
              unique
          }
          
        }
        
      )

    ## Comparison group note (group or countries) -------------------------
    note_compare <-
      eventReactive(
        input$select,
        {
          
          if (is.null(input$groups)) {
            return(input$countries)
          } else if (
            all(
              unique(input$countries) ==
              unique(
                country_list %>%
                filter(group %in% input$groups) %>%
                pull(country_name)
              )
            )
          ) {
            return(input$groups)
          } else {
            return(input$countries)
          }

        }
      )


    ## Indicators with low variance -------------------------------------------
    low_variance_indicators <-
      eventReactive(
        input$select,

        {
          global_data %>%
            low_variance(
              base_country(),
              country_list,
              input$countries,
              vars(),
              variable_names
            )
        }
      )

    data <-
      eventReactive(
        input$select,

        {
          global_data %>%
            def_quantiles(
              base_country(),
              country_list,
              input$countries,
              vars_all,
              variable_names
            )
        }
      )

    data_family <-
      eventReactive(
        input$select,
        {
          family_data(
            global_data,
            base_country(),
            variable_names
          ) %>%
            def_quantiles(
              base_country(),
              country_list,
              input$countries,
              vars_family,
              family_names
            )
        }
      )

    # Missing variables from base country
    na_indicators <-
      eventReactive(
        input$select,

        {

          global_data %>%
            ungroup() %>%
            filter(country_name == input$country) %>%
            select(where(is.na)) %>%
            names

        }
      )

    ## Update inputs based on benchmark selection ------------------------------
    observeEvent(
      input$select,

      {
        
        if (!is.null(input$groups)) {

          updatePickerInput(
            session,
            "groups_data",
            choices = c("All", "Comparison groups only", "None")
          )

          updatePickerInput(
            session,
            "groups_bar",
            selected = input$groups
          )

          updatePickerInput(
           session,
           "high_group",
           selected = input$groups
          )

          updatePickerInput(
           session,
           "group_trends",
           selected = input$groups
          )

        }
        
        updatePickerInput(
          session,
          "countries_data",
          choices = c("All", "Base country only", "Base + comparison countries")
        )
        
        # Create report
        toggleState(
          id = "report",
          condition = input$select,
          shinyjs::disable('report')
        )
        
        # Cross-crountry comparison selection
        updatePickerInput(
          session,
          "country_bar",
          selected = input$country
        )
        
        updateCheckboxGroupButtons(
          session,
          "countries_bar",
          selected = input$countries
        )
        
        # Bivariate correlation selection
        updatePickerInput(
          session,
          "country_scatter",
          selected = input$country
        )

        updateCheckboxGroupButtons(
          session,
          "countries_scatter",
          selected = input$countries
        )

        # Time trends
        updatePickerInput(
          session,
          "country_trends",
          selected = input$country
        )

        updateCheckboxGroupButtons(
          session,
          "countries_trends",
          selected = input$countries
        )
      },

      ignoreNULL = TRUE
    )

    ## Change variable selection in all tabs --------------------------

    observeEvent(
      input$vars_bar,

      {
        updatePickerInput(
          session,
          "y_scatter",
          selected = input$vars_bar
        )

        updatePickerInput(
          session,
          "vars_map",
          selected = input$vars_bar
        )

        updatePickerInput(
          session,
          "vars_trends",
          selected = input$vars_bar
        )

      },

      ignoreNULL = FALSE
    )

    observeEvent(
      input$y_scatter,

      {
        updatePickerInput(
          session,
          "vars_bar",
          selected = input$y_scatter
        )

        updatePickerInput(
          session,
          "vars_map",
          selected = input$y_scatter
        )

        updatePickerInput(
          session,
          "vars_trends",
          selected = input$y_scatter
        )

      },

      ignoreNULL = FALSE
    )

    observeEvent(
      input$vars_map,

      {
        updatePickerInput(
          session,
          "vars_bar",
          selected = input$vars_map
        )

        updatePickerInput(
          session,
          "y_scatter",
          selected = input$vars_map
        )

        updatePickerInput(
          session,
          "vars_trends",
          selected = input$vars_map
        )

      },

      ignoreNULL = FALSE
    )

    observeEvent(
      input$vars_trends,

      {
        updatePickerInput(
          session,
          "vars_bar",
          selected = input$vars_trends
        )

        updatePickerInput(
          session,
          "y_scatter",
          selected = input$vars_trends
        )

        updatePickerInput(
          session,
          "vars_map",
          selected = input$vars_trends
        )

      },

      ignoreNULL = FALSE
    )

    ## Make sure only valid groups are chosen ----------------------------------

    observeEvent(
      input$country,

      {
        valid_vars <-
          ctf_long %>%
          filter(
            country_name == input$country,
            !is.na(value)
          ) %>%
          select(family_name) %>%
          unique %>%
          unlist %>%
          unname

        updatePickerInput(
          session,
          "family",
          choices = c(
            "Overview",
            intersect(names(variable_list), valid_vars)
          )
        )
      },

      ignoreNULL = FALSE
    )

    ## Median data ------------------------------------------------------------

    data_family_median <-
      eventReactive(
        input$select,

        {
          family_data(
            global_data,
            base_country(),
            variable_names
          )
        }
      )

   # Benchmark plot ============================================================

    output$plot <-
      renderPlotly({

        if (length(input$countries) >= 10) {
          input$select

          isolate(

            if (input$family == "Overview") {

              missing_variables <-
                global_data %>%
                missing_var(
                  base_country(),
                  country_list,
                  input$countries,
                  vars_all,
                  variable_names
                )

              low_variance_variables <-
                low_variance_indicators() %>%
                data.frame() %>%
                rename("variable" = ".") %>%
                left_join(variable_names %>% select(variable,var_name), by = "variable") %>%
                .$var_name

              missing_variables <- c(missing_variables, low_variance_variables)

              data_family()  %>%
                static_plot(
                  base_country(),
                  input$family,
                  input$rank,
                  dots = input$benchmark_dots,
                  group_median = input$benchmark_median
                ) %>%
                interactive_plot(
                  base_country(),
                  note_compare(),
                  input$family,
                  plotly_remove_buttons,
                  missing_variables
                )

            } else {

              missing_variables <-
                global_data %>%
                missing_var(
                  base_country(),
                  country_list,
                  input$countries,
                  vars(),
                  variable_names
                )

              low_variance_variables <-
                low_variance_indicators() %>%
                data.frame() %>%
                rename("variable"=".") %>%
                left_join(variable_names %>% select(variable,var_name), by = "variable") %>%
                .$var_name

              missing_variables <- c(missing_variables,low_variance_variables)

              data() %>%
                filter(variable %in% vars()) %>%
                static_plot(
                  base_country(),
                  input$family,
                  input$rank,
                  dots = input$benchmark_dots,
                  group_median = input$benchmark_median
                ) %>%
                interactive_plot(
                  base_country(),
                  note_compare(),
                  input$family,
                  plotly_remove_buttons,
                  missing_variables
                )
            }
          )
        }

      })

 # Bar plot ==================================================================

    observeEvent(
      input$vars_bar,
      
      {
        var <-
          db_variables %>%
          filter(var_name == input$vars_bar) %>%
          pull(variable)
        
        valid <-
          global_data %>%
          filter(
            !is.na(get(var))
          ) %>%
          select(country_name) %>%
          unique %>%
          unlist %>%
          unname
        
        bar_countries <-
          intersect(valid, countries)

        updatePickerInput(
          session,
          "country_bar",
          choices = c(
            "",
            bar_countries
          )
        )

        updateCheckboxGroupButtons(
          session,
          "countries_bar",
          choices = bar_countries,
          checkIcon = list(
            yes = icon(
              "ok",
              lib = "glyphicon"
            )
          )
        )
      },
      
      ignoreNULL = TRUE
    )
    
    
     output$bar_plot <-
      renderPlotly(
        {
          static_bar(
            global_data,
            input$country_bar,
            input$countries_bar,
            input$groups_bar,
            input$vars_bar,
            variable_names
          ) %>%
            interactive_bar(
              input$vars_bar,
              db_variables,
              plotly_remove_buttons
            )

        }
      )

    # Scatter plot ============================================================

    high_group <- reactive({
      country_list %>%
        filter(group %in% input$high_group) %>%
        select(group, country_code)
    })

    output$scatter_plot <-
      renderPlotly({
        static_scatter(
          global_data,
          input$country_scatter,
          input$countries_scatter,
          high_group(),
          input$y_scatter,
          input$x_scatter,
          variable_names,
          country_list
        ) %>%
          interactive_scatter(
            input$y_scatter,
            input$x_scatter,
            db_variables,
            high_group(),
            plotly_remove_buttons
          )
      })

   # Map =======================================================================

    output$map <-
      renderPlotly({

        if (input$vars_map != "") {

          var_selected <-
            variable_names %>%
            filter(var_name == input$vars_map) %>%
            pull(variable)

          static_map(
            input$value_map,
            var_selected,
            input$vars_map,
            input$countries_map,
            base_country(),
            input$countries
          ) %>%
          interactive_map(
            var_selected,
            db_variables,
            plotly_remove_buttons,
            input$value_map
          )
        }

      })

   # Trends plot ===============================================================

    observeEvent(
      input$vars_trends,
      
      {
        
        if (input$vars_trends != "") {
          var <-
            db_variables %>%
            filter(var_name == input$vars_trends) %>%
            pull(variable)
          
          valid <-
            global_data %>%
            filter(
              !is.na(get(var))
            ) %>%
            select(country_name) %>%
            unique %>%
            unlist %>%
            unname
          
          valid_countries <-
            intersect(valid, countries)
          
          updatePickerInput(
            session,
            "country_trends",
            choices = c(
              "",
              valid_countries
            )
          )
          
          updateCheckboxGroupButtons(
            session,
            "countries_trends",
            choices = valid_countries,
            checkIcon = list(
              yes = icon(
                "ok",
                lib = "glyphicon"
              )
            )
          )
        }
        
      },
      
      ignoreNULL = TRUE
    )
  

    output$time_series <-
      renderPlotly({

        if (input$vars_trends != "") {
          
          var <-
            db_variables %>%
            filter(var_name == input$vars_trends) %>%
            pull(variable)

          trends_plot(
            raw_data,
            var,
            input$vars_trends,
            input$country_trends,
            input$countries_trends,
            country_list,
            input$group_trends,
            db_variables
          )
        }

      })

   # Data table ================================================================
    
    browse_data <-
      reactive({
        
        data <-
          if (input$data_source == "Closeness to frontier") {
            global_data
          } else {
            raw_data
          }
        
        groups <-
          if (input$groups_data == "All") {
            all_groups
          } else if (input$groups_data == "Comparison groups only" & input$groups != "") {
            input$groups
          } else {
            ""
          }
        
        selected_countries <-
          if (input$countries_data == "All") {
            countries
          } else if (input$countries_data == "Base country only") {
            input$country
          } else if (input$countries_data == "Base + comparison countries"){
            c(input$country, input$countries)
          }
        
        vars <-
          variable_names %>%
          filter(
            family_name %in% input$vars,
            var_level == "indicator"
          ) %>%
          select(variable) %>%
          unlist
        
        if (input$data_source != "Closeness to frontier") {
          vars_table <- c("Country", "Year", vars)
        } else {
          vars_table <- c("Country", 'group',vars)
        }
        
        vars_table <- unname(vars_table)
        
        data <-
          data %>%
          rename(Country = country_name) %>%
          filter(
            Country %in% c(selected_countries, groups)
          ) %>%
          ungroup() %>%
          mutate(
            across(
              where(is.numeric),
              round, 3
            )
          ) %>%
          select(all_of(vars_table))
        
        data <-
          data %>%
          setnames(
            .,
            as.character(variable_names$variable),
            as.character(variable_names$var_name),
            skip_absent = TRUE
          )
        
        if (input$data_value == "Rank") {
          data <-
            data %>%
            mutate(
              across(
                2:ncol(.),
                ~ dense_rank(desc(.))
              )
            )
        }
        
        return(data)
        
      })
    
    output$benchmark_datatable <-
      renderDataTable(
        server = FALSE,
        datatable(
          browse_data(),
          rownames = FALSE,
          filter = 'none',
          options = list(
            scrollX = TRUE,
            pageLength = 13,
            autoWidth = TRUE,
            dom = "lftipr"
          )
        )
      )

      # Downloadable rds of selected dataset
      output$download_global_rds <-
        
        downloadHandler(
          filename = function(){paste0(input$data_source,"-",input$data_value,"-gbid-data.rds")},

          content = function(file) {
            write_rds(
              browse_data() %>%
                setnames(
                  .,
                  as.character(variable_names$var_name),
                  as.character(variable_names$variable),
                  skip_absent = TRUE
                ),
              file
            )
          }
        )

      # Downloadable csv of selected dataset
      output$download_global_csv <-
        downloadHandler(
          filename = function(){paste0(input$data_source,"-",input$data_value,"-gbid-data.csv")},

          content = function(file) {
            write_csv(
              browse_data(),
              file,
              na = ""
            )
          }
        )

      # Downloadable dta of selected dataset
      output$download_global_dta <-
        downloadHandler(
          filename = function(){paste0(input$data_source,"-",input$data_value,"-gbid-data.dta")},

          content = function(file) {
            write_dta(
              browse_data() %>%
                setnames(
                  .,
                  as.character(variable_names$var_name),
                  as.character(variable_names$variable),
                  skip_absent = TRUE
                ),
              file
            )
          }
        )


   # Report ================================================================================

    output$report <- downloadHandler(

      filename =
        reactive(
          paste0(
            "CLIAR-benchmarking-",
            base_country(),
            ".docx"
          )
        )
      ,

      content = function(file) {

        show_modal_spinner(
          color = "#17a2b8",
          text = "Compiling report",
        )

        on.exit(remove_modal_spinner())

        tmp_dir <- tempdir()

        tempReport <- file.path(tmp_dir, "report.Rmd")

        file.copy("www/", tmp_dir, recursive = TRUE)
        file.copy("report.Rmd", tempReport, overwrite = TRUE)


        params <-
          list(
            base_country = base_country(),
            comparison_countries = input$countries,
            data = data(),
            family_data = data_family(),
            rank = input$rank,
            definitions = definitions,
            variable_names = variable_names
          )

        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv()),
          knit_root_dir = getwd()
        )
      }
    )

   # Definitions ===========================================================================

    output$definition <-
      renderTable({

        variables <-
          db_variables %>%
          filter(var_level == "indicator")

        if (input$family != "Overview") {
          variables <-
            variables %>%
            filter(family_name == input$family)
        }

        variables %>%
          select(
            Indicator = var_name,
            Family = family_name,
            Description = description,
            Source = source
          )

      })

      output$definition_bar <-
        renderTable({

          variables <-
            db_variables %>%
            filter(
              var_name == input$vars_bar
            ) %>%
            select(
              Indicator = var_name,
              Family = family_name,
              Description = description,
              Source = source
            )

        })


    # Download csv with definitions
    output$download_indicators <-
      downloadHandler(
        filename = "Institutional assessment indicators.csv",

        content = function(file) {
          write_csv(
            db_variables %>%
              select(
                indicator = var_name,
                family = family_name,
                description,
                source
              ),
              file,
              na = ""
          )
        }
      )

  # Full methodology --------------------------------------------------------
    output$download_metho <-
      downloadHandler(
        filename = "CLIAR-Methodological-Note_20220403.pdf",

        content = function(file) {
          file.copy("www/CLIAR-Methodological-Note_20220403.pdf", file)
        }

      )

  }

