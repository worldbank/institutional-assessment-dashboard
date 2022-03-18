
# Load packages ###########################################################################

  library(shiny)
  library(tidyverse)
  library(haven)
  library(DT)
  library(data.table)
  library(plotly)
  library(sf)
  library(hrbrthemes)
  library(stringr)
  library(grDevices)
  library(shinyjs)


# Inputs ################################################################################

  ## Auxiliary functions -----------------------------------------------------------------

  source(file.path("auxiliary",
                   "vars-control.R"))

  # Load data with min max year for info available
  period_info_available <-
    read_rds(
      file.path(
        "data",
        "period_info_available.rds")
    )

  period_info_by_variable <-
    read_rds(
      file.path(
        "data",
        "period_info_by_variable.rds"
      )
    )

  # Load data control
  db_variables <-
    db_variables %>%
    filter(variable %in% vars_all | var_level == "family") %>%
    mutate(
      description = str_replace_all(description, "[[:punct:]]", " ")
    ) %>%
    left_join(
      period_info_by_variable,
      by = "variable"
    ) %>%
    mutate(
      range =
        ifelse(
          var_level == "family",
          NA,
          paste0(min, "-", max)
        )
    )

  # Function that defines quantiles based on country, comparison and variables
  source(file.path("auxiliary",
                   "fun_quantiles.R"))

  source(file.path("auxiliary",
                   "fun_family_data.R"))

  # Create benchmark graphs
  source(file.path("auxiliary",
                   "plots.R"))

  ## Data sets ---------------------------------------------------------------------------

  # Closeness to frontier data
  global_data <-
    read_rds(file.path("data",
                       "country_dtf.rds")) %>%
    mutate(
      country_name = country_name %>%
      str_replace_all("Macedonia", "North Macedonia") %>%
      str_replace_all("Swaziland", "Eswatini")
    )

  wb_country_geom_fact <-
    read_rds(file.path("data",
                       "wb_country_geom_fact.rds"))

  st_crs(wb_country_geom_fact) <- "WGS84"

  # Raw data
  raw_data <-
    read_rds(file.path("data",
                       "raw_data.rds")) %>%
    filter(year >= 1990,
           rowSums(!is.na(.)) > 3) %>%
    rename(Year = year)

  # Metadata
  variable_names <-
    db_variables %>%
    select(
      variable,
      var_level,
      var_name,
      family_var,
      family_name
    )

  country_list <-
    read_rds(file.path("data",
                       "wb_country_list.rds"))

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
          choices = global_data$country_name %>% unique %>% sort,
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
    observeEvent(
      input$countries,

       {
         toggleState(
           id = "select",
           condition = length(input$countries) >= 10
         )
       },

       ignoreNULL = FALSE
    )


    observeEvent(
      input$select,

      {
        toggleState(
          id = "report",
          condition = input$select
        )
      },

      ignoreNULL = FALSE
    )

    # Reactive objects ==============================================

    ## Benchmark data -----------------------------------------------
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
              family_names,
              variable_names
            )
        }
      )

    ## Browse data -------------------------------------------------------------
    browse_data <-
      eventReactive(
        input$data,

        {
          selected_data <- input$data

          if (selected_data == "Closeness to frontier") {
            return(global_data)
          }

          if (selected_data == "Compiled indicators") {
            return(raw_data)
          }
        }
      )

   # Benchmark plot ============================================================

    output$plot <-
      renderPlotly({

        input$select

        isolate(

          if (input$family == "Overview") {

            data_family() %>%
              static_plot(base_country(),
                          input$family) %>%
              interactive_plot(base_country(),
                               input$groups,
                               input$family,
                               plotly_remove_buttons)
          } else {

            vars <-
              # case_when()
              if (input$family == "Labor market institutions") {vars_lab} else
                if (input$family == "Financial market institutions") {vars_fin} else
                  if (input$family == "Legal institutions") {vars_leg} else
                    if (input$family == "Political institutions") {vars_pol} else
                      if (input$family == "Social institutions") {vars_social} else
                        if (input$family == "Business environment and trade institutions") {vars_mkt} else
                          if (input$family == "Public sector performance institutions") {vars_publ} else
                            if (input$family == "SOE Corporate Governance") {vars_service_del} else
                              if (input$family == "Anti-Corruption, Transparency and Accountability institutions") {vars_transp}

            data() %>%
              filter(variable %in% vars) %>%
              static_plot(base_country(),
                          input$family) %>%
              interactive_plot(base_country(),
                               input$groups,
                               input$family,
                               plotly_remove_buttons)
          }
        )
      })

   # Bar plot ==================================================================

    output$bar_plot <-

      renderPlotly(
        {
          static_bar(
            global_data,
            country_list,
            input$country_bar,
            input$countries_bar,
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

    output$scatter_plot <-
      renderPlotly({
        static_scatter(
          global_data,
          input$x_scatter, input$y_scatter,
          variable_names
        ) %>%
          interactive_scatter(
            input$x_scatter, input$y_scatter,
            db_variables,
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
            .$variable

          latest_year <- period_info_available %>%
            filter(variable == var_selected)

          static_map(wb_country_geom_fact,
                     var_selected,
                     latest_year,
                     input$vars_map) %>%
          interactive_map(input$vars_map,
                          db_variables,
                          plotly_remove_buttons)
        }

      })

   # Trends plot ===============================================================

    var_trends <-
      eventReactive(
        input$indicator_trends,

        {
          var_selected <-
            variable_names %>%
            filter(var_name == input$indicator_trends) %>%
            .$variable
        }
      )

    observeEvent(
      input$indicator_trends,

      {
        valid_countries <-
          raw_data %>%
          filter(!is.na(get(var_trends()))) %>%
          select(country_name) %>%
          unique %>%
          arrange(country_name) %>%
          unlist %>%
          unname

        last_country <-
          input$country_trends

        updatePickerInput(
          session,
          "country_trends",
          choices = valid_countries,
          selected = last_country
        )

        last_countries <-
          input$countries_trends

        updatePickerInput(
          session,
          "countries_trends",
          choices = valid_countries,
          selected = last_countries
        )
      },

      ignoreNULL = FALSE
    )

    output$time_series <-
      renderPlotly({

        if (input$indicator_trends != "") {

          trends_plot(
            raw_data,
            var_trends(),
            input$indicator_trends,
            input$country_trends,
            input$countries_trends,
            country_list,
            input$group_trends,
            db_variables
          )
        }

      })

   # Data table ================================================================

    output$benchmark_datatable <-
      renderDataTable(
        server = FALSE,
        {

          vars <-
            variable_names %>%
            filter(family_name %in% input$vars,
                   var_level == "indicator") %>%
            .$variable %>%
            unlist

          if (input$data == "Compiled indicators") {
            vars_table <- c("Country", "Year", vars)
          } else {
            vars_table <- c("Country", vars)
          }

          data <-
            browse_data() %>%
            rename(Country = country_name) %>%
            ungroup() %>%
            select(all_of(vars_table)) %>%
            mutate(across(where(is.numeric),
                          round, 3))

          if(input$show_rank){

            data <-
              data %>%
              mutate_at(vars(all_of(vars)),
                        ~ dense_rank(desc(.)
                )
              )

        }

        datatable(
          data %>%
            setnames(.,
                     as.character(variable_names$variable),
                     as.character(variable_names$var_name),
                     skip_absent = TRUE),
          rownames = FALSE,
          filter = 'none',
          options = list(scrollX = TRUE,
                         pageLength = 10,
                         autoWidth = TRUE,
                         dom = "lftipr"))

      })

        # Downloadable rds of selected dataset

        output$download_global_rds <-
          downloadHandler(
            filename = "data.rds",

            content = function(file) {
              write_rds(browse_data(),
                        file)
            }
          )

        # Downloadable csv of selected dataset

        output$download_global_csv <-
          downloadHandler(
            filename = "data.csv",

            content = function(file) {
              write_csv(browse_data(),
                        file,
                        na = "")
            }
          )

        # Downloadable dta of selected dataset

        output$download_global_dta <-
          downloadHandler(
            filename = "data.dta",

            content = function(file) {
              write_dta(browse_data(),
                        file)
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

        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        params <-
          list(
            base_country = base_country(),
            comparison_countries = input$countries,
            data = data(),
            family_data = data_family(),
            definitions = db_variables
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
            Source = source,
            Period = range
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
              Source = source,
              Period = range
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
                source,
                range
              ),
              file,
              na = ""
          )
        }
      )

  }

