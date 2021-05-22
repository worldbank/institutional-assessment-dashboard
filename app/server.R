
# Load packages ###########################################################################

  library(shiny)
  library(shinyjs)
  library(tidyverse)
  library(DT)
  library(data.table)
  library(plotly)
  library(sf)
  library(hrbrthemes)
  library(stringr)


# Inputs ################################################################################

  # Auxiliary functions -----------------------------------------------------------------

  source(file.path("auxiliary",
                   "vars-by-family.R"))

  # Function that defines quantiles based on country, comparison and variables
  source(file.path("auxiliary",
                   "fun_quantiles.R"))

  source(file.path("auxiliary",
                   "plots.R"))

  # Data sets ---------------------------------------------------------------------------


  # Indicator definitions
  definitions <-
    read_rds(file.path("data",
                       "indicator_definitions.rds"))

  all_indicators <-
    read_rds(file.path("data",
                       "list_of_indicators.rds"))

  # Closeness to frontier data
  global_data <-
    read_rds(file.path("data",
                       "country_dtf.rds"))

  family_level_data <-
    read_rds(file.path("data",
                       "dtf_family_level.rds"))

  wb_country_geom_fact <-
    read_rds(file.path("data",
                       "wb_country_geom_fact.rds"))

  st_crs(wb_country_geom_fact) <- "WGS84"

  # Metadata
  variable_names <-
    read_rds(file.path("data",
                       "variable_names.rds"))

  country_list <-
    read_rds(file.path("data",
                       "wb_country_list.rds"))



# Server ################################################################################

  server <- function(input, output, session) {


   # Handle inputs ======================================================================

    # Base country
    base_country <-
      eventReactive(
        input$select,
        input$country
      )

    # Comparison countries
    comparison_countries <-
      eventReactive(input$groups,

                    {
                      selected_groups  <- input$groups
                      selected_country <- input$country

                      # Can use character(0) to remove all choices
                      if (is.null(selected_groups)) {
                        selected <- NULL
                      } else {
                        selected <-
                          country_list %>%
                          filter(group_code %in% selected_groups) %>%
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
                    },

                    ignoreNULL = FALSE
      )


    # Triggered by comparison countries: names of countries selected and action button
    observeEvent(comparison_countries(),

                 {
                   # Can also set the label and select items
                   updatePickerInput(session,
                                     "countries",
                                     label = NULL,
                                     choices = global_data$country_name %>% unique,
                                     selected = comparison_countries()
                   )

                   toggleState(id = "select",
                               condition = length(comparison_countries()) >= 10)
                 },

                 ignoreNULL = FALSE
    )


    observeEvent(input$select,
                 {
                   toggleState(id = "report",
                               condition = input$select == 1)
                 },
                 ignoreNULL = FALSE
    )

    # Benchmark data
    data <-
      eventReactive(input$select,

                    {
                      data <-
                        global_data %>%
                        def_quantiles(
                          base_country(),
                          comparison_countries(),
                          vars_all
                        ) %>%
                        left_join(variable_names)

                    }
      )

    # Plot title according to panel selected
    plot_title <- reactive({
      input$tabsetpanel_id
    })


   # Plots =============================================================================

    # Overview
    output$overview <- renderPlotly({

      variable_names <-
        variable_names %>%
        select(family_var,
               family_name) %>%
        rename(var_name = family_name,
               variable = family_var) %>%
        unique

      data <-
        family_level_data %>%
        def_quantiles(
          base_country(),
          comparison_countries(),
          variable_names$variable
        )  %>%
        left_join(variable_names)

      data %>%
        static_plot(base_country(),plot_title()) %>%
        interactive_plot(base_country(),
                         input$groups,
                         plot_title())

    })
    # Labor
    output$Labor <- renderPlotly({
      data() %>%
        filter(variable %in% vars_lab)  %>%
        static_plot(base_country(),plot_title()) %>%
        interactive_plot(base_country(),
                         input$groups,
                         plot_title())
    })

    # Financial
    output$Financial <- renderPlotly({
      data() %>%
        filter(variable %in% vars_fin)  %>%
        static_plot(base_country(),plot_title()) %>%
        interactive_plot(base_country(),
                         input$groups,
                         plot_title())
    })

    # Legal
    output$Legal <- renderPlotly({
      data() %>%
        filter(variable %in% vars_leg)  %>%
        static_plot(base_country(),plot_title()) %>%
        interactive_plot(base_country(),
                         input$groups,
                         plot_title())
    })

    # Political
    output$Political <- renderPlotly({
      data() %>%
        filter(variable %in% vars_pol)  %>%
        static_plot(base_country(),plot_title()) %>%
        interactive_plot(base_country(),
                         input$groups,
                         plot_title())
    })

    # Social
    output$Social <- renderPlotly({
      data() %>%
        filter(variable %in% vars_social)  %>%
        static_plot(base_country(),plot_title()) %>%
        interactive_plot(base_country(),
                         input$groups,
                         plot_title())
    })

    # Business
    output$Trade <- renderPlotly({
      data() %>%
        filter(variable %in% vars_mkt)  %>%
        static_plot(base_country(),plot_title()) %>%
        interactive_plot(base_country(),
                         input$groups,
                         plot_title())
    })

    # Public sector
    output$Public <- renderPlotly({
      data() %>%
        filter(variable %in% vars_publ)  %>%
        static_plot(base_country(),plot_title()) %>%
        interactive_plot(base_country(),
                         input$groups,
                         plot_title())
    })

    # Governance of SOEs
    output$Governance <- renderPlotly({
      data() %>%
        filter(variable %in% vars_service_del)  %>%
        static_plot(base_country(),plot_title()) %>%
        interactive_plot(base_country(),
                         input$groups,
                         plot_title())
    })

    # Accountability
    output$Account <- renderPlotly({
      data() %>%
        filter(variable %in% vars_transp)  %>%
        static_plot(base_country(),plot_title()) %>%
        interactive_plot(base_country(),
                         input$groups,
                         plot_title())
    })

   # Map =======================================================================================

    output$map <-
      renderPlotly({

        if (input$vars_map != "") {

          var_selected <-
            variable_names %>%
            filter(var_name == input$vars_map) %>%
            .$variable

          map <-
            ggplot(wb_country_geom_fact) +
            geom_sf(aes(fill = get(var_selected),
                        text = paste0(WB_NAME, ": ",
                                      get(paste0(var_selected, "_value")))),
                    color="black",
                    size=0.1
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
              drop=F) +
            labs(title = input$vars_map) +
            theme_bw()

          interactive_map(map, input$vars_map)
        }

      })

   # Data table =================================================================================

    output$benchmark_datatable <-
      renderDataTable(server = FALSE, {

        vars <-
          input$vars %>%
          map(get) %>%
          unlist

        data <-
          global_data %>%
          ungroup() %>%
          select(country_name,
                 all_of(vars)) %>%
          rename(Country = country_name) %>%
          mutate(across(where(is.numeric),
                        round, 3))


        datatable(
          data %>%
            setnames(.,
                     as.character(variable_names$variable),
                     as.character(variable_names$var_name),
                     skip_absent = TRUE),
          rownames = FALSE,
          filter = 'top',
          options = list(scrollX = TRUE,
                         pageLength = 13,
                         fixedColumns = TRUE,
                         autoWidth = TRUE,
                         dom = "lBtipr"))

      })

    # Downloadable rds of selected dataset

    output$download_global_rds <-
      downloadHandler(
        filename = "data.rds",

        content = function(file) {
          write_rds(global_data,
                    file)
        }
      )

    # Downloadable csv of selected dataset

    output$download_global_csv <-
      downloadHandler(
        filename = "data.csv",

        content = function(file) {
          write_csv(global_data,
                    file)
        }
      )

    # Downloadable dta of selected dataset

    output$download_global_dta <-
      downloadHandler(
        filename = "data.dta",

        content = function(file) {
          write_dta(global_data,
                    file)
        }
      )

   # Report ================================================================================
    output$report <- downloadHandler(
      filename = "instutitional-assessment-report.docx",
      content = function(file) {

        file.copy("report.Rmd", "tempReport.Rmd", overwrite = TRUE)

        params <- list(base_country = base_country(),
                       comparison_countries = comparison_countries(),
                       data = data())

        rmarkdown::render("tempReport.Rmd",
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

   # Definitions ===========================================================================

    output$account_def <-
      renderTable(definitions[["Accountability institutions"]])

    output$business_def <-
      renderTable(definitions[["Business environment and trade institutions"]])

    output$fin_def <-
      renderTable(definitions[["Financial market institutions"]])

    output$serv_def <-
      renderTable(definitions[["Institutions for service delivery"]])

    output$labor_def <-
      renderTable(definitions[["Labor market institutions"]])

    output$legal_def <-
      renderTable(definitions[["Legal Institutions"]])

    output$political_def <-
      renderTable(definitions[["Political Institutions"]])

    output$perf_def <-
      renderTable(definitions[["Public sector performance Institutions"]])

    output$social_def <-
      renderTable(definitions[["Social Institutions"]])

    # Download csv with definitions
    output$download_indicators <-
      downloadHandler(
        filename = "Institutional assessment indicators.csv",

        content = function(file) {
          write_csv(all_indicators,
                    file)
        }
      )

  }

