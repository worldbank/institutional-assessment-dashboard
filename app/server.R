
# Load packages ################################################################################

  library(shinydashboard)
  library(shiny)
  library(shinyjs)
  library(tidyverse)
  library(hrbrthemes)
  library(DT)
  library(plotly)
  library(sf)
  library(BAMMtools)
  library(viridis)


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

  definitions <-
    read_rds(file.path("data",
                       "indicator_definitions.rds"))

  global_data <-
    read_rds(file.path("data",
                       "country_dtf.rds"))

  family_level_data <-
    read_rds(file.path("data",
                       "dtf_family_level.rds"))

  variable_names <-
    read_rds(file.path("data",
                       "variable_names.rds"))

  country_list <-
    read_rds(file.path("data",
                       "wb_country_list.rds"))

  wb_country_geom <-
    read_rds(file.path("data",
                       "wb_country_geom.rds"))

  pal <-
    colorFactor(
      palette = c("#D55E00",
                  "#DD7C00",
                  "#E69F00",
                  "#579E47",
                  "#009E73"),
      levels = c("0.0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1.0"),
      na.color = "#808080"
    )

# Server ################################################################################

  server <- function(input, output, session) {


   # Handle inputs ----------------------------------------------------------------------

    # Base country
    base_country <-
      eventReactive(
        input$select,
        input$country
      )

    # Comparison countries
    comparison_countries <-
      eventReactive(
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
    observeEvent(
      comparison_countries(),

      {
        # Can also set the label and select items
        updateCheckboxGroupInput(session,
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

    # Benchmark data
    data <-
      eventReactive(
        input$select,

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

   # Plots ---------------------------------------------------------------

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
          static_plot(base_country()) %>%
          interactive_plot(base_country(),
                           comparison_countries())

    })

    # Labor
    output$Labor <- renderPlotly({
      data() %>%
        filter(variable %in% vars_lab)  %>%
        static_plot(base_country()) %>%
        interactive_plot(base_country(),
                         comparison_countries())
    })

    # Financial
    output$Financial <- renderPlotly({
      data() %>%
        filter(variable %in% vars_fin)  %>%
        static_plot(base_country()) %>%
        interactive_plot(base_country(),
                         comparison_countries())
    })

    # Legal
    output$Legal <- renderPlotly({
      data() %>%
        filter(variable %in% vars_leg)  %>%
        static_plot(base_country()) %>%
        interactive_plot(base_country(),
                         comparison_countries())
    })

    # Political
    output$Political <- renderPlotly({
      data() %>%
        filter(variable %in% vars_pol)  %>%
        static_plot(base_country()) %>%
        interactive_plot(base_country(),
                         comparison_countries())
    })

    # Social
    output$Social <- renderPlotly({
      data() %>%
        filter(variable %in% vars_social)  %>%
        static_plot(base_country()) %>%
        interactive_plot(base_country(),
                         comparison_countries())
    })

    # Business
    output$Trade <- renderPlotly({
      data() %>%
        filter(variable %in% vars_mkt)  %>%
        static_plot(base_country()) %>%
        interactive_plot(base_country(),
                         comparison_countries())
    })

    # Public sector
    output$Public <- renderPlotly({
      data() %>%
        filter(variable %in% vars_publ)  %>%
        static_plot(base_country()) %>%
        interactive_plot(base_country(),
                         comparison_countries())
    })

    # Governance of SOEs
    output$Governance <- renderPlotly({
      data() %>%
        filter(variable %in% vars_service_del)  %>%
        static_plot(base_country()) %>%
        interactive_plot(base_country(),
                         comparison_countries())
    })

    # Accountability
    output$Account <- renderPlotly({
      data() %>%
        filter(variable %in% vars_transp)  %>%
        static_plot(base_country()) %>%
        interactive_plot(base_country(),
                         comparison_countries())
    })

   # Map -----------------------------------------------------------------------------------

    output$map_plot <- renderLeaflet({
      leaflet(data=wb_country_geom) %>%
        addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 1), group = "CartoDB.Positron") %>%
        #addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(opacity = 1), group = "Esri.WorldImagery") %>%
        setView(24.894344, 35.196849, zoom = 2) %>%
        #addLayersControl(baseGroups = c("CartoDB.Positron","Esri.WorldImagery"),
        #                 position = "topleft",
        #                 options = layersControlOptions(collapsed = T)) %>%
        addPolygons(
          fillColor = "white",
          weight = 0.2,
          opacity = 1,
          color = "black",
          dashArray = "1",
          fillOpacity = 0.25,
          #group = "Selected",
          #layerId = as.character(wb_country_geom$ISO_A3),
          highlight = highlightOptions(weight = 2.5, color = "#0066ff", dashArray = "", fillOpacity = 0.25, bringToFront = T),
          label = paste0(
            "<b>", wb_country_geom$WB_NAME,
            "</b><br/>Labor Average CTF: ",formatC(wb_country_geom$vars_lab, digits = 3, format = "f"),
            "</b><br/>Financial Average CTF: ",formatC(wb_country_geom$vars_fin, digits = 3, format = "f"),
            "</b><br/>Legal Average CTF: ",formatC(wb_country_geom$vars_leg, digits = 3, format = "f"),
            "</b><br/>Political Average CTF: ",formatC(wb_country_geom$vars_pol, digits = 3, format = "f"),
            "</b><br/>Social Average CTF: ",formatC(wb_country_geom$vars_social, digits = 3, format = "f"),
            "</b><br/>Business and Trade Average CTF: ",formatC(wb_country_geom$vars_mkt, digits = 3, format = "f"),
            "</b><br/>Public Sector Average CTF: ",formatC(wb_country_geom$vars_publ, digits = 3, format = "f"),
            "</b><br/>Governance of SOEs Average CTF: ",formatC(wb_country_geom$vars_service_del, digits = 3, format = "f"),
            "</b><br/>Accountability Average CTF: ",formatC(wb_country_geom$vars_transp, digits = 3, format = "f")
          ) %>%
            lapply(htmltools::HTML),
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "10px", direction = "auto")
        )
    })

    observe({

      vars_map <- input$vars_map

      if(vars_map!=""){

        var_selected <-
          variable_names %>%
          filter(var_name == sym(vars_map)) %>%
          .$variable

        leafletProxy("map_plot",
                     data = wb_country_geom %>%
                       mutate(
                         classification = case_when(
                           wb_country_geom[[var_selected]] <= 0.2 ~ "0.0 - 0.2",
                           wb_country_geom[[var_selected]] > 0.2 & wb_country_geom[[var_selected]] <= 0.4 ~ "0.2 - 0.4",
                           wb_country_geom[[var_selected]] > 0.4 & wb_country_geom[[var_selected]] <= 0.6 ~ "0.4 - 0.6",
                           wb_country_geom[[var_selected]] > 0.6 & wb_country_geom[[var_selected]] <= 0.8 ~ "0.6 - 0.8",
                           wb_country_geom[[var_selected]] > 0.8 ~ "0.8 - 1.0"
                         )
                       )
                    ) %>%
          clearShapes() %>%
          addPolygons(fillColor = ~pal(classification),
                      weight = 0.2,
                      opacity = 1,
                      color = "black",
                      dashArray = "1",
                      fillOpacity = 0.75,
                      #group = "Selected",
                      #layerId = as.character(wb_country_geom$ISO_A3),
                      highlight = highlightOptions(weight = 2.5, color = "#0066ff", dashArray = "", fillOpacity = 0.25, bringToFront = T),
                      label = paste0(
                        "<b>", wb_country_geom$WB_NAME,
                        "<br><br>",vars_map, " CTF: ",formatC(wb_country_geom[[var_selected]], digits = 3, format = "f"),
                        "</b><br><br/>Labor Average CTF: ",formatC(wb_country_geom$vars_lab, digits = 3, format = "f"),
                        "</b><br/>Financial Average CTF: ",formatC(wb_country_geom$vars_fin, digits = 3, format = "f"),
                        "</b><br/>Legal Average CTF: ",formatC(wb_country_geom$vars_leg, digits = 3, format = "f"),
                        "</b><br/>Political Average CTF: ",formatC(wb_country_geom$vars_pol, digits = 3, format = "f"),
                        "</b><br/>Social Average CTF: ",formatC(wb_country_geom$vars_social, digits = 3, format = "f"),
                        "</b><br/>Business and Trade Average CTF: ",formatC(wb_country_geom$vars_mkt, digits = 3, format = "f"),
                        "</b><br/>Public Sector Average CTF: ",formatC(wb_country_geom$vars_publ, digits = 3, format = "f"),
                        "</b><br/>Governance of SOEs Average CTF: ",formatC(wb_country_geom$vars_service_del, digits = 3, format = "f"),
                        "</b><br/>Accountability Average CTF: ",formatC(wb_country_geom$vars_transp, digits = 3, format = "f")
                      ) %>%
                        lapply(htmltools::HTML),
                      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "10px", direction = "auto")
          ) %>%
          addLegend(pal = pal,
                    values = ~classification,
                    opacity = 0.7,
                    title = paste(vars_map," CTF"),
                    position = "topright",
                    labFormat = labelFormat(digits = 3),
                    na.label = "Not available",
                    layerId = "legenda")

      }


    })

   # Data table ---------------------------------------------------------------
    output$dataset <-
      renderDataTable(server = FALSE, {

        vars <-
          input$vars %>%
          map(get) %>%
          unlist

        datatable(global_data %>%
                    select(country_name,
                           all_of(vars)) %>%
                    mutate(across(where(is.numeric), round, 3)) %>%
                    data.table::setnames(.,
                                         as.character(variable_names$variable),
                                         as.character(variable_names$var_name),
                                         skip_absent = TRUE),
                  rownames = FALSE,
                  extensions = 'Buttons',
                  filter = 'top',
                  options = list(scrollX = TRUE,
                                 pageLength = 13,
                                 fixedColumns = TRUE,
                                 autoWidth = TRUE,
                                 dom = "lBtipr",
                                 buttons = c('copy', 'csv', 'excel')))
      })


   # Definitions ------------------------------------------------------------

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

  }


