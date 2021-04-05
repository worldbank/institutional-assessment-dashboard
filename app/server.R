
# Load packages --------------------------------------------------------------------------------

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


# Load data sets --------------------------------------------------------------------------------

  source(file.path("auxiliary",
                   "vars-by-family.R"))

  # Function that defines quantiles based on country, comparison and variables
  source(file.path("auxiliary",
                   "fun_quantiles.R"))

  definitions <-
    read_csv(file.path("data",
                       "indicator_definitions.csv"))

  country_groups <-
    read_rds(file.path("data",
                       "wb_country_groups.rds"))

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

  data_table <-
    global_data %>%
    ungroup() %>%
    select(-c("lac", "lac6", "structural", "oecd","country_code")) %>%
    mutate(across(where(is.numeric), round, 3))

  wb_country_geom <-
    read_rds(file.path("data",
                       "wb_country_geom.rds"))

# Server ---------------------------------------------------------------------------------------------

  server <- function(input, output, session) {

    observe({

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

      # Can also set the label and select items
      updateCheckboxGroupInput(session,
                               "countries",
                               label = NULL,
                               choices = global_data$country_name %>% unique,
                               selected = selected
      )

      req(input$select)

      # OVERVIEW PLOT ----
      output$overview <- renderPlotly({
        plot <-
          ggplot(
            data =
              family_level_data %>%
                def_quantiles(
                  selected_country,
                  selected,
                  family_names
                ) %>%
                left_join(
                  variable_names %>%
                    select(family_var,family_name) %>%
                    unique,
                  by=c("variable"="family_var")
                )
          ) +
          geom_segment(
            aes(x = reorder(family_name,-dtf),
                xend = reorder(family_name,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(family_name,-dtf),
                y = dtf,
                text = map(paste(' <b>Country:</b>', country_name, '<br>',
                                 '<b>Distance to frontier:</b>', round(dtf, digits = 3), '<br>',
                                 '<b>Classification:</b>', classification), HTML),
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(
            values =
              c("Advanced"="#009E73",
                "Emerging"="#E69F00",
                "Weak"="#D55E00"
              )) +
          scale_y_continuous(
            limits = c(0,1)#,
            #breaks = seq(0,1,0.2))
          )  +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")
        ggplotly(plot,
                 tooltip = "text")

      })

      # LABOR PLOT ----
      output$Labor <- renderPlotly({
        plot <-
          ggplot(
            data =
              global_data %>%
                def_quantiles(
                  selected_country,
                  selected,
                  vars_lab
                ) %>%
                left_join(
                  variable_names %>%
                    select(variable,var_name) %>%
                    unique,
                  by="variable"
                )
          ) +
          geom_segment(
            aes(x = reorder(var_name,-dtf),
                xend = reorder(var_name,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(var_name,-dtf),
                y = dtf,
                text = map(paste(' <b>Country:</b>', country_name, '<br>',
                                 '<b>Distance to frontier:</b>', round(dtf, digits = 3), '<br>',
                                 '<b>Classification:</b>', classification), HTML),
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(
            values =
              c("Advanced"="#009E73",
                "Emerging"="#E69F00",
                "Weak"="#D55E00"
              )) +
          scale_y_continuous(
            limits = c(0,1)#,
            #breaks = seq(0,1,0.2))
          )  +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")

        ggplotly(plot,
                 tooltip = "text")

      })

      # FINANCIAL PLOT ----
      output$Financial <- renderPlotly({
        plot <-
          ggplot(
            data =
              global_data %>%
              def_quantiles(
                selected_country,
                selected,
                vars_fin
              ) %>%
              left_join(
                variable_names %>%
                  select(variable,var_name) %>%
                  unique,
                by="variable"
              )
          ) +
          geom_segment(
            aes(x = reorder(var_name,-dtf),
                xend = reorder(var_name,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(var_name,-dtf),
                y = dtf,
                text = map(paste(' <b>Country:</b>', country_name, '<br>',
                                 '<b>Distance to frontier:</b>', round(dtf, digits = 3), '<br>',
                                 '<b>Classification:</b>', classification), HTML),
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(
            values =
              c("Advanced"="#009E73",
                "Emerging"="#E69F00",
                "Weak"="#D55E00"
              )) +
          scale_y_continuous(
            limits = c(0,1)#,
            #breaks = seq(0,1,0.2))
          )  +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")

        ggplotly(plot,
                 tooltip = "text")

      })

      # LEGAL PLOT ----
      output$Legal <- renderPlotly({
        plot <-
          ggplot(
            data =
              global_data %>%
              def_quantiles(
                selected_country,
                selected,
                vars_leg
              ) %>%
              left_join(
                variable_names %>%
                  select(variable,var_name) %>%
                  unique,
                by="variable"
              )
          ) +
          geom_segment(
            aes(x = reorder(var_name,-dtf),
                xend = reorder(var_name,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(var_name,-dtf),
                y = dtf,
                text = map(paste(' <b>Country:</b>', country_name, '<br>',
                                 '<b>Distance to frontier:</b>', round(dtf, digits = 3), '<br>',
                                 '<b>Classification:</b>', classification), HTML),
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(
            values =
              c("Advanced"="#009E73",
                "Emerging"="#E69F00",
                "Weak"="#D55E00"
              )) +
          scale_y_continuous(
            limits = c(0,1)#,
            #breaks = seq(0,1,0.2))
          )  +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")

        ggplotly(plot,
                 tooltip = "text")


      })

      # POLITICAL PLOT ----
      output$Political <- renderPlotly({
        plot <-
          ggplot(
            data =
              global_data %>%
              def_quantiles(
                selected_country,
                selected,
                vars_pol
              ) %>%
              left_join(
                variable_names %>%
                  select(variable,var_name) %>%
                  unique,
                by="variable"
              )
          ) +
          geom_segment(
            aes(x = reorder(var_name,-dtf),
                xend = reorder(var_name,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(var_name,-dtf),
                y = dtf,
                text = map(paste(' <b>Country:</b>', country_name, '<br>',
                                 '<b>Distance to frontier:</b>', round(dtf, digits = 3), '<br>',
                                 '<b>Classification:</b>', classification), HTML),
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(
            values =
              c("Advanced"="#009E73",
                "Emerging"="#E69F00",
                "Weak"="#D55E00"
              )) +
          scale_y_continuous(
            limits = c(0,1)#,
            #breaks = seq(0,1,0.2))
          )  +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")

        ggplotly(plot,
                 tooltip = "text")


      })

      # SOCIAL PLOT ----
      output$Social <- renderPlotly({
        plot <-
          ggplot(
            data =
              global_data %>%
              def_quantiles(
                selected_country,
                selected,
                vars_social
              ) %>%
              left_join(
                variable_names %>%
                  select(variable,var_name) %>%
                  unique,
                by="variable"
              )
          ) +
          geom_segment(
            aes(x = reorder(var_name,-dtf),
                xend = reorder(var_name,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(var_name,-dtf),
                y = dtf,
                text = map(paste(' <b>Country:</b>', country_name, '<br>',
                                 '<b>Distance to frontier:</b>', round(dtf, digits = 3), '<br>',
                                 '<b>Classification:</b>', classification), HTML),
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(
            values =
              c("Advanced"="#009E73",
                "Emerging"="#E69F00",
                "Weak"="#D55E00"
              )) +
          scale_y_continuous(
            limits = c(0,1)#,
            #breaks = seq(0,1,0.2))
          )  +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")

        ggplotly(plot,
                 tooltip = "text")


      })

      # BUSINESS AND TRADE PLOT ----
      output$Trade <- renderPlotly({
        plot <-
          ggplot(
            data =
              global_data %>%
              def_quantiles(
                selected_country,
                selected,
                vars_mkt
              ) %>%
              left_join(
                variable_names %>%
                  select(variable,var_name) %>%
                  unique,
                by="variable"
              )
          ) +
          geom_segment(
            aes(x = reorder(var_name,-dtf),
                xend = reorder(var_name,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(var_name,-dtf),
                y = dtf,
                text = map(paste(' <b>Country:</b>', country_name, '<br>',
                                 '<b>Distance to frontier:</b>', round(dtf, digits = 3), '<br>',
                                 '<b>Classification:</b>', classification), HTML),
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(
            values =
              c("Advanced"="#009E73",
                "Emerging"="#E69F00",
                "Weak"="#D55E00"
              )) +
          scale_y_continuous(
            limits = c(0,1)#,
            #breaks = seq(0,1,0.2))
          )  +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")

        ggplotly(plot,
                 tooltip = "text")


      })

      # PUBLIC SECTOR PLOT ----
      output$Public <- renderPlotly({
        plot <-
          ggplot(
            data =
              global_data %>%
              def_quantiles(
                selected_country,
                selected,
                vars_publ
              ) %>%
              left_join(
                variable_names %>%
                  select(variable,var_name) %>%
                  unique,
                by="variable"
              )
          ) +
          geom_segment(
            aes(x = reorder(var_name,-dtf),
                xend = reorder(var_name,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(var_name,-dtf),
                y = dtf,
                text = map(paste(' <b>Country:</b>', country_name, '<br>',
                                 '<b>Distance to frontier:</b>', round(dtf, digits = 3), '<br>',
                                 '<b>Classification:</b>', classification), HTML),
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(
            values =
              c("Advanced"="#009E73",
                "Emerging"="#E69F00",
                "Weak"="#D55E00"
              )) +
          scale_y_continuous(
            limits = c(0,1)#,
            #breaks = seq(0,1,0.2))
          )  +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")

        ggplotly(plot,
                 tooltip = "text")

      })

      # GOVERNANCE OF SOEs
      output$Governance <- renderPlotly({
        plot <-
          ggplot(
            data =
              global_data %>%
              def_quantiles(
                selected_country,
                selected,
                vars_service_del
              ) %>%
              left_join(
                variable_names %>%
                  select(variable,var_name) %>%
                  unique,
                by="variable"
              )
          ) +
          geom_segment(
            aes(x = reorder(var_name,-dtf),
                xend = reorder(var_name,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(var_name,-dtf),
                y = dtf,
                text = map(paste(' <b>Country:</b>', country_name, '<br>',
                                 '<b>Distance to frontier:</b>', round(dtf, digits = 3), '<br>',
                                 '<b>Classification:</b>', classification), HTML),
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(
            values =
              c("Advanced"="#009E73",
                "Emerging"="#E69F00",
                "Weak"="#D55E00"
              )) +
          scale_y_continuous(
            limits = c(0,1)#,
            #breaks = seq(0,1,0.2))
          )  +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")

        ggplotly(plot,
                 tooltip = "text")


      })

      # ACCOUNTABILITY PLOT
      output$Account <- renderPlotly({
        plot <-
          ggplot(
            data =
              global_data %>%
              def_quantiles(
                selected_country,
                selected,
                vars_transp
              ) %>%
              left_join(
                variable_names %>%
                  select(variable,var_name) %>%
                  unique,
                by="variable"
              )
          ) +
          geom_segment(
            aes(x = reorder(var_name,-dtf),
                xend = reorder(var_name,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(var_name,-dtf),
                y = dtf,
                text = map(paste(' <b>Country:</b>', country_name, '<br>',
                                 '<b>Distance to frontier:</b>', round(dtf, digits = 3), '<br>',
                                 '<b>Classification:</b>', classification), HTML),
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(
            values =
              c("Advanced"="#009E73",
                "Emerging"="#E69F00",
                "Weak"="#D55E00"
              )) +
          scale_y_continuous(
            limits = c(0,1)#,
            #breaks = seq(0,1,0.2))
          )  +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")

        ggplotly(plot,
                 tooltip = "text")

      })




    }) # Close observer

    # MAP ----

    observe({

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
              "</b><br/>Labor Average DTF: ",formatC(wb_country_geom$vars_lab, digits = 3, format = "f"),
              "</b><br/>Financial Average DTF: ",formatC(wb_country_geom$vars_fin, digits = 3, format = "f"),
              "</b><br/>Legal Average DTF: ",formatC(wb_country_geom$vars_leg, digits = 3, format = "f"),
              "</b><br/>Political Average DTF: ",formatC(wb_country_geom$vars_pol, digits = 3, format = "f"),
              "</b><br/>Social Average DTF: ",formatC(wb_country_geom$vars_social, digits = 3, format = "f"),
              "</b><br/>Business and Trade Average DTF: ",formatC(wb_country_geom$vars_mkt, digits = 3, format = "f"),
              "</b><br/>Public Sector Average DTF: ",formatC(wb_country_geom$vars_publ, digits = 3, format = "f"),
              "</b><br/>Governance of SOEs Average DTF: ",formatC(wb_country_geom$vars_service_del, digits = 3, format = "f"),
              "</b><br/>Accountability Average DTF: ",formatC(wb_country_geom$vars_transp, digits = 3, format = "f")
            ) %>%
              lapply(htmltools::HTML),
            labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "10px", direction = "auto")
          )

      })

      vars_map <- input$vars_map

      if(vars_map!=""){

        var_selected <-
          variable_names %>%
          filter(var_name == sym(vars_map)) %>%
          .$variable

        data_selected <- global_data %>%
          ungroup() %>%
          select(country_code,(sym(var_selected)))

        bins <- getJenksBreaks(data_selected[[var_selected]], k = 6)
        pal <- colorBin(plasma(n=6, alpha=0.75, begin=0, end=1, direction = 1), domain = data_selected[[var_selected]], bins = bins)

        wb_country_geom_selected <-
          wb_country_geom %>%
            left_join(
              data_selected,
              by=c("WB_A3"="country_code")
            )

        leafletProxy("map_plot",
                     data=wb_country_geom_selected) %>%
          clearShapes() %>%
          addPolygons(fillColor = ~pal(wb_country_geom_selected[[var_selected]]),
                      weight = 0.2,
                      opacity = 1,
                      color = "black",
                      dashArray = "1",
                      fillOpacity = 0.75,
                      #group = "Selected",
                      #layerId = as.character(wb_country_geom$ISO_A3),
                      highlight = highlightOptions(weight = 2.5, color = "#0066ff", dashArray = "", fillOpacity = 0.25, bringToFront = T),
                      label = paste0(
                        "<b>", wb_country_geom_selected$WB_NAME,
                        "</b><br/>Labor Average DTF: ",formatC(wb_country_geom_selected$vars_lab, digits = 3, format = "f"),
                        "</b><br/>Financial Average DTF: ",formatC(wb_country_geom_selected$vars_fin, digits = 3, format = "f"),
                        "</b><br/>Legal Average DTF: ",formatC(wb_country_geom_selected$vars_leg, digits = 3, format = "f"),
                        "</b><br/>Political Average DTF: ",formatC(wb_country_geom_selected$vars_pol, digits = 3, format = "f"),
                        "</b><br/>Social Average DTF: ",formatC(wb_country_geom_selected$vars_social, digits = 3, format = "f"),
                        "</b><br/>Business and Trade Average DTF: ",formatC(wb_country_geom_selected$vars_mkt, digits = 3, format = "f"),
                        "</b><br/>Public Sector Average DTF: ",formatC(wb_country_geom_selected$vars_publ, digits = 3, format = "f"),
                        "</b><br/>Governance of SOEs Average DTF: ",formatC(wb_country_geom_selected$vars_service_del, digits = 3, format = "f"),
                        "</b><br/>Accountability Average DTF: ",formatC(wb_country_geom_selected$vars_transp, digits = 3, format = "f")
                      ) %>%
                        lapply(htmltools::HTML),
                      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "10px", direction = "auto")
          ) %>%
          addLegend(pal = pal,
                    values = ~wb_country_geom_selected[[var_selected]],
                    opacity = 0.7,
                    title = paste(vars_map," DTF"),
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

        datatable(data_table %>%
                    select(country_name,
                           all_of(vars)),
                  rownames = FALSE,
                  extensions = 'Buttons',
                  filter = 'top',
                  options = list(scrollX = TRUE,
                                 pageLength = 15,
                                 fixedColumns = TRUE,
                                 autoWidth = TRUE,
                                 dom = "lBtipr",
                                 buttons = c('copy', 'csv', 'excel')))
      })


   # Definitions ------------------------------------------------------------

    families <- definitions$family

    definitions <-
      definitions %>%
      select(-c(family, var_name)) %>%
      split(families)

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


