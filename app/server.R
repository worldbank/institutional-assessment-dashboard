
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

# Server ---------------------------------------------------------------------------------------------

  server <- function(input, output, session) {

    selected_tab <- reactive({
      selected_tab <- input$tab
      selected_tab
    })

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

      toggleState(id = "select",
                  condition = length(selected) >= 10)

      req(input$select)

      if(selected_tab()=="overview"){

        vars_tab <- family_names

        plot_family <-
          ggplotly(
            ggplot(
              data =
                family_level_data %>%
                def_quantiles(
                  selected_country,
                  selected,
                  vars_tab
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
                    text = map(paste('<b>Country:</b>', country_name, '<br>',
                                     '<b>Closeness to frontier:</b>', round(dtf, digits = 3), '<br>',
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
              ylab("Closeness to frontier") +
              xlab(""),
            tooltip = "text") %>%
            layout(
              margin = list(b = -1.5),
              annotations =
                   list(x = 0, y = -0.25,
                        text = map(paste0("Note: ",selected_country,", ",selected_groups,".",
                                         "<br>Closeness to frontier is calculated as (worst-y)/(worst-frontier).",
                                         "<br>1 identifies the best performer and 0 the worst performer",
                                         "<br>Weak = bottom 25%; Emerging = 25%-50%; Advanced = top 50%."), HTML),
                        showarrow = F, xref='paper', yref='paper',
                        align='left',
                        font=list(size=9))
            ) %>%
            config(modeBarButtonsToRemove = c("zoomIn2d",
                                            "zoomOut2d",
                                            "pan2d",
                                            "autoScale2d",
                                            "lasso2d",
                                            "select2d",
                                            "toggleSpikelines",
                                            "hoverClosest3d",
                                            "hoverCompareCartesian"))

        # OVERVIEW PLOT ----
        output$overview <- renderPlotly({
          plot_family
        })

      }

      vars_tab <- NULL

      if(selected_tab()=="labor"){vars_tab <- vars_lab}
      if(selected_tab()=="financial"){vars_tab <- vars_fin}
      if(selected_tab()=="legal"){vars_tab <- vars_leg}
      if(selected_tab()=="political"){vars_tab <- vars_pol}
      if(selected_tab()=="social"){vars_tab <- vars_social}
      if(selected_tab()=="trade"){vars_tab <- vars_mkt}
      if(selected_tab()=="public"){vars_tab <- vars_publ}
      if(selected_tab()=="governance"){vars_tab <- vars_lab}
      if(selected_tab()=="account"){vars_tab <- vars_transp}

      if(!is.null(vars_tab)){

        plot_global <-
          ggplotly(
            ggplot(
              data =
                global_data %>%
                def_quantiles(
                  selected_country,
                  selected,
                  vars_tab
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
                    text = map(paste('<b>Country:</b>', country_name, '<br>',
                                     '<b>Closeness to frontier:</b>', round(dtf, digits = 3), '<br>',
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
              ylab("Closeness to frontier") +
              xlab(""),
            tooltip = "text") %>%
          layout(
            margin = list(b = -1.5),
            annotations =
              list(x = 0, y = -0.25,
                   text = map(paste0("Note: ",selected_country,", ",selected_groups,".",
                                     "<br>Closeness to frontier is calculated as (worst-y)/(worst-frontier).",
                                     "<br>1 identifies the best performer and 0 the worst performer",
                                     "<br>Weak = bottom 25%; Emerging = 25%-50%; Advanced = top 50%."), HTML),
                   showarrow = F, xref='paper', yref='paper',
                   align='left',
                   font=list(size=9))
          ) %>%
          config(modeBarButtonsToRemove = c("zoomIn2d",
                                            "zoomOut2d",
                                            "pan2d",
                                            "autoScale2d",
                                            "lasso2d",
                                            "select2d",
                                            "toggleSpikelines",
                                            "hoverClosest3d",
                                            "hoverCompareCartesian"))

        # LABOR PLOT ----
        output$Labor <- renderPlotly({
          plot_global
        })

        # FINANCIAL PLOT ----
        output$Financial <- renderPlotly({
          plot_global
        })

        # LEGAL PLOT ----
        output$Legal <- renderPlotly({
          plot_global
        })

        # POLITICAL PLOT ----
        output$Political <- renderPlotly({
          plot_global
        })

        # SOCIAL PLOT ----
        output$Social <- renderPlotly({
          plot_global
        })

        # BUSINESS AND TRADE PLOT ----
        output$Trade <- renderPlotly({
          plot_global
        })

        # PUBLIC SECTOR PLOT ----
        output$Public <- renderPlotly({
          plot_global
        })

        # GOVERNANCE OF SOEs PLOT ----
        output$Governance <- renderPlotly({
          plot_global
        })

        # ACCOUNTABILITY PLOT ----
        output$Account <- renderPlotly({
          plot_global
        })

      }


    }) # Close observer

    # MAP ----
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

        datatable(data_table %>%
                    select(country_name,
                           all_of(vars)) %>%
                    data.table::setnames(., as.character(variable_names$variable), as.character(variable_names$var_name), skip_absent = TRUE),
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


