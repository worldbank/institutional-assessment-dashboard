
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
  library(grDevices)


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

  # Raw data
  raw_data <-
    read_rds(file.path("data",
                       "raw_data.rds")) %>%
    filter(year >= 1990) %>%
    select(-c(lac, lac6, oecd, structural)) %>%
    rename(Year = year)

  raw_data <-
    raw_data[rowSums(!is.na(raw_data)) > 3, ]

  # Metadata
  variable_names <-
    read_rds(file.path("data",
                       "variable_names.rds"))

  country_list <-
    read_rds(file.path("data",
                       "wb_country_list.rds"))

  color_groups <- colorRampPalette(c("#053E5D", "#60C2F7"))


# Server ################################################################################

  server <- function(input, output, session) {


   # Handle inputs ======================================================================

    # Base country
    base_country <-
      eventReactive(
        input$select,
        input$country
      )

    observeEvent(input$show_def, {print(input$show_Def)})

    # Comparison countries
    observeEvent(input$groups,
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

                      updatePickerInput(session,
                                        "countries",
                                        label = NULL,
                                        choices = global_data$country_name %>% unique,
                                        selected = selected
                      )
                    },

                    ignoreNULL = FALSE
      )


    # Triggered by comparison countries: names of countries selected and action button
    observeEvent(input$countries,

                 {
                   # Can also set the label and select items


                   toggleState(id = "select",
                               condition = length(input$countries) >= 10)
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
                          input$countries,
                          vars_all
                        ) %>%
                        left_join(variable_names)

                    }
      )

    # Browse data
    browse_data <-
      eventReactive(input$data,

                    {
                      selected_data <- input$data

                      if (selected_data=="Closeness to frontier") {
                        return(global_data)
                      }

                      if (selected_data=="Compiled indicators") {
                        return(raw_data)
                      }

                    }
      )

   # Plots =============================================================================

    # Overview
    output$plot <-
      renderPlotly({

        if (input$family == "Overview") {

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
              input$countries,
              variable_names$variable
            )  %>%
            left_join(variable_names)

          data %>%
            static_plot(base_country(),
                        input$family) %>%
            interactive_plot(base_country(),
                             input$groups,
                             input$family)
        } else {

          vars <-
            if (input$family == "Labor market institutions") {vars_lab} else
              if (input$family == "Financial market institutions") {vars_fin} else
                if (input$family == "Legal institutions") {vars_leg} else
                  if (input$family == "Political institutions") {vars_pol} else
                    if (input$family == "Social institutions") {vars_social} else
                      if (input$family == "Business environment and trade institutions") {vars_mkt} else
                        if (input$family == "Public sector performance institutions") {vars_publ} else
                          if (input$family == "Institutions for service delivery") {vars_service_del} else
                            if (input$family == "Accountability institutions") {vars_transp}


          data() %>%
            filter(variable %in% vars)  %>%
            static_plot(base_country(),
                        input$family) %>%
            interactive_plot(base_country(),
                             input$groups,
                             input$family)
        }
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
            labs(title = paste0("<b>",input$vars_map,"</b>")) +
            theme_bw()

          interactive_map(map, input$vars_map)
        }

      })

   # Trends =====================================================================================

    var_trends <-
      eventReactive(input$indicator_trends,
                    {
                      var_selected <-
                        variable_names %>%
                        filter(var_name == input$indicator_trends) %>%
                        .$variable
                    }
      )

    data_trends <-
      reactive({

        data <-
          raw_data %>%
          filter(country_name %in% c(input$country_trends)) %>%
          mutate(alpha = .8,
                 shape = 19)

        if (!is.null(input$countries_trends)) {

          data <-
            raw_data %>%
            filter(country_name %in% c(input$countries_trends)) %>%
            mutate(alpha = .5,
                   shape = 18) %>%
            bind_rows(data)
        }

        if (!is.null(input$group_trends)) {

          indicator <-
            raw_data %>%
            select(country_name, Year, all_of(var_trends()))

          data <-
            country_list %>%
            filter(group %in% input$group_trends) %>%
            select(group, country_name) %>%
            mutate(country_name = as.character(country_name)) %>%
            left_join(indicator) %>%
            group_by(Year, group) %>%
            summarise_all(~ mean(., na.rm = TRUE)) %>%
            mutate(country_name = as.character(group),
                   alpha = .5,
                   shape = 19) %>%
            bind_rows(data)
        }

        data %>%
          rename(Country = country_name) %>%
          select(Country, Year, all_of(var_trends()), alpha) %>%
          mutate_at(vars(all_of(var_trends())),
                    ~ round(., 3))
      })

    output$time_series <-
      renderPlotly({

        if (input$indicator_trends != "") {
          static_plot <-
            ggplot(data_trends(),
                   aes_string(x = "Year",
                              y = var_trends(),
                              color = "Country",
                              alpha = "alpha")) +
            geom_point(aes(text = paste("Country:", Country, "<br>",
                                        "Year:", Year, "<br>",
                                        "Value:", get(var_trends()))),
                       size = 3) +
            geom_line() +
            labs(title = paste0("<b>",input$indicator_trends,"</b>")) +
            theme_ipsum() +
            labs(
              x = "Year",
              y = "Indicator value"
            ) +
            scale_color_manual(values = c("#FB8500",
                                          gray.colors(length(input$countries_trends)),
                                          color_groups(length(input$group_trends))),
                               breaks = c(input$country_trends,
                                          input$countries_trends,
                                          input$group_trends)
            ) +
            scale_alpha_identity()

          ggplotly(static_plot, tooltip = "text") %>%
            layout(
              margin = list(l=50, r=50, t=75, b=135)
            ) %>%
            config(
              modeBarButtonsToRemove = c("zoomIn2d",
                                         "zoomOut2d",
                                         "pan2d",
                                         "autoScale2d",
                                         "lasso2d",
                                         "select2d",
                                         "toggleSpikelines",
                                         "hoverClosest3d",
                                         "hoverClosestCartesian",
                                         "hoverCompareCartesian"),
              toImageButtonOptions= list(filename = paste0("trends_",
                                                           tolower(input$country_trends),"_",
                                                           tolower(stringr::str_replace_all(input$indicator_trends,"\\s","_"))))
            )
        }

      })

   # Data table =================================================================================

    output$benchmark_datatable <-
      renderDataTable(server = FALSE, {

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
                        file)
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
      filename = "instutitional-assessment-report.docx",
      content = function(file) {

        file.copy("report.Rmd", "tempReport.Rmd", overwrite = TRUE)

        params <- list(base_country = base_country(),
                       comparison_countries = input$countries,
                       data = data())

        rmarkdown::render("tempReport.Rmd",
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

   # Definitions ===========================================================================

    output$definition <-
      renderTable(definitions[[input$family]])

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

